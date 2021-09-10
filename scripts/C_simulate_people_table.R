# Declare the function. All the NULL parameters are there for customization by the end-user.
simulate_people_table <- function(n,
                                  seed = as.integer(format(Sys.time(), "%s")),
                                  # template activates a pre-determined list of parameters hard-coded in the function.
                                  template = NULL,
                                  # init_generation_mean determines the mean used for the ages of the initial generation of people.
                                  init_generation_mean = NULL,
                                  # marriage_name sets a naming convention for spouses. currently 'none', 'male', 'female'.
                                  marriage_name = NULL,
                                  # marriage_amount is a multiplier between 0 - 1 to indicate what fraction of the population is in a relationship.
                                  marriage_amount = NULL,
                                  # child_name sets a naming convention for children. currently 'paternal', 'maternal', 'paternal_maternal' and 'maternal_paternal'.
                                  child_name = NULL,
                                  # child_mean determines the mean used for the amount of children in a family.
                                  child_mean = NULL,
                                  # relationship coefficient
                                  rel_coef = 0.25) {

  # the current year will be used at the end of the script to calculate birthyears for everyone in the people table.
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  # import a database of names with gender and ethnicity information from the United States Census bureau.
  load("./data/census_usa.rda")

  # import a database of country templates that specify family structures.
  country_templates <- utils::read.csv("./data/country_templates.csv", stringsAsFactors = F, encoding = "UTF-8")

  # load template settings
  if (template %in% country_templates$country) {
    init_generation_mean <- country_templates$init_generation_mean[which(country_templates$country == template)]
    marriage_name <- country_templates$marriage_name[which(country_templates$country == template)]
    marriage_amount <- country_templates$marriage_amount[which(country_templates$country == template)]
    child_name <- country_templates$child_name[which(country_templates$country == template)]
    child_mean <- country_templates$child_mean[which(country_templates$country == template)]
  } else {
    init_generation_mean <- 30
    marriage_name <- "male"
    marriage_amount <- 0.65
    child_name <- "paternal"
    child_mean <- 2
  }

  # generate the initial list of random unrelated people
  people <- data.frame(
    individ = id_maker(n, seed = seed),
    firstname = sample(1:nrow(firstnames), n, prob = firstnames$freq),
    lastname = sample(1:nrow(lastnames), n, prob = lastnames$freq),

    birthyear = rpois(n, init_generation_mean),
    gender = rep(NA, n),
    ethnicity = rep(NA, n),
    mother = id_maker(n, reserved = id_maker(n, seed = seed), seed = seed),
    father = id_maker(n, reserved = id_maker(2 * n, seed = seed), seed = seed),
    stringsAsFactors = F
  )

  people$gender <- firstnames$gender[people$firstname]
  people$firstname <- firstnames$firstname[people$firstname]

  people$ethnicity <- lastnames$ethnicity[people$lastname]
  people$lastname <- lastnames$lastname[people$lastname]

  # generate the initial list of marriages
  marriages <- data.frame(
    spouse.x = sample(people$individ[which(people$gender == "male")], round(n * marriage_amount / 2, 0)),
    spouse.y = sample(people$individ[which(people$gender == "female")], round(n * marriage_amount / 2, 0)),
    children = rep(0, round(n * marriage_amount / 2, 0)),
    children_prob = rep(0, round(n * marriage_amount / 2, 0)),
    stringsAsFactors = F
  )

  # generate a poisson distribution of number of children per family
  children_amount_distribution <- stats::rpois(max(round(n * marriage_amount / 2, 0), 100), child_mean)

  # generate a new column containing the probabilities that each family will produce another child
  for (i in 1:nrow(marriages)) {
    marriages$children_prob[i] <- length(which(children_amount_distribution > marriages$children[i])) / length(children_amount_distribution)
  }
  
  reserved_ids <- c(people$individ, people$mother, people$father)
  
  # initialize the single people vector for use in the main loop
  single_people <- c()
  
  m <- max(10, as.integer(n / 10))
  
  # generate the list of external singles
  external_people <- data.frame(
    individ = id_maker(m, seed = seed + 1, reserved = reserved_ids),
    firstname = sample(1:nrow(firstnames), m, prob = firstnames$freq),
    lastname = sample(1:nrow(lastnames), m, prob = lastnames$freq),
    
    birthyear = rpois(m, init_generation_mean) + 10,
    gender = rep(NA, m),
    ethnicity = rep(NA, m),
    mother = id_maker(m, reserved = c(reserved_ids, id_maker(m, seed = seed + 1)), seed = seed + 2),
    father = id_maker(m, reserved = c(reserved_ids, id_maker(2 * m, seed = seed + 1)), seed = seed + 2),
    stringsAsFactors = F
  )
  
  external_people$gender <- firstnames$gender[external_people$firstname]
  external_people$firstname <- firstnames$firstname[external_people$firstname]
  
  external_people$ethnicity <- lastnames$ethnicity[external_people$lastname]
  external_people$lastname <- lastnames$lastname[external_people$lastname]

  reserved_ids <- c(reserved_ids, external_people$individ, external_people$mother, external_people$father)
  
  # generate the actual people table
  for (i in 1:n) {
    individ <- id_maker(1, reserved = c(reserved_ids), seed = seed + i)

    reserved_ids <- c(reserved_ids, individ)
    # select a mother from the marriage table, based on probability that she and her spouse will have a new kid
    mother <- sample(marriages$spouse.y, 1, prob = marriages$children_prob)
    father <- marriages$spouse.x[which(marriages$spouse.y == mother)]

    # add one child to the child count of this couple
    marriages$children[which(marriages$spouse.x == father & marriages$spouse.y == mother)] <- marriages$children[which(marriages$spouse.x == father & marriages$spouse.y == mother)] + 1

    # define how the last name of the child is generated
    if (child_name == "paternal") {
      lastname <- people$lastname[which(people$individ == father)]
    } else if (child_name == "maternal") {
      lastname <- people$lastname[which(people$individ == mother)]
    } else if (child_name == "paternal_maternal") {
      # copy the first word of the father's surname and the first word of the mother's surname
      lastname <- paste(gsub("([A-Za-z]+).*", "\\1", people$lastname[which(people$individ == father)]), gsub("([A-Za-z]+).*", "\\1", people$lastname[which(people$individ == mother)]))
    } else if (child_name == "maternal_paternal") {
      # copy the first word of the mother's surname and the first word of the father's surname
      lastname <- paste(gsub("([A-Za-z]+).*", "\\1", people$lastname[which(people$individ == mother)]), gsub("([A-Za-z]+).*", "\\1", people$lastname[which(people$individ == father)]))
    }

    # determine the ethnicity of the father
    if(father %in% people$individ){
      father_ethn <- people$ethnicity[people$individ == father]
      father_ethn <- father_ethn
    } else if(father %in% external_people$individ){
      father_ethn <- external_people$ethnicity[external_people$individ == father]
      father_ethn <- father_ethn
    } else {
      father_ethn <- "mix"
    }

    # determine the ethnicity of the mother
    if(mother %in% people$individ){
      mother_ethn <- people$ethnicity[people$individ == mother]
      mother_ethn <- mother_ethn
    } else if(mother %in% external_people$individ){
      mother_ethn <- external_people$ethnicity[external_people$individ == mother]
      mother_ethn <- mother_ethn
    } else {
      mother_ethn <- "mix"
    }

    # set the ethnicity of the child
    if(mother_ethn == father_ethn){
      ethnicity <- mother_ethn
    } else {
      ethnicity <- "mix"
    }

    # generate a firstname and gender
    firstname_id <- sample(1:nrow(firstnames), 1, prob = firstnames$freq)
    firstname <- firstnames$firstname[firstname_id]
    gender <- firstnames$gender[firstname_id]

    # generate a birthyear that is at least 18 years later than either father or mother
    birthyear <- sample(rpois(100, init_generation_mean - 3), 1) + max(people$birthyear[which(people$individ %in% c(father, mother))])

    # this if-statement determines if this child will ever enter a relationship
    if (sample(c(TRUE, FALSE), 1, prob = c(marriage_amount, 1 - (marriage_amount)))) {
      # select all available single people from the opposite sex
      single_people <- c(single_people, individ, external_people$individ)
      singles <- people[which(people$individ %in% single_people & people$gender != gender & abs(people$birthyear - birthyear) < 20), ]
      singles_external <- external_people[which(external_people$gender != gender & abs(external_people$birthyear - birthyear) < 20), ]
      singles <- rbind(singles, singles_external)
      
      # remove everybody who is related - incest taboo
      if (nrow(singles) != 0){
        kinship <- rbind(people[, c("individ", "father", "mother")], data.frame(individ = individ, father = father, mother = mother, stringsAsFactors = F))
        for(m in 1:nrow(singles)){
          if(relatedness(individ, singles$individ[m], kinship) > rel_coef){
            singles <- singles[-m, ]
          }
        }
      }

      if (nrow(singles) != 0) {
        # select a partner. partners with a large age difference are less likely to be chosen.
        partner <- sample(singles$individ, 1, prob = (1 / (abs(birthyear - singles$birthyear)**2 + 0.01)))

        # generate a new entry for the marriage table
        if (gender == "male") {
          marriage_add <- data.frame(spouse.x = individ, spouse.y = partner, children = 0, children_prob = 0, stringsAsFactors = F)
        } else if (gender == "female") {
          marriage_add <- data.frame(spouse.x = partner, spouse.y = individ, children = 0, children_prob = 0, stringsAsFactors = F)
        }
        marriages <- rbind(marriages, marriage_add)

        # recalculate the probability of each family to have a new child
        for (j in 1:nrow(marriages)) {
          marriages$children_prob[j] <- length(which(children_amount_distribution > marriages$children[j])) / length(children_amount_distribution)
        }
        
        # refresh the single people list so that all married people are removed
        single_people <- single_people[single_people %!in% c(marriages$spouse.x, marriages$spouse.y)]
        external_people <- external_people[external_people$individ %!in% c(marriages$spouse.x, marriages$spouse.y),]
        
        # add a new external person to the external people table
        add_external <- data.frame(
          individ = id_maker(1, seed = seed + 1, reserved = reserved_ids),
          firstname = sample(1:nrow(firstnames), 1, prob = firstnames$freq),
          lastname = sample(1:nrow(lastnames), 1, prob = lastnames$freq),

          birthyear = birthyear + sample(5:10, 1),
          gender = rep(NA, 1),
          ethnicity = rep(NA, 1),
          mother = id_maker(1, reserved = reserved_ids, seed = seed + i + 1),
          father = id_maker(1, reserved = reserved_ids, seed = seed + i + 2),
          stringsAsFactors = F
        )
        reserved_ids <- c(reserved_ids, add_external$individ, add_external$mother, add_external$father)
        
        add_external$gender <- firstnames$gender[add_external$firstname]
        add_external$firstname <- firstnames$firstname[add_external$firstname]
        
        add_external$ethnicity <- lastnames$ethnicity[add_external$lastname]
        add_external$lastname <- lastnames$lastname[add_external$lastname]
        
        external_people <- rbind(external_people, add_external)
        
      }
    }

    # generate a new entry for the people table
    people_add <- data.frame(
      individ = individ,
      firstname = firstname,
      lastname = lastname,
      birthyear = birthyear,
      gender = gender,
      ethnicity = ethnicity,
      mother = mother,
      father = father,
      stringsAsFactors = F
    )
    
    reserved_ids <- c(reserved_ids, individ)

    people <- rbind(people, people_add)
  }

  # move all birthyears forward so that the most recent person is born this year
  people$birthyear <- people$birthyear + current_year - max(people$birthyear)
  
  # print the amount of people who have children per generation
  cat(paste0("fraction of parents per generation = ", round(nrow(people[people$individ %in% c(marriages$spouse.x, marriages$spouse.y), ]) / nrow(people), 2), "\n"))
  
  # create a kinship table
  kinship_table <- data.frame(individ = people$individ, individ_age = people$birthyear, mother = people$mother, father = people$father, stringsAsFactors = F)
  kinship_table$mother_age <- left_join(data.frame(individ = kinship_table$mother, stringsAsFactors = F), people[,c("individ", "birthyear")], by = "individ")[,"birthyear"]
  kinship_table$father_age <- left_join(data.frame(individ = kinship_table$father, stringsAsFactors = F), people[,c("individ", "birthyear")], by = "individ")[,"birthyear"]
  kinship_table$mother_agedif <- kinship_table$individ_age - kinship_table$mother_age
  kinship_table$father_agedif <- kinship_table$individ_age - kinship_table$father_age
  # print the mean age difference between generations
  cat(paste0("mean generation spread = ", round(mean(c(kinship_table$mother_agedif, kinship_table$father_agedif), na.rm = T), 2), "\n"))
  
  # add birthyear information to the marriage table
  spouse_age <- people[, c("individ", "birthyear")]
  colnames(spouse_age)[1] <- "spouse.x"
  marriages$birthyear.x <- left_join(marriages[, "spouse.x", drop = F], spouse_age, by = "spouse.x")[, 2]
  colnames(spouse_age)[1] <- "spouse.y"
  marriages$birthyear.y <- left_join(marriages[, "spouse.y", drop = F], spouse_age, by = "spouse.y")[, 2]
  
  # print the mean number of children per family
  cat(paste0("mean children per family = ", round(mean(marriages$children), 2), "\n"))
  # return the people table without the initial list of random people
  return(people[(n + 1):(2 * n), ])
}


# END OF SCRIPT