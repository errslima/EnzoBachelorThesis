library(torch)

ann <- function(x, y, neurons = 16, iterations = 1000, learning_rate = 1e-1, mode = "reg", model = NULL, ndropout = 0.1, device = "cpu"){
  
  x <- as.matrix(x)
  y <- as.matrix(as.numeric(y))
  
  d_in <- ncol(x)
  d_out <- ncol(y)
  d_hidden <- neurons
  
  x <- torch_tensor(x)
  y <- torch_tensor(y)
  
  # mean values of the neurons in the first and second layers
  if(is.null(model)){
    w1 <- torch_randn(d_in, d_hidden, requires_grad = TRUE, device = device)
    b1 <- torch_randn(1, d_hidden, requires_grad = TRUE, device = device)
    w2 <- torch_randn(d_hidden, d_hidden, requires_grad = TRUE, device = device)
    b2 <- torch_randn(1, d_hidden, requires_grad = TRUE, device = device)
    w3 <- torch_randn(d_hidden, d_out, requires_grad = TRUE, device = device)
  } else {
    w1 <- torch_tensor(model[[1]], requires_grad = TRUE, device = device)
    w2 <- torch_tensor(model[[2]], requires_grad = TRUE, device = device)
    w3 <- torch_tensor(model[[3]], requires_grad = TRUE, device = device)
    b1 <- torch_tensor(model[[4]], requires_grad = TRUE, device = device)
    b2 <- torch_tensor(model[[5]], requires_grad = TRUE, device = device)
  }
  
  timeStart <- Sys.time()
  loss_count <- c()
  loss_total <- c()
  
  for (t in 1:iterations) {
    
    # ### -------- Error --------
    # e_w1 <- torch_randn(d_in, d_hidden) / 100
    # e_w2 <- torch_randn(d_hidden, d_hidden) / 100
    # e_w3 <- torch_randn(d_hidden, d_out) / 100
    # 
    # w1a <- w1 + e_w1
    # w2a <- w2 + e_w2
    # w3a <- w3 + e_w3
    
    ### -------- Dropout --------
    dropout <- nn_dropout(p = ndropout)
    w1d <- dropout(w1)
    w2d <- dropout(w2)
    w3d <- dropout(w3)
    
    ### -------- Forward pass --------
    y_hat <- torch_sigmoid(torch_tanh(torch_tanh(x$mm(w1d)$add(b1))$mm(w2d)$add(b2))$mm(w3d))
    
    ### -------- compute loss --------
    if(mode == "reg"){
      bce_loss <- nn_mse_loss()
      loss <- bce_loss(y_hat, y)
    } else if(mode == "cat"){
      loss <- nnf_binary_cross_entropy(y_hat, y)
    }
    
    # error catching
    if(is.nan(loss$item())){
      break("loss is nan")
    }
    # loss_count <- c(loss_count, loss$item())
    # loss_total <- c(loss_total, loss$item())
    # if (t %% 100 == 0){
    #   timeEnd <- Sys.time()
    #   cat("Epoch: ", t, "   Loss: ", mean(loss_count), "    time: ", difftime(timeEnd, timeStart, units='mins'), "\n")
    #   cat("Total Loss: ", mean(loss_total), "\n")
    #   loss_count <- c()
    # }
    
    ### -------- Backpropagation --------
    loss$backward()
    
    ### -------- Update weights -------- 
    
    # Wrap in with_no_grad() because this is a part we DON'T 
    # want to record for automatic gradient computation
    with_no_grad({
      w1 <- w1$sub_(learning_rate * w1$grad)
      w2 <- w2$sub_(learning_rate * w2$grad)
      w3 <- w3$sub_(learning_rate * w3$grad)
      b1 <- b1$sub_(learning_rate * b1$grad)
      b2 <- b2$sub_(learning_rate * b2$grad)
      
      # Zero gradients after every pass
      w1$grad$zero_()
      b1$grad$zero_()
      b2$grad$zero_()
      w2$grad$zero_()
      w3$grad$zero_()
    })
    
  }
  
  return(list(matrix(as.numeric(w1), ncol = d_hidden),
              matrix(as.numeric(w2), ncol = d_hidden),
              matrix(as.numeric(w3), ncol = d_out),
              matrix(as.numeric(b1), ncol = d_hidden),
              matrix(as.numeric(b2), ncol = d_hidden)))
}


# END OF SCRIPT