library(torch)

neg_pred <- function(x, model){
  
  x <- as.matrix(x)
  x <- torch_tensor(x)
  
  d_in <- dim(model[[1]])[1]
  d_hidden <- dim(model[[1]])[2]
  d_out <- 1
  
  w1 <- torch_tensor(model[[1]])
  w2 <- torch_tensor(model[[2]])
  w3 <- torch_tensor(model[[3]])
  b1 <- torch_tensor(model[[4]])
  b2 <- torch_tensor(model[[5]])
    
  y_hat <- torch_sigmoid(torch_tanh(torch_tanh(x$mm(w1)$add(b1))$mm(w2)$add(b2))$mm(w3))
  
  return(as.numeric(y_hat))
}


# END OF SCRIPT