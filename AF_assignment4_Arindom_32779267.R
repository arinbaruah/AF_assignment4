
generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  # Generate errors
  error <- rnorm(n, mean = 0, sd = sigma)
  # Set up vector for the response with initial values set to 0
  
  if (any(length(phi) == 0) | any(is.na(phi)) | any(is.null(phi))) {
    phi = 0
  } 
  
  if (any(length(theta) == 0) | any(is.na(theta)) | any(is.null(theta))) {
    theta = 0
  }
  
  if (max(abs(phi))>1) {
    stop("Function is not stationary")
  }
  
  if (max(abs(theta))>1) {
    stop("Function is not invertible")
  }
  
  p <- length(phi)
  q <- length(theta)
  y <- rep(0, n)
  if (p >= q) {
    k <- p
  } else {
    k <- q
  }
  
  expression <- ""  # Initialize expression as an empty character string
  
  j <- 1
  for (j in seq(1,j <= p)){
    #final_phi <- final_phi + phi[j] * y[i - j]
    expression <- paste(expression, "+ phi[", j, "] * y[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  phi_expression <- substring(expression, 3)
  
  j <- 1
  for (j in seq(1,j <= q)) {
    #final_theta <- final_theta + theta[j]*error[i-j]
    expression <- paste(expression, "+ theta[", j, "] * error[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  theta_expression <- substring(expression, 3)
  
  # Generate remaining observations
  for (i in seq(k + 1, length = n - k)) {
    y[i] <- eval(parse(text = paste(phi_expression, "+", theta_expression, "+ error[i] + c", sep = "")))
  }
  return(y[y!=0])
}


