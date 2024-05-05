
generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  
  # Common errors to handle 
  if (any(length(phi) == 0) | any(is.na(phi)) | any(is.null(phi))) {
    phi = 0
  } 
  
  if (any(length(theta) == 0) | any(is.na(theta)) | any(is.null(theta))) {
    theta = 0
  }
  
  # Check if stationary
  
  if (any(abs(polyroot(c(1,-phi))) <= 1)) {
    stop("Model is not stationary")
  }
  
  # Check if invertible
  
  if (any(abs(polyroot(c(1,theta))) <= 1)) {
    stop("Model is not invertible")
  }
  p <- length(phi)
  q <- length(theta)
  y <- rep(0, n)
  
  # Generate errors with extra observations
  error <- rnorm(n + 200, mean = 0, sd = sigma)
  
  if (p >= q) {
    max_component <- p
  } else {
    max_component <- q
  }
  
  expression <- ""  # Initialize expression as an empty character string
  
  # Perform AR calculation
  j <- 1
  for (j in seq(1,j <= p)){
    expression <- paste(expression, "+ phi[", j, "] * y[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  phi_expression <- substring(expression, 3)
  


  expression <- ""  # Initialize expression as an empty character string
  
  # Perform MA calculation
  
  j <- 1
  for (j in seq(1,j <= q)) {
    expression <- paste(expression, "+ theta[", j, "] * error[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  theta_expression <- substring(expression, 3)
  
  # Add constant and error to ARMA sum calculation with extra observations
  
  for (i in seq(max_component + 1, length = n + max_component)) {
    y[i] <- eval(parse(text = paste(phi_expression, "+", theta_expression, "+ error[i] + c", sep = "")))
  }
  
  # Remove initial observations with 0s by printing from (max_component + 1) to n 
  return(y[max_component + 1 : n])
} 

