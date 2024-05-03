
generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  # Generate errors
  error <- rnorm(n, mean = 0, sd = sigma)
  
  # Common errors to handle 
  if (any(length(phi) == 0) | any(is.na(phi)) | any(is.null(phi))) {
    phi = 0
  } 
  
  if (any(length(theta) == 0) | any(is.na(theta)) | any(is.null(theta))) {
    theta = 0
  }
  # Check if stationary
  if (max(abs(phi))>1) {
    stop("Function is not stationary")
  }
  # Check if invertible
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
  
  # Perform AR calculation
  j <- 1
  for (j in seq(1,j <= p)){
    expression <- paste(expression, "+ phi[", j, "] * y[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  phi_expression <- substring(expression, 3)
  
  # Perform MA calculation
  j <- 1
  for (j in seq(1,j <= q)) {
    expression <- paste(expression, "+ theta[", j, "] * error[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  theta_expression <- substring(expression, 3)
  y <- rep(0, n)
  # Add constant and error to ARMA calculation
  for (i in seq(k + 1, length = n - k)) {
    y[i] <- eval(parse(text = paste(phi_expression, "+", theta_expression, "+ error[i] + c", sep = "")))
  }
  # Remove initial observations with 0s by printing from k+1 to n
  return(y[(k + 1) : n])
} 



