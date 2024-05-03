
generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  
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
  
  # Generate errors with extra observations
  error <- rnorm(n + 200, mean = 0, sd = sigma)
  
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
  expression <- ""  # Initialize expression as an empty character string
  
  j <- 1
  for (j in seq(1,j <= q)) {
    expression <- paste(expression, "+ theta[", j, "] * error[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  theta_expression <- substring(expression, 3)
  
  # Add constant and error to ARMA sum calculation with extra observations
  for (i in seq(k + 1, length = n + k)) {
    y[i] <- eval(parse(text = paste(phi_expression, "+", theta_expression, "+ error[i] + c", sep = "")))
  }
  # Remove initial observations with 0s by printing from k+1 to n
  return(y[k + 1 : n])
} 



library(fpp3)
n <- 50
p <- 1
q <- 1

tsibble(time = 1 : 100, y = generate_arma(n = 100, c=1, phi=0.3, theta = c(0.2,0.3),index = time)) |>
  gg_tsdisplay(y,plot_type = "partial")


