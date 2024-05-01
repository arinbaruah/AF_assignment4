


generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  # Generate errors
  error <- rnorm(n, mean = 0, sd = sigma)
  # Set up vector for the response with initial values set to 0
  p <- length(phi)
  q <- length(theta)
  y <- rep(0, n)
  if (length(phi) >= length(theta)) {
    k <- length(phi)
  } else {
    k <- length(theta)
  }

  expression <- ""  # Initialize expression as an empty character string

  j <- 1
  while (j <= length(phi)) {
    #final_phi <- final_phi + phi[j] * y[i - j]
    expression <- paste(expression, "+ phi[", j, "] * y[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  phi_expression <- substring(expression, 3)
  
  j <- 1
  while (j <= length(theta)) {
    #final_theta <- final_theta + theta[j]*error[i-j]
    expression <- paste(expression, "+ theta[", j, "] * error[i -", j, "]", sep = "")
    j <- j + 1
  }
  
  theta_expression <- substring(expression, 3)

  # Generate remaining observations
  for (i in seq(k + 1, length = n - k)) {
    y[i] <- eval(parse(text = paste(phi_expression, "+", theta_expression, "+ error[i] + c", sep = "")))
  }
  return(y)
}


#testdata
library(fpp3)
tsibble(time = 1:50, y = generate_arma(n=50, c=1, phi=0.8), index = time) |>
  autoplot(y)

generate_arma(n = 50, c = 2, phi = 0.4, theta = c(0.3, -0.6))

set.seed(1148)
tsibble(time = 1:50, y = generate_arma(n=50, c=1, phi=, theta=), index = time) |>
  autoplot(y)