#' This is a code implementation and simulation of the normal theory linear measurement error model 
#' in Boos and Stefanski Essential Statistical Inference. 

#' Y_i ~ N(alpha + beta*U_i, sigma_e^2)
#' X_i ~ N(U_i, sigma^2)
#' U_1,...,U_n are unknown nuisance parameters
#' 
#' Y | T ~ N()

set.seed(1)

# knowns
n     <- 100
sigma <- 10

# unknowns
alpha   <- 2
beta    <- 4
sigma_e <- 2
U       <- runif(n, 0, 1)

# Observed data
X  <- rnorm(n, U, sigma)
Y  <- rnorm(n, alpha + beta*U, sigma_e)

# Our unknown sufficient "statistic" (depends on parameters beta and sigma_e)
T_i <- Y * beta / sigma_e^2 + X / sigma^2

#' Perform standard linear regression on Y|X. Based on the consistency and bias of the beta estimated by Y|X, 
#' It underestimates the true beta by a factor of (sigma_u^2 / (sigma_u^2 + sigma^2)). So if the known
#' measurement error variance is small, using X to regress is not a huge issue. Depends on degree of 
#' measurement error as well as natural spread within the distribution of U. 

lm(Y ~ X)
lm(Y ~ U)

#' If we knew U we could just optimize the likelihood based on Y and U
loss_ideal <- function(theta, Y, U){-sum( dnorm(Y, theta[1] + theta[2] * U, exp(theta[3]), log = TRUE) )}
fit_ideal <- optim(c(1, 1, .5), loss_ideal, Y = Y, U = U)

#' Now use the conditional likelihood technique to eliminate the U parameters using only Y and X

#' Approach 1: Plug in the definition of T_i from the beginning, and do MLE
#' Approach 2: Find score without taking derivative of T_i with respect to any params, then plug in 
#' definition of T_i to solve score equations. Compare. 

log_conditional_pdf <- function(y, t, alpha, beta, sigma_e, sigma){
  mu_cond <- (alpha / sigma^2 + beta*t) * sigma_e^2 * sigma^2 / (sigma^2 * beta^2 + sigma_e^2)
  sigma_cond <- sqrt( sigma_e^4 / (sigma^2 * beta^2 + sigma_e^2) )
  dnorm(y, mu_cond, sigma_cond, log = TRUE)
}

loss <- function(theta, Y, X, sigma){
  alpha   <- theta[1]
  beta    <- theta[2]
  sigma_e <- exp(theta[3])
  
  # The sufficient "statistic"
  T_i <- Y * beta / sigma_e^2 + X / sigma^2
  
  loglik <- log_conditional_pdf(Y, T_i, alpha, beta, sigma_e, sigma)
  
  return(-sum( loglik ))
}

fit       <- optim(c(2, 10, 2), loss, sigma = sigma, Y = Y, X = X)


#' Approach 1 does not converge when optimizing the plugged in T_i directly. Highly dependent on the 
#' initial guess. We can then try to derive the score equations analytically then plugging in T_i afterwards. 