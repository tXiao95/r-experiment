# R-Squared experiment for linear vs. nonlinear models
beta0 <- 1
beta1 <- 0.98
beta2 <- 1.1
n     <- 100

set.seed(123)
eps <- rnorm(n, 0, 2)
x   <- runif(n, -1, 5)
x2  <- x^2
ylin <- beta0 + beta1 * x + eps
yqua <- beta0 + beta1 * x + beta2 * x^2 + eps

plot(x, yqua)

fit <- lm(yqua ~ x + x2)
pred <- predict(fit)

# SST = SSreg + SSres
SST   <- sum((yqua - mean(yqua))^2)
SSreg <- sum((pred - mean(yqua))^2)
SSres <- sum((yqua - pred)^2)

abs(SST - (SSreg + SSres))

