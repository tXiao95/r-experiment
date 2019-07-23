#' Testing how different an ARIMA(0,0,1) is compared to a mean model added to random walk
library(forecast)
library(data.table)
library(ggplot2)
library(xts)

#' Initialize data
set.seed(123)
n <- 1000

# white noise - Normal centered around 0
eps <- ts(rnorm(n))
xcov <- ts(1:1000)
eps
# initialise the first two values:
ar1 <- ma1 <- arma11 <- arma22 <- eps[1:2]

beta1 <- 0.9
beta2 <- -0.3
drift <- 0

# loop through and create the 3:1000th values:
for(i in 3:n){
  message(i)
  rw[i]       <- rw[i-1] + eps[i] + drift
  ar1[i]      <- beta1 * ar1[i - 1] + eps[i] + drift
  ma1[i]      <- beta1 * eps[i - 1] + eps[i] + drift
  arma11[i]   <- beta1 * arma11[i - 1] + beta1 * eps[i - 1] + eps[i]  + drift
  arma22[i]   <- beta1 * arma22[i - 1] + beta2 * arma22[i - 2] + beta1 * eps[i-1] + beta2 * eps[i-2] + eps[i] + drift
  arcov       <- beta1 * xcov + beta1 * eps[i - 1] + eps[i] + drift
}

# turn them into time series, and for the last two, "integrate" them via cumulative sum
ar1 <- ts(ar1)
ma1 <- ts(ma1)
arma11 <- ts(arma11)
arima111 <- ts(cumsum(arma11))
arima222 <- ts(cumsum(cumsum(arma22)))
arcov <- ts(arcov)

# Generate plots
par(mfrow = c(2,3))
plot(ar1)
plot(ma1)
plot(arima111)
plot(arima222)
plot(arcov)

#' Fit ARIMA(0, 0, 1)
ma1 <- as.xts(ma1)
x <- 1:500
ma1_past <- ma1[x]
ma1_future <- setdiff(ma1, ma1_past)

lm.fit <- lm(ma1_past ~ 1)

arima.fit <- arima(ma1, order = c(0,0,1))

auto.arima(ma1)
plot(ma1)

x <- seq(-8,8,.01)

sigmoid <- function(x){1 / (1 + exp(x))*5}



plot(x, sigmoid(x))

