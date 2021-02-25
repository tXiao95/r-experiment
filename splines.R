library(data.table)
library(ggplot2)

k <- c(.2, .5, .7)
x <- seq(0,1,.01)

#' Cubic spline
#' X
#' X^2
#' (X-a)^3+
#' (X-b)^3+
#' (X-c)^3+

spline <- function(x, k){
  X <- matrix(0, ncol=length(k)+3, nrow=length(x))
  X[, 1] <- 1
  X[, 2] <- x
  X[, 3] <- x^2
  for(i in 1:length(k)){
    a <- k[i]
    X[, 3+i] <- ifelse(x < a, 0, (x-a)^3)
  }
  return(X)
}

design <- spline(x, k)
b <- c(1, 2, 3, 4, -100, 1)

y <- as.matrix(design) %*% b

X <- splines::bs(x, knots=k)

y2 <- X %*% b

plot(x, y)
plot(x, y2)
