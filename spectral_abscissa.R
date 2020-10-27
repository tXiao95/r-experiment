library(data.table)
library(ggplot2)
library(Matrix)
library(pracma)


# Inspired by 24.3 in Trefethen "Numerical Linear Algebra"
n <- 10

random_matrix <- function(n){
  A <- matrix(rnorm(n^2), nrow = n) - diag(2,nrow = n)
  A
}

# Separate spectral abscissa's greater and less than 1

set.seed(40)
A  <- random_matrix(10)
#A <- 1 * diag(10)
ew <- eigen(A)$values
dt <- data.table(t = 0:50)
dt[, two_norm := unlist(lapply(t, function(t) norm(Matrix::expm(t*A),"2")))]
dt[, two_norm_other := unlist(lapply(t, function(t) norm(exp(t*A),"2")))]
spectral_abscissa <- max(Re(ew))
dt[, linear := exp(t*spectral_abscissa)]

ggplot(dt, aes(t, linear)) + 
  geom_line() + 
  geom_line(data = dt, aes(t, two_norm)) + 
  scale_y_log10() 

householder <- function(w){
  I <- diag(length(w))
  I - 2 / pracma::dot(w,w) * w %*% t(w)
}

householder(c(0,0,3,4,5,5))

p <- function(z){
  -53*z + 7*z^2
}

p(2) - p(5)
p(5) - p(2)
p(6) - p(2)

4 * -53 + 32 * 7

solve(matrix(c(3,4,21,32,117,208
               ),nrow=2),c(0,0))

poly <- function(z){52*z - 13*z^2 + z^3}
poly(4)
poly(5)
poly(6)
poly(0)

x <- 1:10
y <- poly(1:10)

plot(x,y)
