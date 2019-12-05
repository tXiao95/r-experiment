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

# Separate spectarl abscissa's greater and less than 1

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
