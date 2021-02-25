library(clustMD)
library(data.table)
library(ggplot2)

df <- data.table(Byar)
ggplot(df, aes(Serum.prostatic.acid.phosphatase, Stage)) + geom_point()

n <- 10
v <- matrix(c(1,rep(0,n-1)), ncol = 1)
u <- v
u <- matrix(c(0,1,rep(0,n-2)),ncol=1)

identity_perturbation <- function(n, u, v){
  outer_product <- u %*% t(v)
  inner_product <- t(v) %*% u
  A <- diag(n) + outer_product
  alpha <- as.numeric(-1 / (inner_product + 1))
  
  A_inv <- diag(n) + alpha * outer_product
}

v <- matrix(c(3,0,0))

z <- matrix(rpois(3,2))
z

v %*% t(z)
