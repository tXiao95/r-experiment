#Rcpp Examples

library(Rcpp)
library(microbenchmark)
setwd("/home/twh42/Documents/r-experiment")
sourceCpp("rcpp_practice.cpp")

x <- as.logical(sample(T, 1000, replace = T))
microbenchmark(
  all(x), 
  allC(x)
)

x <- runif(1000, .5, 1)
microbenchmark(
  cumsum(x), 
  cumsumC(x)
)

microbenchmark(
  diff(x), 
  diffC(x)
)

microbenchmark(
  range(x), 
  rangeC(x)
)

