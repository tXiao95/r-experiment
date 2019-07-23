#MCMC Chapter Problems
library(rethinking)

# 8H1: Run the model below and then inspect the posteerior distribution and explain what it is accomplishing
mp <- map2stan(
  alist(
    a ~ dnorm(0,1), 
    b ~ dcauchy(0,1)
  ), 
  data=list(y=1), 
  start=list(a=0,b=0),
  iter=1e4, warmup=100, WAIC=FALSE)

# 8H2: 