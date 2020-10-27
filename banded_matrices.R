library(pracma)
library(Matrix)

indices <- pracma::meshgrid(1:10)

rankMatrix(indices$X)
rankMatrix(diag(10))

p <- 4

# Create band matrix, with numbands = 2p + 1
A <- ifelse(abs(indices$X - indices$Y) > p, 0, 1)

sparseMatrix()

A <- matrix(c(1,1,1,1,1,1,0,0,1,0,-1,4), nrow = 4)
