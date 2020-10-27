library(pracma)
library(Matrix)

# Create diagonalizable linear operator of transpose for nxn matrices
# T(A) = t(A)

n <- 10
basis <- vector("list", n^2)
index <- 1
create_basis <- function(n){
  #' Find basis of n^2 eigenvectors for the linear operator T(A) = t(A) 
  # 'n' symmetric matrices
  for(i in 1:n){
    e <- matrix(0, nrow=n, ncol=1)
    e[i,1] <- 1
    basis[[index]] <- e %*% t(e)
    index <- index + 1
  }
  
  #'n(n-1)/2' skew-symmetric matrices
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      A <- matrix(0, nrow = n, ncol = n)
      A[i,j] <- 1
      A[j,i] <- -1
      basis[[index]] <- A
      index <- index + 1
    }
  }
  
  # 'n(n-1)/2' symmetric matrices
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      A <- matrix(0, nrow = n, ncol = n)
      A[1,1] <- 1
      A[i,j] <- 1
      A[j,i] <- 1
      basis[[index]] <- A
      index <- index + 1
    }
  }
}
