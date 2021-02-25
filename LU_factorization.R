A <- matrix(rpois(9, 2), nrow=3, ncol=3, byrow=T)
b <- c(2,2,2)

A[1,1] <- 1e-20

A <- matrix(c(1,-1,2,-1,5,0,2,0,9),nrow=3)

LU_factorization <- function(A){
  #' @description LU factorization of a square matrix A, without partial pivoting
  #' @return List of both the L and U factors
  if(nrow(A)!=ncol(A)){
    stop("A must be a square matrix for LU factorization")
  }
  
  n <- nrow(A)
  L <- diag(n)
  
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      if(A[i,i] == 0){
        stop("Since not using partial pivoting, divide by 0 error")
      }
      l     <- -A[j,i] / A[i,i]
      A[j,] <- A[i,]*l + A[j,]
      #' Elementary matrices are inverses of entire product of L. 
      #' So inverse of any elementary matrix just negative values at non-diagonal values. 
      L[j,i] <- -l
    }
  }
  list(L=L, U=A)
}

solve_LU <- function(A, b){
  #' Solve a linear system using LU factorization (without partial pivoting)
  n <- nrow(A)
  if(length(b) != n){
    stop("Rows of A must equal length of b")
  }
  LU <- LU_factorization(A)
  
  L <- LU$L
  U <- LU$U
  
  #' Need to solve two linear systems. Since triangular, use back substitution algorithms for L and U
  #' Ax = b
  #' L(Ux) = b
  #' Ly = b
  #' Ux = y
  
  ### Solve L back substitution #####
  y    <- rep(0, n)
  y[1] <- b[1] / L[1,1]
    
  for (i in 2:n){
    diff <- b[i]
    for (j in 1:(i-1)){
      # b[i] = L[i,1]*y[1] + L[i,2]*y[2] + ... + L[i,i]*y[i]
      # b[i] - L[i,1]*y[1] - L[i,2]*y[2] - ... - L[i,i-1]*y[i-1] = L[i,i]*y[i]
      diff <- diff - L[i,j]*y[j]
    }
    y[i] <- diff / L[i,i]
  }
  
  ### Solve U back substitution #####
  x    <- rep(0, n)
  x[n] <- y[n] / U[n,n]
  
  for (i in (n-1):1){
    diff <- y[i]
    for (j in n:(i+1)){
      # y[i] = U[i,1]*x[1] + U[i,2]*x[2] + ... + U[i,i]*y[i]
      # y[i] - U[i,1]*x[1] - U[i,2]*x[2] - ... - U[i,i-1]*y[i-1] = U[i,i]*y[i]
      diff <- diff - U[i,j]*x[j]
    }
    x[i] <- diff / U[i,i]
  }
  
  list(x=x, y=y,L=L, U=U)
}

solve_LU(A, b)
