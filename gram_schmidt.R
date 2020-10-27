QR <- function(A){
  m <- nrow(A)
  n <- ncol(A)
  
  Q <- matrix(0, m, n)
  R <- matrix(0, n, n)
  
  #' Triangular orthogonalization
  #' q_j = v_j - <v_j,e1>e1 - <v_j,e2>e2 - ...- <v_j,e_{j-1}>e_{j-1}
  #' v_j = <v_j,e1>e1 + ... +<v_j,e_{j-1}>e_{j-1} + norm(q_j) * e_j
  #' 
  #' i < j
  #' R[j,j] = norm(q_j) = ||q_j||
  #' R[i,j] = <v_j,e_i>
  
  for(j in 1:n){
    v_j <- A[,j]
    q_j <- v_j
    if(j > 1){
      for(i in 1:(j-1)){
        e_i <- Q[,i]
        inner <- sum(v_j * e_i)
        q_j <- q_j - inner * e_i
        R[i,j] <- inner
      }
    }
    
    norm_q_j <- sqrt(sum(q_j * q_j))
    R[j,j] <- norm_q_j
    e_j <- q_j / norm_q_j
    Q[,j] <- e_j
  }
  return(list(Q=Q, R=R))
}

A <- matrix(rnorm(15,2), 5, 3)

A

Q <- QR(A)$Q
R <- QR(A)$R

norm(Q %*% R - A)

