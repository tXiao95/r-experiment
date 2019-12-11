library(cowplot)
library(data.table)
library(directlabels)
library(ggplot2)
library(metR)
library(microbenchmark)
library(RSpectra)

# 26.3 in Trefethen. Visualizing \eps-pseudospectra for a specific 
# upper triangular matrix

# Upper triangular - diagonal is all -1
# First and second upper subdiagonal is 1
A <- diag(-1,32)
A[(row(A) - col(A)) %in% c(-1,-2)] <- 1
n <- nrow(A)

# Plot contours of all scalars 'z' such that the smallest singular value of 
# (zI - A) is equal to epsilon
eps <- cumprod(c(1,rep(10e-2,7)))

# Creating grid of scalars on the complex plane. Then calculate minimum singular value for 
# all matrices zI - A, where z \in C. data.table doesn't support complex numbers so need to make vector
# outside of the data frame
grid <- CJ(a = seq(-4,2,.1), b = seq(-3,3,.1))
#Remove true eigenvalue
grid <- grid[!(a==1 & b == 0)]
c    <-  complex(real = grid$a, imaginary =grid$b)

# Try to narrow down where the contours are...ideally you would fix 'a' and the 
# minimum singular value at epsilon, and solve for the complex part 'b' but 
# I didn't know how to do that. So just finding all the computed minimum singular values
# that are below some tolerance distance from the contour I want. Then let the contour 
#software handle the rest

# Want to know if there's a way to use svds() or similar to calculate smallest singular values? 
# I don't want to calculate the inverse first...should experiment 

# Method 1: calculate all singular values and take minimum
grid[,sigma := sapply(c, function(c) min(svd(diag(c, n) - A)$d))] 

# Method 2: run svds to get 1st singular value and take reciprocal
# grid[, sigma := sapply(c, function(c){
#   B <- diag(c,n) - A
#   X <- t(B) %*% B
#   eigs_sym(X,k=1)$values
#   #1 / eigs(Conj(t(B)) %*% B,k=1,sigma=0)$values
# })] 

# Sparisity pattern for matrix
spy <- function(w){
  # Get indices not equal to zero
  inds <- which(w != 0, arr.ind=TRUE )
  # Create sparse matrix with ones where not zero
  A <- sparseMatrix(inds[,1], inds[,2], x = rep(1,nrow(inds)))
  #
  image(A,main = "Matrix A")
}

pdf(file = "C:/Users/thsiao3/Documents/r-experiment/eps_pseudospectrum.pdf", width = 11, height = 7)
a <- spy(A)
p <- ggplot(grid[sigma <= 2], aes(a, b)) + 
  geom_point(aes(-1,0),col = "red",size=4) + 
  geom_contour(aes(z = sigma, colour = stat(level)), size = 1,
               breaks = c(eps)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  scale_colour_distiller(palette = "Spectral", direction = 1) + 
  xlab("Real") + 
  ylab("Imaginary") + 
  ggtitle("Epsilon pseudospectrum for matrix A") + 
  labs(colour = "epsilon") + 
  geom_dl(aes(label=..level.., z = sigma), method = "top.pieces",
          stat="contour",breaks = eps) + 
  coord_fixed()

plot_grid(a, p, ncol=2, nrow = 1)
dev.off()
