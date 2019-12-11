library(cowplot)
library(data.table)
library(directlabels)
library(ggplot2)
library(gridExtra)
library(microbenchmark)
library(RSpectra)

# 26.3 in Trefethen. Visualizing \eps-pseudospectra for a specific 
# upper triangular matrix

# RSpectra: https://cran.r-project.org/web/packages/RSpectra/vignettes/introduction.html
# MATLAB svds(): https://www.mathworks.com/help/matlab/ref/svds.html#bu4mm8y

plot_pseudospectra <- function(A, title = "e-pseudospectrum", eps = c(1,.5,.1, .01, .001)){
  #' @description Plots contour plot on the complex plane for the 
  #' pseudospectra for the matrix A, under the given epsilon.
  if(nrow(A) != ncol(A)){stop("A must be a square matrix")}
  
  n <- nrow(A)
  
  true_eigs <- eigen(A)$values
  eig_table <- data.table(a = Re(true_eigs), b = Im(true_eigs))
  
  min_Re <- min(eig_table$a)
  min_Im <- min(eig_table$b)
  max_Re <- max(eig_table$a)
  max_Im <- max(eig_table$b)
  
  # Try to narrow down where the contours are...ideally you would fix 'a' and the 
  # minimum singular value at epsilon, and solve for the complex part 'b' but 
  # I didn't know how to do that. So just finding all the computed minimum singular values
  # that are below some tolerance distance from the contour I want. Then let the contour 
  #software handle the rest
  
  # Creating grid of scalars on the complex plane. Then calculate minimum singular value for 
  # all matrices zI - A, where z \in C. data.table doesn't support complex numbers so need to make vector
  # outside of the data frame
  grid <- CJ(a = seq(min_Re - 5, max_Re + 5,.1), 
             b = seq(min_Im - 5, max_Im + 5,.1))
  c    <-  complex(real = grid$a, imaginary =grid$b)
  # Method 1: calculate all singular values and take minimum
  grid[,sigma := sapply(c, function(c) min(svd(diag(c, n) - A)$d))] 
  
  # Want to know if there's a way to use svds() or similar to calculate smallest singular values? 
  # I don't want to calculate the inverse first...should experiment 
  
  # Method 2: run svds to get 1st singular value and take reciprocal
  # grid[, sigma := sapply(c, function(c){
  #   B <- diag(c,n) - A
  #   X <- t(B) %*% B
  #   eigs_sym(X,k=1)$values
  #   #1 / eigs(Conj(t(B)) %*% B,k=1,sigma=0)$values
  # })] 
  
  p <- ggplot(grid, aes(a, b)) + 
    geom_point(data = eig_table, aes(a,b),col = "red",size=4,alpha = 0.5) + 
    geom_contour(aes(z = sigma, colour = stat(level)), 
                 size = 1.5,
                 breaks = c(eps)) + 
    theme_dark() + 
    scale_colour_distiller(palette = "Spectral", direction = 1) + 
    xlab("Real") + 
    ylab("Imaginary") + 
    ggtitle(title) + 
    labs(colour = "epsilon") + 
    geom_dl(aes(label=..level.., z = sigma), method = "top.pieces",
            stat="contour",breaks = eps) + 
    geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 0) + 
    coord_fixed() 
  p
}

############ TEST MATRICES ###################################
pdf(file = "C:/Users/thsiao3/Documents/r-experiment/eps_pseudospectrum.pdf", width = 11, height = 7)
set.seed(1)
# Identity matrix
A <- diag(10)
p1 <- plot_pseudospectra(A,title = "Identity (10)") + coord_fixed(xlim = c(-4,6), ylim = c(-4,6))

# Upper triangular - diagonal is all -1
# First and second upper subdiagonal is 1
A <- diag(-1,10)
A[(row(A) - col(A)) %in% c(-1,-2)] <- 1
p2 <- plot_pseudospectra(A, title = "Trefethen (10)") + coord_fixed(xlim = c(-4,6), ylim = c(-4,6))

A <- matrix(rnorm(100),nrow=10)
p3 <- plot_pseudospectra(A, title = "Normal Dist (10)") + coord_fixed(xlim = c(-4,6), ylim = c(-4,6))

A <- matrix(runif(10^2),nrow=10)
p4 <- plot_pseudospectra(A, title = "Unif Dist (10)") + coord_fixed(xlim = c(-4,6), ylim = c(-4,6))

A <- pracma::hilb(10)
p5 <- plot_pseudospectra(A, title = "Hilbert (10)") + coord_fixed(ylim = c(-1,3)) + coord_fixed(xlim = c(-4,6), ylim = c(-4,6))

A <- pracma::vander(seq(0,1,.1))
p6 <- plot_pseudospectra(A, title = "Vandermonde (seq(0,1,.1))") + coord_fixed(ylim = c(-4,6)) + coord_fixed(xlim = c(-4,6), ylim = c(-4,6))

grid.arrange(p1,p2,p3,p4,p5,p6,nrow=2)
dev.off()
