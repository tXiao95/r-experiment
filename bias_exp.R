# We have a symmetric, log-transformed series. The mean is 1.5, sigma is 1
mu    <- 1.5
sigma <- 1

# W is normal, Y is LogNormal
w     <- rnorm(1000000, mean = mu, sd = sigma)
y     <- exp(w)
y_adj <- exp(w - var(w)/2)

hist(w)
hist(y, breaks = 1000, xlim = c(0, 50))
hist(y_adj, breaks = 1000, xlim = c(0, 50))

# Mean of Y is equal to exponentiated adjusted mean of W
mean(y)
exp(mean(w) + var(w)/2)

# Mean of adjusted-W draws = the median of Y = exponentiated mean of W
exp(mean(w))
mean(y_adj)
median(y)

# Do we usually want mean of W to be preserved? Yes. We want exp(mean of W) to be same as mean(Y).

# If you already have the mean of the log distribution, you can just exponentiate it. The only problem 
# occurs when you have draws of the log distribution, in which case you need to adjust them so that their mean when exponentiated
# is the same as the exponentiated mean of the original draws