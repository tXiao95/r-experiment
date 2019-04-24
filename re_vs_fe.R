library(data.table)
library(ggplot2)
library(lme4)

# Simulating to see when random effects models differ from fixed effects 
#' 1. How often is that predicting WITHOUT random effects is the same as predicting with a fixed effects model? 
#' If they are the same, what is the point of running a RE model in the first place, which is computationally more intensive? 
#' 2. If the models have a similar estimate for the fixed effect coefficient - what does this really mean? In what scenario would one 
#' expect to see same fixed effect coefficient b/w a RE and FE model? 
#' 3. What effect does a RE have on the standard errors of the betas? 

# Set parameters
global_mu <- 5 
n <- 1000
sigma_i <- 10
sigma_mu <- 20
j <- 20 # number of groups
beta1 <- 3.4
x1 <- runif(n, 0, 40)

#### RANDOM INTERCEPT EXAMPLE ###############
mu_j <- rnorm(j, mean = global_mu, sd = sigma_mu)
x <- runif(n, 0, 10)
eps <- rnorm(n, mean = 0, sd = sigma_i)

#' Compute y vector from global mean, group mean, covariate, and eps
y <- global_mu + mu_j + beta1 * x1 + eps

df <- data.table(y = y, j = 1:j)
df[, j := as.factor(j)]

ggplot(df, aes(y)) + geom_histogram(bins=50, color = "black") + facet_wrap(~j) 
ggplot(df, aes(x1, y)) + geom_point(aes(color = j), alpha = 0.3) + facet_wrap(~j) 

# Full pooling - fixed effect model no group 
full.fit <- lm(y ~ x1, data = df)
full.fit

# Partial pooling - random effects model
partial.fit <- lmer(y ~ x1 + (1|j), data = df)
partial.fit

# No pooling - fixed effects model - group covariate
none.fit <- lm(y ~ x1 + j, data = df)
none.fit

df[, pred.none := predict(none.fit, df)]
df[, pred.full := predict(full.fit, df)]
df[, pred.partial := predict(partial.fit, df)]

ggplot(df, aes(pred.full, pred.partial)) + geom_abline() + geom_point(alpha=0.5, aes(color = j))

#### RANDOM SLOPE EXAMPLE #####################

#### RANDOM SLOPE AND INTERCEPT EXAMPLE #########