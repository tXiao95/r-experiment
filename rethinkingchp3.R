library(rethinking)
library(data.table)
data(homeworkch3)

#' Grid Search Algorithm
#' 1. Define the grid. The number of points to estimate
#' 2. Compute value of prior at each grid value
#' 3. Compute likelihood at each grid parameter value
#' 4. Compute unstandardized posterior by multiplying prior by likelihood
#' 5. Standardize by dividing each value by sum of all values

# Loaded birth1 and birth2 into vectors
df           <- data.table(first = birth1, second = birth2)
num_boys     <- df[, sum(birth1 + birth2)]
num_children <- 2 * nrow(df)

# 3H1: Using grid approximation, compute the posterior distribution for the probability
# of a birth being a boy. Assume a uniform prior probability. Which parameter value
# maximizes the posterior probability? 

# Set all parts of the equation
p_grid <- seq(0,1,length.out = 1000)
prior  <- dunif(x = p_grid, min = 0, max = 1) 
likelihood <- dbinom(x = num_boys, size = num_children, prob = p_grid)
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)
# MAP estimate
parameter <- p_grid[which(posterior == max(posterior))]

# 3H2: Using sample function, draw 10K value from the posterior distribution. 
# Use the samples to calculate various confidence intervals of the posterior
draws <- sample(x = p_grid, 
                size = 10000, 
                replace = T,
                prob = posterior)
create_interval <- function(draws, confidence = 0.95){
  lower <- (1 - confidence) / 2
  upper <- confidence + lower
  quantile(draws, prob = c(lower, upper))
}

lapply(c(0.5, 0.89, 0.97), create_interval, draws = draws)

#' 3H3: Use rbinom to simulate 10K replicates of 200 births. Should end up with 
#' 10K numbers, each one a count of boys out of 200 births. Compare the distribution
#' of predicted numbers to the actual count. 
sim <- rbinom(n = 10000, size = 200, prob = draws)
dens(sim)
abline(v=num_boys, col="red")

#' 3H4: Now compare 10K counts of boys from 100 simulated firstborns only to the
#' the number of boys in the first births, birth1. How does the model look in this light? 
first_boys <- df[, sum(birth1)]
sim_firstborns <- rbinom(n = 10000, size = 100, prob = draws)
dens(sim_firstborns)
abline(v=first_boys, col = "red")

# A: the posterior center is a little off from the data for observed first births. In fact, 
# first birth sex distribution is much closer to 0.5 which is what one would expect. 

#' 3H5: The model assumes that sex of first and second births are independent. To check this 
#' assumption, focus now on second births that followed female first borns. Compare 10K simulated 
#' counts of boys to only those second births that followed girls. To do this correctly, 
#' you need to count the number of first born swho were girls and simulate that
#' many births, 10K times. Compare the counts of boys to the actual observed count of
#' boys following girls. How does the model look? 

df[,.(is.boy = mean(second == 1)), first]
num_second_boys_after_girl <- df[first == 0, sum(second)]
num_children_after_girl <- nrow(df[first == 0])

sim_second <- rbinom(n = 10000, size = num_children_after_girl, prob = draws)
hist(sim_second)
abline(v = num_children_after_girl, col = "red")

# The posterior diverges significantly from the data. If we assume this posterior to be true, then
# the probability of observing our data point is near 0. It seems that separate models for 
# the first and second births are required, due to the dependence between them.