library(data.table)
library(magrittr)
library(ggplot2)

# http://cohort-fertility.schmert.net/index.html

n     <- 90 # Number of steps to forecast
drift <- 0  # Drift
beta1 <- 1 # AR(1) coefficient
last  <- 20

# Create draws
draws <- lapply(1:500, function(x){
  eps <- rnorm(n)
  ar1 <- last
  for (i in 2:n){
    ar1[i] <- ar1[i-1] * beta1 + eps[i] + drift
  }
  dt <- data.table(draw = x, t = 1:n, value = ar1)
}) %>% rbindlist

summary <- draws[, .(mean = mean(value), 
                     lower = quantile(value, .025), 
                     upper = quantile(value, 0.975)), by = t]

ggplot(draws, aes(t, value)) + 
  geom_line(aes(group = draw), alpha = 0.3) + 
  theme_bw()

ggplot(summary, aes(t, mean)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_bw()
