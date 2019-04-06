library(data.table)
library(Rcpp)
library(ggplot2)

#' Simulate a random walk with 1000 draws and various other dimensions
sigma <- 0.5
ma_beta <- 1

#' Setup
df <- CJ(year_id = 2017:2050, location_id = 1:10, draw = 0:99)
setkeyv(df, c("location_id", "draw", "age_group_id", "year_id"))
df[year_id == 2017, eps := 0]
df[, sigma := sigma]

ptm <- proc.time()
for (i in (min(df$year_id)+1):max(df$year_id)){
  message(i)
  df[, eps_lag := shift(eps, n = 1L, type = "lag"), by = .(location_id, draw)]
  df[, eps_lag2 := shift(eps, n = 2L, type = "lag"), by = .(location_id, draw)]
  df[is.na(eps), eps := ma_beta * eps_lag + mapply(rnorm, n = 1, mean = 0, sd = sigma)] 
}
proc.time() - ptm
# Visualize RW
ggplot(df[location_id == 1], aes(year_id, eps)) + 
  geom_line(aes(group = draw), alpha = 0.1) 

sum <- df[location_id == 1, .(mean = mean(eps), 
                                           lower = quantile(eps, 0.025), 
                                           upper = quantile(eps, 0.975)), by = .(year_id)]

ggplot(sum, aes(year_id, mean)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)

#### Reshape wide, then loop over year, reshape back
ptm <- proc.time()
wide_df <- dcast.data.table(df, location_id + draw + age_group_id ~ year_id, value.var = "eps")
wide_df[, ]
proc.time() - ptm


#### Rcpp Function ######################
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
