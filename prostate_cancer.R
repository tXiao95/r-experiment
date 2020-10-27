library(data.table)
library(ggplot2)
library(haven)

df <- read_dta("C:/Users/thsiao3/Documents/r-experiment/prostate.dta")
df <- as_factor(df)

dt <- data.table(df)

dt[, status_short := ifelse(status == "alive", "alive", "dead")]

y <- dt$status_short

#' patno: Patient number
#' stage: 
#' rx: prescription
#' dtime: Months of follow-up
#' status: y, alive or dead
#' age: Age in years
#' wt: Weight index
#' hx: History of CVD
#' ap: Serum prosphatic
#' bm: bone metastases

# Visualize: x-axis as follow-up time, and y-axis as proportion of total 
dt <- dt[,.(dtime, status_short)]
dt <- dt[order(dtime)]
dt[, dx := sum(status_short == "dead"), dtime]

lt <- dt[, .(dx = sum(status_short == "dead")),dtime]

lt[,cdx := cumsum(dx)]
lt[, lx := nrow(dt) - cdx]
lt[, lag_lx := shift(lx,1)]
lt[, qx := dx / lag_lx]
lt <- lt[2:nrow(lt)]
lt[, Sx := cumprod(1-qx)]
lt[, cqx := 1-Sx]

complexity <- data.table(n=1:2000)
complexity[, nlogn := n*log(n)]
complexity[, n2 := n^2]
complexity[, n3 := n^3]
complexity <- melt(complexity, id.vars = c("n"), 
                   variable.name = "type", value.name="speed")
ggplot(complexity, aes(n, speed)) + geom_line(aes(col = type)) + scale_y_log10() +
