# simulation for SIR
library(ggplot2)
library(data.table)

#' Situation 1: independent events. Everyone in Suseptible has same probability of contracting the
#' disease, and it does not spread person to person. 
#' 
#' Start with 1000 healthy people and 0 infected. In this case, this is more like some idea in a group
#' like buy a home. There may be some marginald effect of my friend buying a home, but really it will 
#' depend on things that are influenced by those around me like my income, my family status, my 
#' intergenerational wealth, my spouse. Also there's no concept of recovery either. You either buy a home, 
#' or you don't. What does this look like in SIR world?
S0 <- 300000000
I0 <- 1
R0 <- 0
D0 <- 0
N <- sum(S0,I0,R0)

b <- .5 # Number of people an infected person infects per day. Social distancing, contact tracing, 
# and hospitalizations affect this parameter
d <- 7 # Recovery time. If a treatment is found, it affects this parameter
m <- 1 / 100

t <- 100

#' Situation 2. 
#' 
#' Dependent events. Spreads by human-to-human transmission. 
#' 
#' I start with 999 susceptible and 1 infected person. 
#' 
#' On average, every person sees about 10 people a day. Of those 10 people, 
#' S(t) / N * 10 are susceptible to be newly infected. Of those people, let's say
#' half get infected. so newly infected people are dI/dt = beta*(S(t)/N)*B*I(t)
#' 
#' NumInfect * NumSee * PropS * PropInfect
#' 
#' On average, it takes an infected person 3 days to recover. So 
#' dR/dt = (1/d) * I(t), where d is days to recover. 
progress <- function(S0, I0, R0, D0, t, b, d, m){
  df <- data.table(t=0:t, S=S0, I=I0, R=R0, D=D0)
  k <- 1/d
  
  N <- S0 + I0 + R0
  for(x in 1:t){
    Slag <- df[t==x-1, S]
    Ilag <- df[t==x-1, I]
    Rlag <- df[t==x-1, R]
    Dlag <- df[t==x-1, D]
    df[t==x, S := Slag - b * Slag / N * Ilag]
    df[t==x, I := Ilag + b * Slag / N * Ilag - (k + m) * Ilag]
    df[t==x, R := Rlag + k * Ilag]
    df[t==x, D := Dlag + m * Ilag]
  }
  
  df
}


df <- progress(S0, I0, R0, D0, t, b, d, m)
df[, dI := c(NA, diff(I))]


ggplot(df, aes(t, S/N, col="Susceptible")) + geom_line() + 
  geom_line(aes(t, I/N, col="Infected")) + 
  geom_line(aes(t, R/N, col="Recovered")) + 
  geom_line(aes(t, (I + R)/N, col="I + R")) + 
  geom_line(aes(t, D/N, col="Dead")) + 
  ggtitle("SIR") + 
  theme_bw()
