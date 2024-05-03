#readRDS("/home/asr/Desktop/ProjStat/Data/processeddata.rds")
library(tidyverse)
m <- function(w1,w2){
  w2*exp(sin(w1))/exp(cos(10*w1))-2
}
g <- function(w1,w2){
  w2*exp(cos(w1))/exp(sin(10*w1))
}
simulate_from_model <- function(g,m,theta=0.5, n = 10000){
  w1 <- rnorm(n = n)
  w2 <- sign(rbinom(n,1,0.5)-0.5)
  A <- as.numeric(m(w1,w2) + rnorm(n=n) > 0) 
  Y <- as.numeric(A*theta + g(w1,w2) + rnorm(n=n) > 0)
  list(
  out_frame = tibble(w1 = w1, w2 = w2, A = A, Y = Y),
  avg_effect = tibble(true_outcome = mean(theta + g(w1,w2)>0) - mean(g(w1,w2)>0),
                       true_propensity = mean(m(w1,w2)>0))
  )
}

