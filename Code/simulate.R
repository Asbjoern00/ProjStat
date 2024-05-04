#readRDS("/home/asr/Desktop/ProjStat/Data/processeddata.rds")
library(tidyverse)
m <- function(w1,w2){
  w2*exp(sin(w1))/exp(cos(10*w1))-2
}
g <- function(w1,w2){
  w2*exp(cos(w1))/exp(sin(10*w1))/6 # divide by 6 to not make the this too large
}

#Define third functions here that takes A, w1, and w2 and returns some number that will be used in the logit calc for y

simulate_from_model <- function(g,m,theta=0.5, n = 10000){
  
  #Simulate covariates
  w1 <- rnorm(n = n)
  w2 <- sign(rbinom(n,1,0.5)-0.5)
  
  #simulate A
  A <- as.numeric(m(w1,w2) + rnorm(n=n) > 0)
  
  #Convert to a probability (the true conditional mean of Y given A and W)
  sim <- (A+0.1)*theta*g(w1,w2)
  p <- logit(sim)
  cond_mean_trt <- mean(logit((1+0.1)*theta*g(w1,w2)))
  cond_mean_c <- mean(logit((0.1)*theta*g(w1,w2)))
  
  #simulate from Y
  Y <- rbinom(n = n, size = 1, prob = p)
  list(
  true_cond_mean = tibble(cond_Y_true = p, g_func = g(w1,w2)),
  out_frame = tibble(w1 = w1, w2 = w2, A = A, Y = Y),
  avg_effect = tibble(true_outcome = cond_mean_trt-cond_mean_c,
                       true_propensity = mean(m(w1,w2)>0))
  )
}
logit <- function(x){
  exp(x)/(1+exp(x))
}

