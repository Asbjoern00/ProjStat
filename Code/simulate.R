#readRDS("/home/asr/Desktop/ProjStat/Data/processeddata.rds")
library(tidyverse)

#Make a function that generates ten covariates and applies some transformation of these 
sim_cov <- function(n = 100){
  w1 <- rnorm(1, n = n)
  w2 <- rbinom(n,1,0.65)*w1
  w3 <- rnorm(-1,n = n)
  w4 <- rbinom(n,1,0.65)*w2
  w5 <- rnorm(n = n)
  w6 <- rbinom(n,1,0.47)*w1
  w7 <- rnorm(n = n)
  w8 <- rbinom(n,1,0.65)*w7
  w9 <- rnorm(n = n)
  w10 <- rbinom(n,1,0.35)*w9
  W <- cbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10)
  W
}
logit <- function(x){
  exp(x)/(1+exp(x))
}

#Function that takes output from sim_cov and simulates A using some non-linear function of W
sim_A <- function(W){
  prob_A <- logit(W[,1] + 2*W[,2] + 0.5*W[,3])
  A <- rbinom(n = nrow(W), size = 1, prob = prob_A)
  A
}

sim_Y <- function(A,W){
  pY <- function(A,W){
    logit(0.5 + 10*A / (0.5*exp(W[,1])) *sin(W[,2])^2 * log(0.5*W[,3]^2) + 0.4*cos(W[,4]) + 0.3*sin(W[,5]) + 0.2*cos(W[,6]) + 0.1*sin(W[,7]) + 0.1*cos(W[,8]) + 0.1*sin(W[,9]) + 0.1*cos(W[,10]))
  }
  prob_Y <- pY(A,W)
  Y <- rbinom(n = nrow(W), size = 1, prob = prob_Y)
  list(Y = Y, pY1 = pY(1,W), pY0 = pY(0,W))
}

simulate_from_model <- function(n = 100){
  
  #Simulate covariates
  W <- sim_cov(n)

  #simulate A
  A <- sim_A(W)
  
  #Simulate Y
  Y_lst <- sim_Y(A,W)
  Y <- Y_lst$Y
  ATE <- mean(Y_lst$pY1 - Y_lst$pY0)
  
  # Bind W, A, Y together to tibble, such that the columns in W are named w1, w2, ..., w10
  out_frame <- cbind(W, A, Y) %>% as_tibble()
  
  list(
  ATE = ATE,
  out_frame = out_frame
  )
}
simulate_from_model()

