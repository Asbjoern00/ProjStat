# Make R6 class for simulation. Takes arguments for number of observations, how to simulate covariates, how to simulate A, and how to simulate Y
Simulator <- R6::R6Class("Simulator",
                         public = list(
                           n = NULL,
                           sim_cov = NULL,
                           sim_A = NULL,
                           sim_Y = NULL,
                           out_frame = NULL,
                           ATE = NULL,
                           asvar = NULL,
                           initialize = function(n, sim_cov, sim_A, sim_Y){
                             self$n <- n
                             self$sim_cov <- sim_cov
                             self$sim_A <- sim_A
                             self$sim_Y <- sim_Y
                           },
                           simulate = function(){
                             #Simulate covariates
                             W <- self$sim_cov(self$n)
                             
                             #simulate A. Function sim_A should return a list with A and pA
                             A_lst <- self$sim_A(W)
                             A <- A_lst$A
                             propA <- A_lst$pA
                             
                             #Simulate Y. Function sim_Y should return a list with Y, pY1, pY0
                             Y_lst <- self$sim_Y(A,W)
                             Y <- Y_lst$Y
                             ATE <- mean(Y_lst$pY1 - Y_lst$pY0)
                             
                             # Bind W, A, Y together to tibble, such that the columns in W are named w1, w2, ..., w10
                             out_frame <- cbind(W, A, Y) %>% as_tibble()
                             
                             self$ATE <- ATE
                             self$asvar <- mean(private$eif(Y = Y, pY1 = Y_lst$pY1, pY0 = Y_lst$pY0, A = A, propA = propA, pY = Y_lst$pY)^2)
                             self$out_frame <- out_frame
                           }
                         ),
                         private = list(
                           eif = function(Y, pY1, pY0, pY , A, propA){
                             clever_cov <- (A/propA) - (1-A)/(1-propA)
                             clever_cov*(Y-pY) + (pY1 - pY0) - self$ATE 
                           }
                         )
)



library(tidyverse)
logit = function(x){
  exp(x) / (1 + exp(x))
}
#Make a function that generates ten covariates and applies some transformation of these 
sim_cov <- function(n = 100){
  w1 <- rnorm(1, n = n)
  w2 <- rbinom(n,1,0.65)*w1
  w3 <- rnorm(-1,n = n)
  w4 <- rbinom(n,1,0.65)*w2
  w5 <- rnorm(n = n)
  w6 <- rbinom(n,1,0.47)*w1
  w7 <- rnorm(1, n = n)
  w8 <- rbinom(n,1,0.65)
  w9 <- rnorm(n = n)
  w10 <- rbinom(n,1,0.35)
  W <- cbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10)
  W
}


#Function that takes output from sim_cov and simulates A using some non-linear function of W
sim_A <- function(W){
  pA <- function(W){
    pmin(pmax(logit(W[,1] - 2*W[,2] + 0.5*W[,3]),0.001),0.999)
  }
  prob_A <- pA(W)
  A <- rbinom(n = nrow(W), size = 1, prob = prob_A)
  list(A = A, pA = prob_A)
}

sim_Y <- function(A,W){
  pY <- function(A,W){
    #make rare outcome, change here if unwanted
    logit(6*A/(0.5*exp(W[,1])) *sin(W[,2])^2 - (1+log(0.5*W[,3]^2))^2 + 0.4*cos(W[,4]) + 0.3*sin(W[,5]) - 0.2*cos(W[,6])^2 - exp(W[,7]) + 0.1*cos(W[,8]) + 0.1*sin(W[,9]) + 0.1*cos(W[,10]))
  }
  prob_Y <- pY(A,W)
  Y <- rbinom(n = nrow(W), size = 1, prob = prob_Y)
  list(Y = Y, pY1 = pY(1,W), pY0 = pY(0,W), pY = prob_Y)
}

