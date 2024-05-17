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
    logit(W[,1] - 2*W[,2] + 0.5*W[,3])
  }
  prob_A <- pA(W)
  A <- rbinom(n = nrow(W), size = 1, prob = prob_A)
  list(A = A, pA = prob_A)
}

sim_Y <- function(A,W){
  pY <- function(A,W){
    #make rare outcome, change here if unwanted
    logit(3*A + -1*W[,7] - 3*W[,3])
  }
  prob_Y <- pY(A,W)
  Y <- rbinom(n = nrow(W), size = 1, prob = prob_Y)
  list(Y = Y, pY1 = pY(1,W), pY0 = pY(0,W), pY = prob_Y)
}



#SIMULATION SETTINGS
nsim <- 250
n <- 500
set.seed(123)
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)
prp_corr_spec <- GLM$new(A~w1+w2+w3-1, name = "GLM prp")
mean_corr_spec <- GLM$new(Y~A + w7 + w3 -1, name = "GLM prp")



