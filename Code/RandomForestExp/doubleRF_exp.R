setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)



#SIMULATION SETTINGS

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
    plogis(W[,1] - 2*W[,2] + 0.5*W[,3])
  }
  prob_A <- pA(W)
  A <- rbinom(n = nrow(W), size = 1, prob = prob_A)
  list(A = A, pA = prob_A)
}

sim_Y <- function(A,W){
  pY <- function(A,W){
    #make rare outcome, change here if unwanted
    plogis(3*A/(0.5*exp(W[,1])) *sin(W[,2])^2 - (1+log(0.5*W[,3]^2))^2 + 0.4*cos(W[,4]) + 0.3*sin(W[,5]) - 0.2*cos(W[,6])^2 - exp(W[,7]) + 0.1*cos(W[,8]) + 0.1*sin(W[,9]) + 0.1*cos(W[,10]))
  }
  prob_Y <- pY(A,W)
  Y <- rbinom(n = nrow(W), size = 1, prob = prob_Y)
  list(Y = Y, pY1 = pY(1,W), pY0 = pY(0,W), pY = prob_Y)
}



nsim <- 500
n <- 1000
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)


prp_rf_ib <- RF$new(A~., name = "RF prp",oob = FALSE, autotune =FALSE,list("num.trees" = 500))
mean_rf_ib <- RF$new(Y~., name = "RF mean",oob = FALSE, autotune =FALSE, hyperparams = list("num.trees" = 500))
prp_corr_spec <- GLM$new(A~w1+w2+w3-1, name = "GLM prp")
#Create list of Experiment objects to illustrate the importance of the rate criterion for convergence. 
#All optimize the parameters of the random forest automatically
#All use cross-fitting to eliminate bias from overfitting
# All using TMLE as the estimator
# 1. corr_spec_prp and mean_rf_ib
# 2. prp_rf_ib and mean_rf_ib
# 3. Increasing K of 1.
# 4. Increasing K of 2.

exps <- list(
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_corr_spec, mean_lrn = mean_rf_ib,cross_fit = 2), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_rf_ib, mean_lrn = mean_rf_ib,cross_fit = 2), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_corr_spec, mean_lrn = mean_rf_ib,cross_fit = 10), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_rf_ib, mean_lrn = mean_rf_ib,cross_fit = 10), n_sim = nsim)
)

# For loop to run all experiments
for(i in 1:length(exps)){
  exps[[i]]$run()
  #Save results in folder 
  saveRDS(exps, file = "/home/asr/Desktop/ProjStat/Code/RandomForestExp/rate_experiment2.rds")
}


