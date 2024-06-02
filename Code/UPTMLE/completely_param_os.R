setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)
sim_cov <- function(n = 100){
  w1 <- rnorm(1, n = n)
  w2 <- rbinom(n,1,0.65)*w1
  w3 <- rnorm(-1,n = n)
  w4 <- rnorm(1, n = n)
  W <- cbind(w1,w2,w3,w4)
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
    plogis(3*A + -1*W[,4] - 3*W[,3])
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
mean_corr_spec <- GLM$new(Y~A + w4 + w3 -1, name = "GLM prp")

exps <- list(
  Experiment$new(sim = sim, est = OS$new(prp_lrn = prp_corr_spec, mean_lrn = mean_corr_spec,cross_fit = 50), n_sim = nsim),
  Experiment$new(sim = sim, est = OS$new(prp_lrn = prp_corr_spec, mean_lrn = mean_corr_spec,cross_fit = 2), n_sim = nsim),
  Experiment$new(sim = sim, est = OS$new(prp_lrn = prp_corr_spec, mean_lrn = mean_corr_spec,cross_fit = FALSE), n_sim = nsim)
  
)

# For loop to run all experiments
for(i in 1:length(exps)){
  exps[[i]]$run()
  saveRDS(exps, file = "/home/asr/Desktop/ProjStat/Code/UPTMLE/parametric_exp_os.rds")
}


