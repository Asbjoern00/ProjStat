setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)

#SIMULATION SETTINGS
nsim <- 500
n <- 1000
#Make a function that generates ten covariates and applies some transformation of these 
sim_cov <- function(n){
  w1 <- rbinom(n,1,0.20)
  w2 <- rbinom(n,1,0.65)*w1
  w3 <- rbinom(n, 1,0.3)
  w4 <- rbinom(n,1,0.7)*w3
  w5 <- rpois(n,30) #simulate an age-like variable
  w6 <- rbinom(n,1,0.47)*w4
  w7 <- rpois(n,10)
  w8 <- rbinom(n,1,0.65)
  w9 <- rbinom(n,1,0.35)
  w10 <- rbinom(n,1,0.35)*w9
  W <- cbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10)
  W
}
#Function that takes output from sim_cov and simulates A using some non-linear function of W
sim_A <- function(W){
  pA <- function(W){
    prob_A <- plogis(W[,1] - exp(2*W[,2]*W[,4]) + log((W[,3]+W[,6]+1)*(1+W[,7]))/2 + 0.7*sin(W[,8])/cosh(W[,5]+W[,9]) + W[,9]*(0.3+W[,10])^2-1.5)
    prob_A <- pmin(pmax(prob_A,0.0001),0.9999)
    prob_A
  }
  prob_A <- pA(W)
  A <- rbinom(n = nrow(W), size = 1, prob = prob_A)
  list(A = A, pA = prob_A)
}

sim_Y <- function(A,W){
  pY <- function(A,W){
    #We assume that w1, w2, .., w6 are well established riskfactors for the outcome. Therefore make the probability of 
    #Y given interactions of these assign positive probability to Y = 1. Also know that there is a risk of Y = 1 if A = 1
    risk_factors <- W[,1]*(exp(0.5*W[,2])) + log(1.5+W[,3]*W[,4])*(W[,6]+(W[,5]-35)*0.1) + cosh(0.1*(W[,5]-35)+W[,6]*W[,3]) + 0.3*A
    #Assume also that there are some unknown factors that are not well established. We let these enter the model as noise with mixed signs
    noise <- (W[,9] + sqrt(W[,7]))/(W[,8]+2) + sinh(W[,10] - 1.2)*W[,4] + 0.3*W[,9]*W[,6]
    plogis(risk_factors + noise - 5)
  }
  prob_Y <- pY(A,W)
  Y <- rbinom(n = nrow(W), size = 1, prob = prob_Y)
  list(Y = Y, pY1 = pY(1,W), pY0 = pY(0,W), pY = prob_Y)
}
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

hyperparams <- list(smoothness_orders = 0)

prp_hal <- HAL$new(A~., name = "HAL", hyperparams = hyperparams)
mean_hal <- HAL$new(Y~., name = "HAL", hyperparams = hyperparams)

prp_rf_ib <- RF$new(A~., name = "RF",oob = FALSE, autotune =TRUE, cv = TRUE,hyperparams = list("num.trees" = 500))
mean_rf_ib <- RF$new(Y~., name = "RF",oob = FALSE, autotune =TRUE, cv = TRUE, hyperparams = list("num.trees" = 500))


prp_rf_oob <-RF$new(A~., name = "RF OOB",oob = TRUE, autotune =TRUE, cv = TRUE, hyperparams = list("num.trees" = 500))
mean_rf_oob <-RF$new(Y~., name = "RF OOB",oob = TRUE, autotune =TRUE, cv = TRUE,hyperparams = list("num.trees" = 500))

prp_glm <- GLMNet$new(A ~ .^3, name = "LASSO")
mean_glm <- GLMNet$new(Y ~ .^3, name = "LASSO")

exps <- list(Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_rf_oob,mean_lrn=mean_rf_oob,cross_fit = FALSE), n_sim = nsim),
             Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_rf_ib,mean_lrn=mean_rf_ib,cross_fit = 5), n_sim = nsim)
             )
              #OOB mean, OOB prp
for(i in 1:(length(exps))){
  exps[[i]]$run()
  saveRDS(exps, file = "/home/asr/Desktop/ProjStat/Code/HALExp/AutoTunedCrossFitCV.rds")
  print(i)
}