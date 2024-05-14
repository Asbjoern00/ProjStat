setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)


Mscale_linkfunction <- function(oli = "logit", M = 1){
  if (class(oli) == "character")  oli <- make.link(oli) 
  structure(list(
    linkfun = function(mu) oli$linkfun(mu*M),
    linkinv = function(eta) oli$linkinv(eta)*M,
    mu.eta = function(eta) oli$mu.eta(eta)*M,
    
    valideta =  oli$valideta,
    name = paste0("M-scaled ",oli$name,", M = ",signif(M,3))),
    class = "link-glm"
  )
}


nsim <- 500
n <- 800
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

hyperparams <- list(
  smoothness_orders = 1
)
miss_spec_prp <- GLM$new(A ~ w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10-1, name = "GLM misspec.")
corr_spec_prp <- GLM$new(A ~ w1+w2+w3, name = "GLM corr. spec.")

mean_lrn_cl <- GLM$new(Y ~ ., name = "GLM CL.", hyperparams = list(customlink = Mscale_linkfunction("logit", 0.20)))
mean_lrn_glm <- GLM$new(Y ~ ., name = "GLM standard")

# Misspecified Propensity Score and Misspecified Mean Learner
exps1 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = TMLE$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_cl, cross_fit = FALSE)),
  exp_2 = Experiment$new(n_sims = nsim, sim = sim, est = TMLE$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_glm, cross_fit = FALSE))
)
cvg <- numeric(length(exps1))
K <- numeric(length(exps1))
for(exp in seq_along(exps1)){
  exps1[[exp]]$run(verbose = TRUE)
  cvg[exp] <- exps1[[exp]]$cvrg
  K[exp] <- exps1[[exp]]$est$cross_fit
}
p1 <- cowplot::plot_grid(exps1[[1]]$plot(14), exps1[[2]]$plot(10))#, exps1[[3]]$plot(10))
p1
