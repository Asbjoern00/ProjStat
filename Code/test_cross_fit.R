source("Estimator.R")
source("LearnerTypes.R")
source("simulate.R")

#Initialise estimator Class with using GLMnet as propensity learner and GLMnet as mean learner
mean_lrn <- GLM$new(Y ~ w1 + w3 + w9 + w10 + A)

estimator <- Estimator$new(prp_lrn = GLM$new(A ~ w1 + w2 + w3), mean_lrn = mean_lrn)


#Repeat experiment nsim times, each time simulating 500 data points
nsim <- 500
theta <- numeric(nsim)
ATE <- numeric(nsim)
asvar <- numeric(nsim)
asvarest <- numeric(nsim)
clow <- numeric(nsim)
chigh <- numeric(nsim)
for(i in 1:nsim){
  sim_data <- simulate_from_model(sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y, n = 500)
  estimator$fit(sim_data$out_frame, one_step = TRUE, cross_fit = 2)
  theta[i] <- estimator$ATE
  asvarest[i] <- estimator$asvar
  clow[i] <- estimator$confint_lwr
  chigh[i] <- estimator$confint_upr
  ATE[i] <- sim_data$ATE
  asvar[i] <- sim_data$asvar
  print(i)
}

library(ggplot2)
#make dataframe of theta values and plot histogram with a vertical line at 
df <- tibble(theta = theta)

#plot experimental results alongside normal distribtion with mean 0 and variance equal to the average squared efficient influence function
# plot geom_histogram as density
ggplot(df) + geom_histogram(aes(x = sqrt(500)*(theta-mean(ATE)), after_stat(density)),bins = 30) + geom_density(aes(x = sqrt(500)*(theta-mean(ATE)), after_stat(density))) + geom_vline(xintercept = 0, color = "red") +
  stat_function(fun = dnorm, args = list(mean = 0, sd =mean(sqrt(asvar))), color = "blue") + theme_minimal()

#check if confidence intervals cover the true ATE
mean(clow < ATE & chigh > ATE)