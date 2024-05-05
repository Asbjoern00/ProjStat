source("Estimator.R")
source("LearnerTypes.R")
source("simulate.R")

#Initialise estimator Class with using GLMnet as propensity learner and GLMnet as mean learner
mean_lrn <- GLM$new(Y ~ w1 + w3 + w9 + w10 + A)

estimator <- Estimator$new(prp_lrn = GLM$new(A ~ w1 + w2 + w3), mean_lrn = mean_lrn)


#Repeat experiment 1000 times, each time simulating 500 data points
theta <- numeric(1000)
ATE <- numeric(1000)
asvar <- numeric(1000)
asvarest <- numeric(1000)
for(i in 1:1000){
  sim_data <- simulate_from_model(sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y, n = 250)
  lst <-  estimator$fit(sim_data$out_frame, one_step = TRUE, cross_fit = FALSE)
  theta[i] <- lst$ATE
  asvarest[i] <- lst$asvar
  ATE[i] <- sim_data$ATE
  asvar[i] <- sim_data$asvar
  print(i)
}

library(ggplot2)
#make dataframe of theta values and plot histogram with a vertical line at 
df <- tibble(theta = theta)

#plot experimental results alongside normal distribtion with mean 0 and variance equal to the average squared efficient influence function
# plot geom_histogram as density

ggplot(df) + geom_histogram(aes(x = sqrt(500)*(theta-mean(ATE)), after_stat(density)),bins = 10) + geom_density(aes(x = sqrt(500)*(theta-mean(ATE)), after_stat(density))) + geom_vline(xintercept = 0, color = "red") +
  stat_function(fun = dnorm, args = list(mean = 0, sd =mean(sqrt(asvarest))), color = "blue") + theme_minimal()
