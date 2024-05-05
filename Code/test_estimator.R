source("Estimator.R")
source("LearnerTypes.R")
source("simulate.R")

#Initialise estimator Class with using GLMnet as propensity learner and GLMnet as mean learner
mean_lrn <- GLM$new(Y ~ w9 + w10 + A)

estimator <- Estimator$new(prp_lrn = GLM$new(A ~ w1 + w2 + w3), mean_lrn = mean_lrn)


#Repeat experiment 1000 times, each time simulating 500 data points
theta <- numeric(1000)
ATE <- numeric(1000)
for(i in 1:1000){
  sim_data <- simulate_from_model(n = 500)
  theta[i] <- estimator$fit(sim_data$out_frame, one_step = FALSE, cross_fit = FALSE)
  ATE[i] <- sim_data$ATE
  print(i)
}
library(ggplot2)
#make dataframe of theta values and plot histogram with a vertical line at 
df <- tibble(theta = theta)


estimator$mean_lrn$predict(sim_data$out_frame)
mean(sim_data$true_cond_mean$cond_Y_true-estimator$mean_lrn$predict(sim_data$out_frame))

ggplot(df, aes(x = sqrt(500)*(theta-mean(ATE)))) + geom_histogram(bins = 50) + geom_vline(xintercept = 0, color = "red") + theme_minimal()
