source("Estimator.R")
source("LearnerTypes.R")
source("simulate.R")

#Initialise estimator Class with using GLMnet as propensity learner and GLMnet as mean learner
estimator <- Estimator$new(prp_lrn = GLMNet$new(A ~ w1+w2), mean_lrn = GLM$new(Y ~ w1 + w2 + A))

#Simulate data with theta = 1


#True parameter = 0.370

#Repeat experiment 100 times, each time simulating 1000 data points
theta <- numeric(1000)
for(i in 1:1000){
  sim_data <- simulate_from_model(g,m, theta = 1, n = 500)
  theta[i] <- estimator$fit(sim_data$out_frame, one_step = FALSE, cross_fit = FALSE)
  print(i)
}
library(ggplot2)
#make dataframe of theta values and plot histogram with a vertical line at 0.370 (true effect)
df <- tibble(theta = theta[theta>0])

ggplot(df, aes(x = theta)) + geom_histogram(bins = 20) + geom_vline(xintercept = 0.370, color = "red") + theme_minimal()
