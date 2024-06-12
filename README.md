This git repo contains the code for my project in statistics in Targeted Learning at the University of Copenhagen. 

# Prerequisites
The code is written in `R` and requires the following installations
```{r}
install.packages(c("hal9001",
                   "glmnet",
                   "tidyverse",
                   "ranger",
                   "latex2exp",
                   "doMC"))
```
We implement a flexible `R6`-class system for estimation of the average treatment effect using TMLE, CV-TMLE and UPCV-TMLE (and actually also a one-step estimator with and without cross-fitting not discussed in the project) with a variety of different estimator configurations as described in the project. 

# Nuisance Function Estimation

The file `Learner.R` implements
a `R6` base class named `Learner` for nuisance function estimators. All nuisance function estimators should inherit from this class by overiding the private methods `predictor` and `fitter`. The `Learner` class takes a required formula argument which
should be a `formula` object specyfing via `R`'s `~` the response on the left hand side and a model matrix on the right hand side as known from the standard `lm`/`glm` syntax. As an example suppose we want to model a response which in our data is named `Y` by considering all second order interaction terms between covariates we would do
```{r}
learn_y <- Learner$new(Y ~ .^2) # Can not be used for fitting as base class does not have proper fitter and predictor methods.
#Can however construct the model matrix specified by formula given data.
#The preprocess method should not be overriden by inheriting classes
# generate some data. Illustrative, Y is independent from covariates
df <- tibble(Y = rnorm(100), w1 = rnorm(100,1,0.5), w2 = rbinom(100,1,0.5),
             w3 = rbinom(100,1,0.5), A = rbinom(100,1,0.5))
preprocessed <- learn_y$.__enclos_env__$private$preprocess(df)
# Response vector. This is named y irrespective of the formula used.
# Response name stored in as.character(learn_y$formula[2])
preprocessed$y
# Model matrix
preproccesed$X 
```
Specfic estimators used in the project, i.e. (OOB) random forests, (Regularized) GLMs, and the highly adaptive LASSO are implemented in the `LearnerTypes.R` file by wrapping the `Learner` base class on top of their corresponding fitting libraries. Thus,
say we are constructing a random forest which is implemeted in the `RF` class. In the same context as above we construct, fit and predict from a random forest as
```{r}
library(ranger)
RF$new(Y~ .^2) #Intialize
RF$fit(df) #Fit
RF$fitted #The fitted `ranger` object
RF$predict(df) #Predicted values of df
```
In addition, as we are fitting the object via the `ranger` package we can specify additional arguments to the `ranger` function to override default hyperparameter settings. I.e if we want to override the default `num.trees` argument we can do
```
RF$new(Y~ .^2) # This has num.trees = 500 as is default in ranger
RF$new(Y~ .^2, hyperparams = list("num.trees" = 1000)) # This has num.trees = 1000 when fitting
```
We highlight that in addition to passing hyperparameters to the fitting function, specifically for random forests we have implemented additional arguments on top of `ranger` to be able to tune the random forest via the OOB MSE and cross-validation
via the hyperparameter grid discussed in the project. Specifically
```{r}
RF$new(Y~ .^2) #Random forest with default ranger hyperparameters
RF$new(Y~ .^2, autotune = TRUE, cv = FALSE) #Random forest tuned via oob mse
RF$new(Y~ .^2, autotune = TRUE, cv = TRUE) #Random forest tuned via cross-validation
```
Moreover we implement the OOB estimator on top of `ranger`. We can specify an OOB random forest as
```{r}
RF$new(Y~ .^2, oob = TRUE) #OOB Random forest with default ranger hyperparameters. Can be combined with arbitrary tuning as above.
```

# ATE Estimation
When we have constructed nuisance function estimators we can use these to construct an ATE estimator. We implemtent these estimators in the `Estimator.R` file. Each of these takes 3 arguments. `prp_lrn` which is a `Learner` instance for estimating the propensity score, `mean_lrn` which is a `Learner` instance for estimating the outcome model and `cross_fit` which indicates whether to cross-fit.
For instance the following (CV)-TMLE for estimating the ATE using the HAL estimator for propensity score and LASSO for the outcome model both with a main terms model matrix for our running example.
```{r}
propensity <- HAL$new(A ~ .) # Note that Y is automatically removed from the formula in later steps
outcome <- GLMNet$new(Y ~ .)
tmle_est <-TMLE$new(prp_lrn = propensity, mean_lrn = outcome, cross_fit = 0) # TMLE
cvtmle_est <-TMLE$new(prp_lrn = propensity, mean_lrn = outcome, cross_fit = 5) # 5-fold CV-TMLE
upcvtmle_est <-UPTMLE$new(prp_lrn = propensity, mean_lrn = outcome, cross_fit = 5) # 5-fold UPCV-TMLE

tmle_est$fit(df) # Fit TMLE
TMLE$ATE # estimated ate
TMLE$asvar # estimated asymptotic variance
c(TMLE$confint_lwr,TMLE$confint_upr) # Confidence interval
```
# Simulation Implementation

In addition to nuisance function and ATE estimators we also implement two `R6` class for simulation studies. The `Simulator` class is implemented in the `Simulator.R` file. The constructor for the simulator class takes four arguments that jointly describe the data generating proces.
- `n`: Integer specyfing the number of samples to draw.
- `sim_cov`: Function taking an integer and output a matrix of covariates. Referred to as `W` in the following
- `sim_A` : Function taking output from `sim_cov`, `W`, and outputting a list containing `A` indicating the treatment variable vector and `pA` which is the conditional probability of receving treatment given covariates for each sample.
- `sim_Y`: Function taking `A` and `W` and outputting a list containing `Y`, the outcome variable vector, `pY1`, the conditional probability of outcome given covariates and `A=1` and similarly for `pY0`.

When calling the `simulate` method of the `Simulator` class, a dataset of size `n` is generated, and based on this the ATE is computed as `mean(pY1-pY0)` and analogously the asymptotic variance of an efficient estimator is computed via the efficient influence function.

Once we have specified an instance of the `Simulator` class and an instance of an estimator, say `TMLE`, we can pass these to an instance of the `Experiment` class implemented in the `Experiment.R` file. The `run` method of the `Experiment` class 
first simulates data from the `Simulator` instance and then estimates ATE via the `TMLE` instance. In addition to these two arguments the `Experiment` constructor takes the argument `nsim` indicating how many times to repeat a given simulation setup. ATE's and asymptotic variances are averaged over all simulations.
A complete run of the simulation study of UPCV-TMLE versus CV-TMLE as described in the report thus looks like:

```{r}
#Simulates W
sim_cov <- function(n){
  w1 <- rnorm(1, n = n)
  w2 <- rbinom(n,1,0.65)*w1
  w3 <- rnorm(-1,n = n)
  w4 <- rnorm(1, n = n)
  W <- cbind(w1,w2,w3,w4)
  W
}
#Simulates A
sim_A <- function(W){
  pA <- function(W){
    plogis(W[,1] - 2*W[,2] + 0.5*W[,3])
  }
  prob_A <- pA(W)
  A <- rbinom(n = nrow(W), size = 1, prob = prob_A)
  list(A = A, pA = prob_A)
}
#Simulates Y
sim_Y <- function(A,W){
  pY <- function(A,W){
    plogis(3*A + -1*W[,4] - 3*W[,3])
  }
  prob_Y <- pY(A,W)
  Y <- rbinom(n = nrow(W), size = 1, prob = prob_Y)
  list(Y = Y, pY1 = pY(1,W), pY0 = pY(0,W), pY = prob_Y)
}

nsim <- 500 # Number of simulations
n <- 500 # Number of samples in each dataframe
set.seed(123) # seed for reproducability
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y) # simulator
prp_corr_spec <- GLM$new(A~w1+w2+w3-1, name = "GLM prp") # Correctly specified GLM for propensity
mean_corr_spec <- GLM$new(Y~A + w4 + w3 -1, name = "GLM prp") # # Correctly specified GLM for outcome

# Estimator configurations
exps <- list(
  Experiment$new(sim = sim, est = UPTMLE$new(prp_lrn = prp_corr_spec, mean_lrn = mean_corr_spec,cross_fit = 50), n_sim = nsim),
  Experiment$new(sim = sim, est = UPTMLE$new(prp_lrn = prp_corr_spec, mean_lrn = mean_corr_spec,cross_fit = 2), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_corr_spec, mean_lrn = mean_corr_spec,cross_fit = 50), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_corr_spec, mean_lrn = mean_corr_spec,cross_fit = 2), n_sim = nsim)
)

# For loop to run all experiments
for(i in 1:length(exps)){
  exps[[i]]$run()
}
exp[[1]]$ATE # ATE's for experiment 1
exp[[1]]$confint_lwr # Lower CI endpoints
exp[[1]]$confint_upr # Upper CI endpoints
exp[[1]]$cvrg # CI coverage
exp[[1]]$plotdist() # plot distribution of estimated ATE's
exp[[1]]$plotci() # plot confidence intervals
```
