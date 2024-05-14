library(tidyverse) # for data manipulation
library(SuperLearner) # for ensemble learning
source("Simulator.R") # source the simulator.R file


set.seed(7) # for reproducible results

sl_libs <- c('SL.glmnet', 'SL.ranger', 'SL.earth') # a library of machine learning algorithms (penalized regression, random forests, and multivariate adaptive regression splines)


sim_cov <- function(n = 100){
  W1 <- rbinom(n, size=1, prob=0.2) # binary confounder
  W2 <- rbinom(n, size=1, prob=0.5) # binary confounder
  W3 <- round(runif(n, min=2, max=7)) # continuous confounder
  W4 <- round(runif(n, min=0, max=4)) # continuous confounder
  W <- cbind(W1,W2,W3,W4)
  W
}


#Function that takes output from sim_cov and simulates A using some non-linear function of W
sim_A <- function(W){
  pA <- function(W){
    plogis(-0.2 + 0.2*W[,2] + log(0.1*W[,3]) + 0.3*W[,4] + 0.2*W[,1]*W[,4])
  }
  
  prob_A <- pA(W)
  A <- rbinom(n = nrow(W), size = 1, prob = prob_A)
  list(A = A, pA = pA(W))
}

sim_Y <- function(A,W){
  pY <- function(A,W){
    plogis(-1 + A - 0.1*W[,1] + 0.2*W[,2] + 0.3*W[,3] - 0.1*W[,4] + sin(0.1*W[,2]*W[,4]))
  }
  prob_Y <- pY(A,W)
  Y <- rbinom(n = nrow(W), size = 1, prob = prob_Y)
  list(Y = Y, pY1 = pY(1,W), pY0 = pY(0,W), prob_Y = prob_Y)
}

sim <- Simulator$new(n = 1000, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

sim$simulate()
sim$asvar



generate_data <- function(n){ 
  W1 <- rbinom(n, size=1, prob=0.2) # binary confounder
  W2 <- rbinom(n, size=1, prob=0.5) # binary confounder
  W3 <- round(runif(n, min=2, max=7)) # continuous confounder
  W4 <- round(runif(n, min=0, max=4)) # continuous confounder
  A  <- rbinom(n, size=1, prob= plogis(-0.2 + 0.2*W2 + log(0.1*W3) + 0.3*W4 + 0.2*W1*W4)) # binary treatment depends on confounders
  Y <- rbinom(n, size=1, prob= plogis(-1 + A - 0.1*W1 + 0.2*W2 + 0.3*W3 - 0.1*W4 + sin(0.1*W2*W4))) # binary outcome depends on confounders
  return(tibble(Y, W1, W2, W3, W4, A))
}

n <- 1000
dat_obs <- generate_data(n) # generate a data set with n observations

Y <- dat_obs$Y
W_A <- dat_obs %>% select(-Y) # remove the outcome to make a matrix of predictors (A, W1, W2, W3, W4) for SuperLearner

### Step 1: Estimate Q
Q <- SuperLearner(Y = Y, # Y is the outcome vector
                  X = W_A, # W_A is the matrix of W1, W2, W3, W4, and A
                  family=binomial(), # specify we have a binary outcome
                  SL.library = sl_libs) # specify our superlearner library of LASSO, RF, and MARS
Q_A <- as.vector(predict(Q)$pred) # obtain predictions for everyone using the treatment they actually received
W_A1 <- W_A %>% mutate(A = 1)  # data set where everyone received treatment
Q_1 <- as.vector(predict(Q, newdata = W_A1)$pred) # predict on that everyone-exposed data set
W_A0 <- W_A %>% mutate(A = 0) # data set where no one received treatment
Q_0 <- as.vector(predict(Q, newdata = W_A0)$pred)
dat_tmle <- tibble(Y = dat_obs$Y, A = dat_obs$A, Q_A, Q_0, Q_1)

### Step 2: Estimate g and compute H(A,W)
A <- dat_obs$A
W <- dat_obs %>% select(-Y, -A) # matrix of predictors that only contains the confounders W1, W2, W3, and W4
g <- SuperLearner(Y = A, # outcome is the A (treatment) vector
                  X = W, # W is a matrix of predictors
                  family=binomial(), # treatment is a binomial outcome
                  SL.library=sl_libs) # using same candidate learners; could use different learners

g_w <- as.vector(predict(g)$pred) # Pr(A=1|W)
H_1 <- 1/g_w
H_0 <- -1/(1-g_w) # Pr(A=0|W) is 1-Pr(A=1|W)
dat_tmle <- # add clever covariate data to dat_tmle
  dat_tmle %>%
  bind_cols(
    H_1 = H_1,
    H_0 = H_0) %>%
  mutate(H_A = case_when(A == 1 ~ H_1, # if A is 1 (treated), assign H_1
                         A == 0 ~ H_0))  # if A is 0 (not treated), assign H_0

### Step 3: Estimate fluctuation parameter
glm_fit <- glm(Y ~ -1 + offset(qlogis(Q_A)) + H_A, data=dat_tmle, family=binomial) # fixed intercept logistic regression
eps <- coef(glm_fit) # save the only coefficient, called epsilon in TMLE lit

### Step 4: Update Q's
H_A <- dat_tmle$H_A # for cleaner code in Q_A_update
Q_A_update <- plogis(qlogis(Q_A) + eps*H_A) # updated expected outcome given treatment actually received
Q_1_update <- plogis(qlogis(Q_1) + eps*H_1) # updated expected outcome for everyone receiving treatment
Q_0_update <- plogis(qlogis(Q_0) + eps*H_0) # updated expected outcome for everyone not receiving treatment

### Step 5: Compute ATE
tmle_ate <- mean(Q_1_update - Q_0_update) # mean diff in updated expected outcome estimates

### Step 6: compute standard error, CIs and pvals
infl_fn <- (Y - Q_A_update) * H_A + Q_1_update - Q_0_update - tmle_ate # influence function
tmle_se <- sqrt(var(infl_fn)/nrow(dat_obs)) # standard error
conf_low <- tmle_ate - 1.96*tmle_se # 95% CI
conf_high <- tmle_ate + 1.96*tmle_se
pval <- 2 * (1 - pnorm(abs(tmle_ate / tmle_se))) # p-value at alpha .05

tmle_ate
conf_low
conf_high