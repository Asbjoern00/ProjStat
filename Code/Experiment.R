# Make R6 class for experiment. Should take an instance of Simulator and Estimator as input and the number of simulation to run.
# The experiment should have a method run that runs the simulation and estimation and returns the estimated ATE and variance of the estimator.
# In the run method we should also compute a valid confidence interval for the ATE, as stored in the confint_lwr and confint_upr field of the
# Estimator class.
# It should have a plot method that plots the estimated (sqrt(Simulator$n))*(ATE-True ATE) for each simulation in a histogram along with the
# Asymptotic normal distribution it should follow (i.e. mean zero and variance Simulator$asvar)
if (!("tidyverse" %in% .packages())){
  library(tidyverse)
}

source("Simulator.R")
source("Estimator.R")
source("LearnerTypes.R")
Experiment <- R6::R6Class("Experiment",
                          public = list(
                            n_sims = NULL,
                            sim = NULL,
                            est = NULL,
    ATE = NULL,
    asvar = NULL,
    TrueATE = NULL,
    confint_lwr = NULL,
    confint_upr = NULL,
    cvrg = NULL,
    initialize = function(n_sims, sim, est){
      self$n_sims <- n_sims
      self$sim <- sim
      self$est <- est
    },
    run = function(verbose = TRUE){
      ATEs <- numeric(self$n_sims)
      TrueATEs <- numeric(self$n_sims)
      asvars <- numeric(self$n_sims)
      confint_lwr <- numeric(self$n_sims)
      confint_upr <- numeric(self$n_sims)
      for(i in 1:self$n_sims){
        
        #Consider parallelizing this
        #simulate until we get a valid simulation
        valid_sim <- FALSE
        while(!valid_sim){
          self$sim$simulate()
          if(!any(is.na(self$sim$out_frame))){
            valid_sim <- TRUE
          }
        }
        asvars[i] <- sim$asvar
        TrueATEs[i] <- sim$ATE
        self$est$fit(sim$out_frame)
        ATEs[i] <- self$est$ATE
        confint_lwr[i] <- self$est$confint_lwr
        confint_upr[i] <- self$est$confint_upr
        if (verbose){
          cat(sprintf("Simulation %d done\n", i))
        }
      }
      self$TrueATE <- mean(TrueATEs)
      self$asvar <- mean(asvars)
      self$confint_lwr <- confint_lwr
      self$confint_upr <- confint_upr
      self$ATE <- ATEs
      self$cvrg <- mean((TrueATEs > confint_lwr) * (TrueATEs < confint_upr))
    },
    plot = function(bins = 8){
      data = tibble(ATE = self$ATE)
      for_title <- paste0(self$est$name, " on ", self$sim$n , " observations", " with ", self$n_sims, " simulations")
      ggplot(data) + geom_histogram(aes(x = sqrt(self$sim$n)*(ATE-self$TrueATE), after_stat(density)), bins = bins) + 
        geom_density(aes(x = sqrt(self$sim$n)*(ATE-self$TrueATE), after_stat(density))) + geom_vline(xintercept = 0, color = "red") +
        stat_function(fun = dnorm, args = list(mean = 0, sd =sqrt(self$asvar)), color = "blue") + ylab("Density") + xlab("sqrt(n)*(ATE-ATE_0)") + 
        ggtitle(for_title)  +theme_bw() + theme(plot.title = ggtext::element_textbox_simple(size = 7))
    }
  )
)
