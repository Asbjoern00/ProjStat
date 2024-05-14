# Implements an S6 class called Learner which all implemented learners should derive from

# Define the class
Estimator <- R6::R6Class(
  "OneStepEstimator",  # Class name
  public = list(  # Public methods and fields
    prp_lrn = NULL,
    mean_lrn = NULL,
    trt_var_name = NULL,
    resp_name = NULL,
    cross_fit = NULL,
    one_step = NULL,
    ATE = NULL,
    asvar = NULL,
    confint_lwr = NULL,
    confint_upr = NULL,
    name = NULL,
    initialize = function(prp_lrn, mean_lrn, cross_fit = 0, one_step = TRUE){
      self$prp_lrn <- prp_lrn
      self$mean_lrn <- mean_lrn
      self$trt_var_name <- as.character(prp_lrn$formula[2])
      self$resp_name <- as.character(mean_lrn$formula[2])
      self$cross_fit = cross_fit
      self$one_step = one_step
      if (self$one_step){
        os <- "One-step"
      }
      else{
        os <- "G-formula"
      }
      if (self$cross_fit > 0){
        cf <- paste0("Cross-fit (K = ", self$cross_fit, ") ")
      }
      else{
        cf <- "No cross-fit "
      }
      self$name = paste0(cf, os, " with mean_lrn: ", mean_lrn$name, " and prop_lrn: ", prp_lrn$name)
    },
    fit = function(df){
      # Method to estimate ATE
      if(self$cross_fit > 0){
        #Split up datasets for crossfitting. Also setup vectors to store ATE and asvar
        split_datasets <- self$split_dataset(df, self$cross_fit)
        ATE <- numeric(self$cross_fit)
        asvar <- numeric(self$cross_fit)
        
        for(i in seq(self$cross_fit)){
          
          #Bind dataset to do fitting of ml-estimators on
          fit_frame <- do.call(rbind, split_datasets[-i])
          
          #Fit mean learner
          self$mean_lrn$fit(fit_frame)
          
          #Fit propensity learner
          self$prp_lrn$fit(dplyr::select(fit_frame, -c(self$resp_name)))
          
          # Predict propensity scores and conditional means on the left out fold
          #Conditional means
          for_predict <- split_datasets[[i]]
          
          #Keep treatment effect in seperate vector
          trtmt <- for_predict[[self$trt_var_name]]
          
          for_predict[[self$trt_var_name]] <- 0
          cond_mean_ctrl <- self$mean_lrn$predict(for_predict)
          
          for_predict[[self$trt_var_name]] <- 1
          cond_mean_trt <- self$mean_lrn$predict(for_predict)
          
          for_predict[[self$trt_var_name]] <- trtmt
          for_predict$cond_mean_ctrl <- cond_mean_ctrl
          for_predict$cond_mean_trt <- cond_mean_trt
          
          #Propensity scores
          for_predict$prop_score <- self$prp_lrn$predict(dplyr::select(for_predict, -c(self$resp_name, "cond_mean_trt", "cond_mean_ctrl")))
          
          #Estimate ATE
          ATE_lst <- self$computeATE(for_predict)
          ATE[i] <- ATE_lst$ATE
          asvar[i] <- ATE_lst$asvar
          
        }   
      }
      else {
        #Fit mean learner
        self$mean_lrn$fit(df)
        
        #Do the same as above but with the whole dataset as we are not cross-fitting
        for_predict <- df
        
        for_predict[[self$trt_var_name]] <- 0
        df$cond_mean_ctrl <- self$mean_lrn$predict(for_predict)
        
        for_predict[[self$trt_var_name]] <- 1
        df$cond_mean_trt <- self$mean_lrn$predict(for_predict)
        
        #Fit propensity score if one_step
        # Get name of response, don't want this included in estimation
        resp_name <- as.character(self$mean_lrn$formula[2])
        self$prp_lrn$fit(dplyr::select(df, -c(resp_name,"cond_mean_trt", "cond_mean_ctrl")))
        df$prop_score <- self$prp_lrn$predict(dplyr::select(df, -c(resp_name,"cond_mean_trt", "cond_mean_ctrl")))
        
        #Estimate ATE 
        ATE_lst <- self$computeATE(df)
        ATE <- ATE_lst$ATE
        asvar <- ATE_lst$asvar
      }
      
      self$ATE <- mean(ATE)
      self$asvar <- mean(asvar)
      self$confint_lwr <- self$ATE - 1.96*sqrt(self$asvar)/sqrt(nrow(df))
      self$confint_upr <- self$ATE + 1.96*sqrt(self$asvar)/sqrt(nrow(df))
      
      return(list(ATE = mean(ATE), asvar = mean(asvar)))
      
    },
    split_dataset = function(data, n){
      # Calculate the number of rows in each subset
      subset_size <- ceiling(nrow(data) / n)
      
      # Generate a grouping variable for splitting
      groups <- rep(1:n, each = subset_size, length.out = nrow(data))
      
      # Split the dataset into equal-sized subsets
      split_data <- split(data, groups)
      
      return(split_data)
    },
    computeATE = function(df){
      if (self$one_step){
        #Compute ATE using one-step estimator
        ATE <- mean(df$cond_mean_trt - df$cond_mean_ctrl 
                    +df[[self$trt_var_name]]/df$prop_score*(df[[self$resp_name]] - df$cond_mean_trt)
                    - (1-df[[self$trt_var_name]])/(1-df$prop_score)*(df[[self$resp_name]] - df$cond_mean_ctrl))
        
        
        #Compute asymptotic variance - Check if this is correctly implemented at some point
        asvar <- mean(((df[[self$trt_var_name]]/df$prop_score*(df[[self$resp_name]] - df$cond_mean_trt) - (1-df[[self$trt_var_name]])/(1-df$prop_score)*(df[[self$resp_name]] - df$cond_mean_ctrl)))^2)
        
        return(list(ATE = ATE, asvar = asvar))
      }
      else {
        #Compute ATE using g-formula
        ATE <- mean(df$cond_mean_trt - df$cond_mean_ctrl)
        asvar <- mean(((df[[self$trt_var_name]]/df$prop_score*(df[[self$resp_name]] - df$cond_mean_trt) - (1-df[[self$trt_var_name]])/(1-df$prop_score)*(df[[self$resp_name]] - df$cond_mean_ctrl)))^2)
        self$ATE <- ATE
        self$asvar <- asvar
        return(list(ATE = ATE, asvar = asvar))
      }
    }
  )
)

#Create a TMLE class inheriting from the estimator class but with a different computeATE method
TMLE <- R6::R6Class(
  "TMLE",
  inherit = Estimator,
  public = list(
    initialize = function(prp_lrn, mean_lrn, trt_var_name = "A", cross_fit = 0){
      self$prp_lrn <- prp_lrn
      self$mean_lrn <- mean_lrn
      self$trt_var_name <- as.character(prp_lrn$formula[2])
      self$resp_name <- as.character(mean_lrn$formula[2])
      self$cross_fit = cross_fit
      if (self$cross_fit > 0){
        cf <- paste0("Cross-fit (K = ", self$cross_fit, ") ")
      }
      else{
        cf <- "No cross-fit "
      }
      self$name = paste0(cf,"TMLE with mean_lrn: ", mean_lrn$name, " and prop_lrn: ", prp_lrn$name)
    },
    computeATE = function(df) {
      #Compute ATE using TMLE
      df$cond_mean_obs <- df$cond_mean_trt*df[[self$trt_var_name]] + df$cond_mean_ctrl*(1-df[[self$trt_var_name]])
      df$prop_score <- pmin(pmax(df$prop_score, 0.00001), 0.99999) # keep propenstiy score bounded away from extremes to avoid infinite weights
      df$clever_cov <- df[[self$trt_var_name]]/df$prop_score - (1-df[[self$trt_var_name]])/(1-df$prop_score)
      
      # Estimate epsilon on when regressing the response on the clever covariate offset with cond_mean_obs
      off_setter <- qlogis(pmax(pmin(df$cond_mean_obs, 1-1e-6),0+1e-6))
      fit <- glm(df[[self$resp_name]] ~ df$clever_cov + offset(off_setter) - 1, family = binomial())
      
      if(!fit$converged){
        warning("Targetting in TMLE step did not converge, setting epsilon = 0")
        epsilon <- 0
      }
      else{
        epsilon <- unname(coef(fit))
      }
      #Update estimates
      df$cond_mean_obs <- plogis(qlogis(df$cond_mean_obs) + epsilon*df$clever_cov)
      df$cond_mean_ctrl <- plogis(qlogis(df$cond_mean_ctrl) - epsilon*(1/(1-df$prop_score)))
      df$cond_mean_trt <- plogis(qlogis(df$cond_mean_trt) + epsilon*(1/df$prop_score))
      #Compute ATE
      ATE <- mean(df$cond_mean_trt - df$cond_mean_ctrl)
      
      #compute asymptotic variance
      asvar <- var((df[[self$resp_name]]-df$cond_mean_obs)*df$clever_cov + df$cond_mean_trt - df$cond_mean_ctrl - ATE)

      return(list(ATE = ATE, asvar = asvar))
    }
  ))

