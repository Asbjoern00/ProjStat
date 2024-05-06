# Implements an S6 class called Learner which all implemented learners should derive from

# Define the class
Estimator <- R6::R6Class(
  "Estimator",  # Class name
  public = list(  # Public methods and fields
    prp_lrn = NULL,
    mean_lrn = NULL,
    trt_var_name = NULL,
    ATE = NULL,
    asvar = NULL,
    confint_lwr = NULL,
    confint_upr = NULL,
    initialize = function(prp_lrn, mean_lrn, trt_var_name = "A"){
      self$prp_lrn <- prp_lrn
      self$mean_lrn <- mean_lrn
      self$trt_var_name <- trt_var_name
    },
    fit = function(df, cross_fit = 0, one_step = FALSE){
      # Method to estimate ATE
      if(cross_fit > 0){
        #Split up datasets for crossfitting. Also setup vectors to store ATE and asvar
        split_datasets <- self$split_dataset(df, cross_fit)
        ATE <- numeric(cross_fit)
        asvar <- numeric(cross_fit)
        resp_name <- as.character(self$mean_lrn$formula[2])
        
        for(i in seq(cross_fit)){
          
          #Bind dataset to do fitting of ml-estimators on
          fit_frame <- do.call(rbind, split_datasets[-i])
          
          #Fit mean learner
          self$mean_lrn$fit(fit_frame)
          
          #Fit propensity learner
          self$prp_lrn$fit(dplyr::select(fit_frame, -c(resp_name)))
          
          # Predict propensity scores and conditional means on the left out fold
          #Conditional means
          for_predict <- split_datasets[[i]]
          
          #Keep treatment effect in seperate vector
          trtmt <- for_predict[[self$trt_var_name]]
          
          for_predict[[self$trt_var_name]] <- 0
          for_predict$cond_mean_ctrl <- self$mean_lrn$predict(for_predict)
          
          for_predict[[self$trt_var_name]] <- 1
          for_predict$cond_mean_trt <- self$mean_lrn$predict(for_predict)
          for_predict[[self$trt_var_name]] <- trtmt
          
          #Propensity scores
          for_predict$prop_score <- self$prp_lrn$predict(dplyr::select(for_predict, -c(resp_name, "cond_mean_trt", "cond_mean_ctrl")))
          
          #Estimate ATE
          ATE_lst <- self$computeATE(for_predict, one_step = one_step)
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
        ATE_lst <- self$computeATE(df, one_step = one_step)
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
    computeATE = function(df, one_step = FALSE){
      #df$prop_score <- max(min(df$prop_score ,1-1e-6),1e-6)
      if (one_step){
        #Compute ATE using one-step estimator
        ATE <- mean(df$cond_mean_trt - df$cond_mean_ctrl 
                    +df[[self$trt_var_name]]/df$prop_score*(df$Y - df$cond_mean_trt)
                    - (1-df[[self$trt_var_name]])/(1-df$prop_score)*(df$Y - df$cond_mean_ctrl))
        asvar <- mean(((df[[self$trt_var_name]]/df$prop_score*(df$Y - df$cond_mean_trt) - (1-df[[self$trt_var_name]])/(1-df$prop_score)*(df$Y - df$cond_mean_ctrl)))^2)
        
        return(list(ATE = ATE, asvar = asvar))
      }
      else {
        #Compute ATE using g-formula
        ATE <- mean(df$cond_mean_trt - df$cond_mean_ctrl)
        asvar <- mean(((df[[self$trt_var_name]]/df$prop_score*(df$Y - df$cond_mean_trt) - (1-df[[self$trt_var_name]])/(1-df$prop_score)*(df$Y - df$cond_mean_ctrl)))^2)
        self$ATE <- ATE
        self$asvar <- asvar
        return(list(ATE = ATE, asvar = asvar))
      }
    }
  )
)

