# Implements an S6 class called Learner which all implemented learners should derive from

# Define the class
Estimator <- R6::R6Class(
  "Estimator",  # Class name
  public = list(  # Public methods and fields
    prp_lrn = NULL,
    mean_lrn = NULL,
    trt_var_name = NULL,
    initialize = function(prp_lrn, mean_lrn, trt_var_name = "A"){
      self$prp_lrn <- prp_lrn
      self$mean_lrn <- mean_lrn
      self$trt_var_name <- trt_var_name
    },
    fit = function(df, cross_fit = 0, one_step = FALSE) {
      # Method to estimate ATE
      if(cross_fit > 0){
        split_datasets <- self$split_dataset(df, cross_fit)
        ATE <- numeric(cross_fit)
        for(i in seq(cross_fit)){
          
          #Bind dataset to do fitting of ml-estimators on on
          fit_frame <- do.call(rbind, split_datasets[-i])
          
          #Fit learners
          #Only need to fit the propensity learner if we are using one-step estimator
          if (one_step){
            self$prp_lrn$fit(fit_frame)
          }
          
          self$mean_lrn$fit(fit_frame)
          
          #Get dataset to compute the nonparametric ATE on 
          to_predict <- split_datasets[i]
          
          # Compute propensity scores if one_step is true. Dont need it otherwise
          if (one_step){
            to_predict$prop_score <- self$prp_lrn$predict(to_predict)
          }
          
          # Make two new datasets, each having treatment set to 0 or 1 for getting the estimated conditional mean
          # Keep original dataset for computing ATE
          trt <- to_predict[[self$trt_var_name]]
          
          to_predict[[self$trt_var_name]] <- 0
          to_predict$cond_mean_ctrl <- self$mean_lrn$predict(to_predict)
          
          to_predict[[self$trt_var_name]] <- 1
          to_predict$cond_mean_trt <- self$mean_lrn$predict(to_predict)
          
          to_predict[[self$trt_var_name]] <- trt
          
          ATE[i] <- self$computeATE(to_predict, one_step = one_step)
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
        if (one_step){
          # Get name of response, don't want this included in estimation
          resp_name <- as.character(self$mean_lrn$formula[2])
          self$prp_lrn$fit(dplyr::select(df, -c(resp_name,"cond_mean_trt", "cond_mean_ctrl")))
          df$prop_score <- self$prp_lrn$predict(dplyr::select(df, -c(resp_name,"cond_mean_trt", "cond_mean_ctrl")))
        }
        
        
        #Estimate ATE 
        ATE <- self$computeATE(df, one_step = one_step)
        ATE <- min(max(ATE,-1),1) #Make sure ATE is between 1 and -1
      }
      return(mean(ATE))
      
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
      if (one_step){
        #Compute ATE using one-step estimator
        ATE <- mean(df$cond_mean_trt - df$cond_mean_ctrl 
                    +df[[self$trt_var_name]]/df$prop_score*(df$Y - df$cond_mean_trt)
                    - (1-df[[self$trt_var_name]])/(1-df$prop_score)*(df$Y - df$cond_mean_ctrl))
      }
      else {
        #Compute ATE using g-formula
        ATE <- mean(df$cond_mean_trt - df$cond_mean_ctrl)
      }
    }
  )
)

