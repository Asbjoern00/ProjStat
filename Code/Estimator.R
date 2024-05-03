# Implements an S6 class called Learner which all implemented learners should derive from

# Define the class
Estimator <- R6::R6Class(
  "Estimator",  # Class name
  public = list(  # Public methods and fields
    prp_lrn = NULL,
    mean_lrn = NULL,
    initialize = function(prp_lrn, mean_lrn){
      self$prp_lrn <- prp_lrn
      self$mean_lrn <- mean_lrn
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
          self$prp_lrn$fit(fit_frame)
          self$mean_lrn$fit(fit_frame)
          
          #Get dataset to compute the nonparametric ATE on 
          to_predict <- split_datasets[i]
          
          # predict on dataset
          to_predict$prop_score <- self$prp_lrn$predict(to_predict)
          to_predict$cond_mean <- self$mean_lrn$predict(to_predict)
          ATE[i] <- self$computeATE(to_predict)
        }
      }
      else {
        #Fit learners
        df$prop_score <- self$prp_lrn$fit(df)
        df$cond_mean <- self$mean_lrn$fit(df)
        
        #Estimate ATE 
        ATE <- self$computeATE(df)
      }
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
      return(0)
    }
  )
)

