source("Learner.R")
library(mgcv)
library(ranger)
#Make GLM class that Inherits from the learner class 
GLM <- R6::R6Class(
  "GLM",
  inherit = Learner,
  private = list(
    fitter = function(X,y){
      if(is.null(self$hyperparams)){
        arglist <- c(list("formula"= y~X-1, "family"= binomial()))
      }
      else{
        arglist <- c(list("formula"= y~X-1, "family"= binomial(link = self$hyperparams$customlink)))
      }
      fitted <- do.call(glm, arglist)
      return(fitted)
    },
    predictor = function(X){
      new_data <- data.frame(X = I(X))
      predict(self$fitted, newdata = new_data, type = "response")
    }
  )
)

GLMNet <- R6::R6Class(
  "GLMNet",
  inherit = Learner,
  private = list(
    fitter = function(X,y){
      #Consider adding parallel to this if slow. This is actually the LASSO objective. Specify otherwise to get ridge
      doMC::registerDoMC(cores = 5)
      glmnet::cv.glmnet(X, y, family = "binomial", nfolds = 5,parallel = TRUE)
    },
    predictor = function(X){
      as.vector(predict(self$fitted, X, type = "response"))
    }
  )
)

GAM <- R6::R6Class(
  "GAM",
  inherit = Learner,
  public = list(
    fit = function(df){
      #Override the fit function for GAM as it constructs it's own modelmatrix in C++
      self$fitted <- gam(self$formula, family = binomial(), data = df)
    },
    predict = function(df){
      predict(self$fitted, df, type = "response")
    }
  )
)
RF <- R6::R6Class(
  "RF",
  inherit = Learner,
  public = list(
    oob = NULL,
    autotune = NULL,
    cv = NULL,
    initialize = function(formula, name=NULL, hyperparams=NULL, oob = FALSE, autotune = FALSE, cv = FALSE){
      super$initialize(formula, name, hyperparams)
      self$oob <- oob
      self$autotune <- autotune
      self$cv <- cv
    }
  ),
  private = list(
    fitter = function(X,y){
      rhs <- as.character(self$formula[3])
      form <- as.formula(paste0("y~", rhs))
      df = as.data.frame(cbind(X,y))
      df <- df[, which(names(df) != "(Intercept)")]
      if(self$autotune){
        mtrys <- c(ceiling(sqrt(ncol(df)-1)-2),ceiling(sqrt(ncol(df)-1)-1),ceiling(sqrt(ncol(df)-1)),ceiling(sqrt(ncol(df)-1)+1))
        minnodesizes <- c(ceiling(1/10*nrow(df)), ceiling(nrow(df)*1/100), ceiling(nrow(df)/200), 3)
        # Make list of cartesian product of mtrys and minnodesizes
        hyperparams <- expand.grid(mtrys, minnodesizes)
        names(hyperparams) <- c("mtry", "min.node.size")
        models <- vector(mode = "list", length = nrow(hyperparams))
        model_loss <- numeric(nrow(hyperparams))
        if(self$cv){
          #split data into 5 folds
          split_data <- private$split_dataset(df, 5)
          for(i in 1:nrow(hyperparams)){
            model_loss[i] <- 0 
            for(j in 1:5){
              #get training data
              train_data <- do.call(rbind, split_data[-j])
              #get test data
              test_data <- split_data[[j]]
              arglist <- c(list("formula"= form, data = train_data, "keep.inbag" = TRUE, "mtry" = hyperparams[i,"mtry"],
                                "min.node.size" = hyperparams[i,"min.node.size"]), self$hyperparams)
              models[[i]] <- do.call(ranger::ranger, arglist)
              
              model_loss[i] <- model_loss[i] + mean((test_data$y -predict(models[[i]], test_data)$predictions)^2)/5
            }
          }
          best_model_idx <- which.min(model_loss)
          arglist <- c(list("formula"= form, data = df, "keep.inbag" = TRUE, "mtry" = hyperparams[best_model_idx,"mtry"],
                            "min.node.size" = hyperparams[best_model_idx,"min.node.size"]), self$hyperparams)
          do.call(ranger::ranger, arglist)
        }
        else{
          for(i in 1:nrow(hyperparams)){
            arglist <- c(list("formula"= form, data = df, "keep.inbag" = TRUE, "mtry" = hyperparams[i,"mtry"],
                              "min.node.size" = hyperparams[i,"min.node.size"]), self$hyperparams)
            models[[i]] <- do.call(ranger::ranger, arglist)
            model_loss[i] <- models[[i]]$prediction.error
          }
          best_model_idx <- which.min(model_loss)
          models[[best_model_idx]] 
        }
        
      }
      else{
        arglist <- c(list("formula"= form, data = df, "keep.inbag" = TRUE), self$hyperparams)
        do.call(ranger::ranger, arglist) 
      }
    },
    predictor = function(X){
      #get predictions
      if(self$oob){
        #bind number of inbag counts to matrix
        ibcs <- do.call(cbind, self$fitted$inbag.counts)
        # get indices of rows that are inbag
        idx <- which(ibcs > 0)
        #get predictions
        pred <- predict(self$fitted, as.data.frame(X), type = "response", predict.all = TRUE)$predictions
        #set values to NA for rows that are inbag
        pred[idx] <- NA
      }
      else{
        #Take means over rows if running a classification forest.
        pred <- predict(self$fitted, as.data.frame(X), predict.all = TRUE)$predictions
      }
      #get probalities by taking the average over columns in pred
      pred <- rowMeans(pred, na.rm = TRUE)
      
      return(pred)
    },
    split_dataset = function(data, n){
      # Calculate the number of rows in each subset
      subset_size <- ceiling(nrow(data) / n)
      
      # Generate a grouping variable for splitting
      groups <- rep(1:n, each = subset_size, length.out = nrow(data))
      
      # Split the dataset into equal-sized subsets
      split_data <- split(data, groups)
      
      return(split_data)
    }
  )
)


#Implement a learner of the Highly Adaptive Lasso (HAL) type
HAL <- R6::R6Class(
  "HAL",
  inherit = Learner,
  private = list(
    fitter = function(X,y){
      doMC::registerDoMC(cores = 5)
      arglist <- c(list("X"= X, "Y" = y, "family" = "binomial",
                   "fit_control" = list(nfolds = 5, parallel = TRUE)), self$hyperparams)
      do.call(hal9001::fit_hal, arglist)
    },
    predictor = function(X){
      as.vector(predict(self$fitted, X, type = "response"))
    }
  )
)
