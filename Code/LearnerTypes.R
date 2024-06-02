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
      #I fcking hate this solution
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
    initialize = function(formula, name=NULL, hyperparams=NULL, oob = FALSE, autotune = FALSE){
      super$initialize(formula, name, hyperparams)
      self$oob <- oob
      self$autotune <- autotune
    }
  ),
  private = list(
    fitter = function(X,y){
      rhs <- as.character(self$formula[3])
      form <- as.formula(paste0("y~", rhs))
      df = as.data.frame(cbind(X,y))
      df <- df[, -which(names(df) == "(Intercept)")]
      if(self$autotune){
        df$y <- as.factor(df$y)
        task <- mlr::makeClassifTask(data = df, target = "y")
        res <- tuneRanger::tuneRanger(task, iters = 25, num.threads = 8)
        res$model$learner.model
      }
      else{
        arglist <- c(list("formula"= form, data = df, "classification" = TRUE, "keep.inbag" = TRUE), self$hyperparams)
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
        #browser()
        #Take means over rows if running a classification forest.
        pred <- predict(self$fitted, as.data.frame(X), predict.all = TRUE)$predictions
      }
      #get probalities by taking the average over columns in pred
      pred <- rowMeans(pred, na.rm = TRUE)
      
      return(pred)
    }
  )
)

# # Note that this is how to make a probability forest work (this is what tune-ranger outputs). Consider doing this for the other RF as well)
#if(self$autotune){
#pred <- predict(self$fitted, as.data.frame(X))$predictions[,"1"]
#}

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
