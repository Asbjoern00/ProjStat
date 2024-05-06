source("Learner.R")
library(mgcv)
library(ranger)
options(error=browser)
#Make GLM class that Inherits from the learner class 
GLM <- R6::R6Class(
  "GLM",
  inherit = Learner,
  private = list(
    fitter = function(X,y){
      glm(y~X-1, family = binomial())
    },
    predictor = function(X){
      #I fcking hate this solution
      #browser()
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
      #Consider adding parallel to this if slow
      #doMC::registerDoMC(cores = 5)
      glmnet::cv.glmnet(X, y, family = "binomial", nfolds = 5)
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
  private = list(
    fitter = function(X,y){
      rhs <- as.character(self$formula[3])
      form <- as.formula(paste0("y~", rhs))
      df = as.data.frame(cbind(X,y))
      df <- df[, -which(names(df) == "(Intercept)")]
      arglist <- c(list("formula"= form, data = df, "classification" = TRUE, "keep.inbag" = TRUE), self$hyperparams)
      do.call(ranger::ranger, arglist)
    },
    predictor = function(X){
      #get predictions
      #If data equals training data, return the out of bag predictions
      if(all.equal(X, self$Xtrain)){
        ibcs <- do.call(cbind, self$fitted$inbag.counts)
        pred <- predict(self$fitted, as.data.frame(X), type = "response", predict.all = TRUE)$predictions
        pred[which(ibcs > 0)] <- NA
      }
      else{
        pred <- predict(self$fitted, as.data.frame(X), type = "response", predict.all = TRUE)$predictions
      }
      
      #get probalities by taking the average over columns in pred
      return(rowMeans(pred, na.rm = TRUE))
    }
  )
)

