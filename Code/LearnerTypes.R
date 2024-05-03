source("Learner.R")
source("simulate.R")
library(mgcv)
#Make GLM class that Inherits from the learner class 
GLM <- R6::R6Class(
  "GLM",
  inherit = Learner,
  private = list(
    fitter = function(X,y){
      glm(y ~ X, family = binomial())
    },
    predictor = function(X){
      predict(self$fitted, as.data.frame(X), type = "response")
    }
  )
)

GLMNet <- R6::R6Class(
  "GLMNet",
  inherit = Learner,
  private = list(
    fitter = function(X,y){
      #Consider adding parallel to this if slow 
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

