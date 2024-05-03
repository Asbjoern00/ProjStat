# Implements an S6 class called Learner which all implemented learners should derive from

# Define the class
Learner <- R6::R6Class(
  "Learner",  # Class name
  public = list(  # Public methods and fields
    name = NULL,
    hyperparams = NULL,
    formula = NULL,
    fitted = NULL,
    initialize = function(formula,name=NULL, hyperparams=NULL){
      self$name <- name
      self$hyperparams <- hyperparams
      self$formula <- formula
    },
    fit = function(df) {
      # Method to fit to data
      preprocessed <- private$preprocess(df)
      self$fitted <- private$fitter(preprocessed[["X"]], preprocessed[["y"]])
    },
    predict = function(df){
      # Method to predict on new data
      preprocessed <- private$preprocess(df)
      to_predict <- preprocessed[["X"]]
      predicted <- private$predictor(to_predict)
      return(predicted)
    }
  ),
  private = list(
    fitter = function(X,y){
      #OVERRIDE THIS FUNCTION
      return(0)
    },
    predictor = function(X){
      #OVERRIDE THIS FUNCTION
      return(0)
    },
    preprocess = function(df){
      X <- model.matrix(self$formula, df)
      resp_name <- as.character(self$formula[2]) #get the name of the response vector
      y <- df[[resp_name]]
      return(list(X = X, y = y))
    }
  )
)


