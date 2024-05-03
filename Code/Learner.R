# Implements an S6 class called Learner which all implemented learners should derive from

# Define the class
Learner <- R6::R6Class(
  "Learner",  # Class name
  public = list(  # Public methods and fields
    name = NULL,
    hyperparams = NULL,
    formula = NULL
    initialize = function(name, hyperparams, formula){
      self$name <- name
      self$hyperaparams <- hyperparams
      self$formula <- formula
    },
    fit = function(X, y) {
      # Method to fit to data
      return(0)
    }
    predict(X) {
      # Method to predict on new data
      return (0)
    }
  ),
  private = list(
    fitter = function(X,y){
      return(0)
    },
    preprocess = function(df){
      return(df)
    }
  )
)


