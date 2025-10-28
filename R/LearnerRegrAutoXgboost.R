#' @title Regression XGBoost Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoXgboost = R6Class("LearnerRegrAutoXgboost",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_xgboost") {
      super$initialize(id = id, learner_ids = "xgboost")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_xgboost"]] = LearnerRegrAutoXgboost


