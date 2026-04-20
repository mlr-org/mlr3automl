#' @title Classification XGBoost Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoXgboost = R6Class("LearnerClassifAutoXgboost",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_xgboost", rush = NULL) {
      super$initialize(id = id, learner_ids = "xgboost", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_xgboost"]] = LearnerClassifAutoXgboost
