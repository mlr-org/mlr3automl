#' @title Regression Extra Trees Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @export
LearnerRegrAutoExtraTrees = R6Class(
  "LearnerRegrAutoExtraTrees",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_extra_trees", rush = NULL) {
      super$initialize(id = id, learner_ids = "extra_trees", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_extra_trees"]] = LearnerRegrAutoExtraTrees
