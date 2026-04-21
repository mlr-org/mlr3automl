#' @title Regression k-Nearest-Neighbor Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @export
LearnerRegrAutoKKNN = R6Class(
  "LearnerRegrAutoKKNN",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_kknn", rush = NULL) {
      super$initialize(id = id, learner_ids = "kknn", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_kknn"]] = LearnerRegrAutoKKNN
