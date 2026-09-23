#' @title Regression TabFM Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrAutoTabFM`.
#'
#' @templateVar id regr.auto_tabfm
#' @templateVar packages c("mlr3extralearners", "callr")
#' @template example_learner
#'
#' @export
LearnerRegrAutoTabFM = R6Class(
  "LearnerRegrAutoTabFM",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_tabfm", rush = NULL) {
      super$initialize(id = id, learner_ids = "tabfm", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_tabfm"]] = LearnerRegrAutoTabFM
