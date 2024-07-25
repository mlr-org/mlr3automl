#' @title Regression Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#'
#' @export
LearnerRegrAuto = R6Class("LearnerRegrAuto",
  inherit = Learner,
  public = list(

    #' @field graph ([mlr3pipelines::Graph]).
    graph = NULL,

    #' @field tuning_space (`list()`).
    tuning_space = NULL,

    #' @field instance ([mlr3tuning::TuningInstanceAsyncSingleCrit]).
    instance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto") {

    }
  ),
  private = list(
    .train = function(task) {

    },

    .predict = function(task) {

    }
  )
)

#' @include aaa.R
learners[["regr.auto"]] = LearnerRegrAuto
