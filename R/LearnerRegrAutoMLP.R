#' @title Regression MLP Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoMLP = R6Class("LearnerRegrAutoMLP",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_mlp") {
      super$initialize(id = id, learner_ids = "mlp")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_mlp"]] = LearnerRegrAutoMLP


