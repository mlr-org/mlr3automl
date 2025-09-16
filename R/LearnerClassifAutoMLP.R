#' @title Classification MLP Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoMLP = R6Class("LearnerClassifAutoMLP",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_mlp") {
      super$initialize(id = id, learner_ids = "mlp")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_mlp"]] = LearnerClassifAutoMLP
