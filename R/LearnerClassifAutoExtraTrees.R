#' @title Classification Extra Trees Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoExtraTrees = R6Class(
  "LearnerClassifAutoExtraTrees",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_extra_trees", rush = NULL) {
      super$initialize(id = id, learner_ids = "extra_trees", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_extra_trees"]] = LearnerClassifAutoExtraTrees
