#' @title Classification TabPFN Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoTabPFN = R6Class("LearnerClassifAutoTabPFN",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_tabpfn", rush = NULL) {
      super$initialize(id = id, learner_ids = "tabpfn", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_tabpfn"]] = LearnerClassifAutoTabPFN


