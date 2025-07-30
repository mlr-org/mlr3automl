#' @title Regression TabPFN Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoTabPFN = R6Class("LearnerRegrAutoTabPFN",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_tabpfn") {
      super$initialize(id = id, learner_ids = "tabpfn")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_tabpfn"]] = LearnerRegrAutoTabPFN
