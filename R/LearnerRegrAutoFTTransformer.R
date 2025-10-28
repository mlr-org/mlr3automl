#' @title Regression FT-Transformer Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoFTTransformer = R6Class("LearnerRegrAutoFTTransformer",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_ft_transformer") {
      super$initialize(id = id, learner_ids = "ft_transformer")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_ft_transformer"]] = LearnerRegrAutoFTTransformer


