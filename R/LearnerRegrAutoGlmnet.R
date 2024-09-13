#' @title Regression GLM with Elastic Net Regularization Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoGlmnet = R6Class("LearnerRegrAutoGlmnet",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_glmnet") {
      super$initialize(id = id, learner_ids = "glmnet")

      # reduce parameter set to the relevant parameters
      private$.param_set = private$.param_set$subset(
        c("learner_ids",
        "learner_timeout",
        "small_data_size",
        "small_data_resampling",
        "max_cardinality",
        "resampling",
        "terminator",
        "measure",
        "lhs_size",
        "callbacks",
        "store_benchmark_result")
      )

      self$param_set$set_values(learner_ids = "glmnet")
      self$packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", "glmnet")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_glmnet"]] = LearnerRegrAutoGlmnet
