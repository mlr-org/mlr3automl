#' @title Glmnet Auto
#'
#' @include mlr_auto.R Auto.R
#'
#' @description
#' Glmnet auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_devices
#' @template param_pv
#'
#' @export
AutoGlmnet = R6Class("AutoGlmnet",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "glmnet") {
      super$initialize(id = id,
        properties = character(),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3learners", "glmnet"),
        devices = "cpu"
      )
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices, pv) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      require_namespaces("mlr3learners")

      learner = lrn(sprintf("%s.glmnet", task$task_type), id = "glmnet")

      po("removeconstants", id = "glmnet_removeconstants") %>>%
        po("imputehist", id = "glmnet_imputehist") %>>%
        po("imputeoor", id = "glmnet_imputeoor") %>>%
        po("fixfactors", id = "glmnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "glmnet_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "glmnet_collapse") %>>%
        po("encode", method = "one-hot", id = "glmnet_encode") %>>%
        po("removeconstants", id = "glmnet_post_removeconstants") %>>%
        learner
    }
  ),

  private = list(
    .search_space = ps(
      glmnet.lambda = p_dbl(1e-4, 1e4, logscale = TRUE),
      glmnet.alpha  = p_dbl(0, 1)
    ),

    .default_values = list(
      glmnet.lambda = 0,
      glmnet.alpha = 1
    )
  )
)

mlr_auto$add("glmnet", function() AutoGlmnet$new())


