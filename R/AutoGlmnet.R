#' @title Glmnet Auto
#'
#' @description
#' Glmnet auto.
#'
#' @template param_id
#'
#' @include mlr_auto.R
#'
#' @export
AutoGlmnet = R6Class("AutoGlmnet",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "glmnet") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = character()
    },

    #' @description
    #' Create the graph for the auto.
    #'
    #' @param task ([mlr3::Task]).
    #' @param measure ([mlr3::Measure]).
    #' @param n_threads (`numeric(1)`).
    #' @param timeout (`numeric(1)`).
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

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
    },

    #' @description
    #' Get the default values for the auto.
    #'
    #' @param task ([mlr3::Task]).
    default_values = function(task) {
      list(
        glmnet.s = 0.01,
        glmnet.alpha = 1
      )
    }
  ),

  active = list(

    #' @field search_space (`ParamSet`).
    search_space = function() {
      ps(
        glmnet.s     = p_dbl(1e-4, 1e4, logscale = TRUE),
        glmnet.alpha = p_dbl(0, 1)
      )
    }
  )
)

mlr_auto$add("glmnet", function() AutoGlmnet$new())


