#' @title Kknn Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Kknn auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#'
#' @export
AutoKknn = R6Class("AutoKknn",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "kknn") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = character()
      self$packages = c("mlr3", "mlr3learners", "kknn")
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      require_namespaces("mlr3learners")

      learner = lrn(sprintf("%s.kknn", task$task_type), id = "kknn")

      po("removeconstants", id = "kknn_removeconstants") %>>%
        po("imputehist", id = "kknn_imputehist") %>>%
        po("imputeoor", id = "kknn_imputeoor") %>>%
        po("fixfactors", id = "kknn_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "kknn_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "kknn_collapse") %>>%
        po("removeconstants", id = "kknn_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Get the default hyperparameter values.
    default_values = function(task) {
      list(
        kknn.k = 7L,
        kknn.distance = 2,
        kknn.kernel = "optimal"
      )
    },

    #' @description
    #' Get the initial hyperparameter set.
    design_set = function(task, measure, size) {
      sample_design_set(task, measure, size, "kknn", self$search_space)
    }
  ),

  active = list(

    #' @field search_space ([paradox::ParamSet]).
    search_space = function(rhs) {
      assert_ro_binding(rhs)
      ps(
        kknn.k = p_int(1L, 100L, logscale = TRUE),
        kknn.distance = p_dbl(1L, 5L),
        kknn.kernel = p_fct(levels = c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank"))
      )
    }
  )
)

mlr_auto$add("kknn", function() AutoKknn$new())

