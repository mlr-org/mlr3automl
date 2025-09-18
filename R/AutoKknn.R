#' @title Kknn Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Kknn auto.
#'
#' @template param_id
#'
#' @export
AutoKknn = R6Class("AutoKknn",
  inherit = Auto,
  public = list(
    initialize = function(id = "kknn") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = character()
    },

    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

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

    default_values = function(task) {
      list(
        kknn.k = 7L,
        kknn.distance = 2,
        kknn.kernel = "optimal"
      )
    }
  ),

  active = list(
    search_space = function() {
      ps(
        kknn.k = p_int(1L, 50L),
        kknn.distance = p_int(1L, 5L),
        kknn.kernel = p_fct(levels = c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank"))
      )
    }
  )
)

mlr_auto$add("kknn", function() AutoKknn$new())


