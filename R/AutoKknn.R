#' @title Kknn Auto
#'
#' @include mlr_auto.R Auto.R
#'
#' @description
#' Kknn auto.
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
AutoKknn = R6Class("AutoKknn",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "kknn") {
      super$initialize(id = id,
        properties = character(),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3learners", "kknn"),
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

      learner = lrn(sprintf("%s.kknn", task$task_type), id = "kknn", kernel = "optimal")

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
    #' Get the search space for the auto.
    search_space = function(task) {
      assert_task(task)
      ps(
        # k must be less than the number of rows
        kknn.k = p_int(1L, min(100L, task$nrow - 1L), logscale = TRUE),
        kknn.distance = p_dbl(1L, 5L)
      )
    }
  ),

  private = list(
    .default_values = list(
      kknn.k = 7L,
      kknn.distance = 2
    )
  )
)

mlr_auto$add("kknn", function() AutoKknn$new())

