#' @title Tabpfn Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Tabpfn auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#'
#' @export
AutoTabpfn = R6Class("AutoTabpfn",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "tabpfn") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = character(0)
      self$packages = c("mlr3", "mlr3extralearners")
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE) {
      if (task$nrow > 1e5) {
        lg$info("Learner '%s' is not compatible with tasks with more than 10,000 rows", self$id)
        return(FALSE)
      }
      super$check(task, memory_limit, large_data_set)
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      require_namespaces("mlr3extralearners")

      learner = lrn(sprintf("%s.tabpfn", task$task_type), id = "tabpfn")

      set_threads(learner, n_threads)

      po("removeconstants", id = "tabpfn_removeconstants") %>>%
        po("fixfactors", id = "tabpfn_fixfactors") %>>%
        po("encodeimpact", id = "tabpfn_encode") %>>%
        po("removeconstants", id = "tabpfn_post_removeconstants") %>>%
        learner

    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      memory_size = task$nrow * task$ncol * 8 * 10 / 1e6
      lg$info("Tabpfn memory size: %s MB", round(memory_size))
      memory_size
    },

    #' @description
    #' Get the default hyperparameter values.
    default_values = function(task) {
      list()
    },

    #' @description
    #' Get the search space for the auto.
    search_space = function(task) {
      if (task$task_type == "classif") {
        ps(
          tabpfn.n_estimators           = p_int(1, 8),
          tabpfn.softmax_temperature    = p_dbl(0.75, 1.0),
          tabpfn.balance_probabilities  = p_lgl(),
          tabpfn.average_before_softmax = p_lgl()
        )
      } else if (task$task_type == "regr") {
        ps(
          tabpfn.n_estimators           = p_int(1, 8),
          tabpfn.average_before_softmax = p_lgl()
        )
      }
    }
  )
)

mlr_auto$add("tabpfn", function() AutoTabpfn$new())


