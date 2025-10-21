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
#' @template param_memory_limit
#' @template param_large_data_set
#'
#' @export
AutoTabpfn = R6Class("AutoTabpfn",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "tabpfn") {
      super$initialize(id = id,
        properties = character(0),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3extralearners"),
        devices = c("cpu", "cuda")
      )
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE, devices = "cpu") {
      ok = check_python_packages(c("fastai", "torch"))
      if (!isTRUE(ok)) {
        lg$info(ok)
        lg$info("Remove tabpfn from search space")
        return(FALSE)
      }

      if ("cuda" %nin% devices && task$nrow > 1e3) {
        lg$info("Learner '%s' is not compatible with tasks with more than 1,000 rows when using 'cpu' as device", self$id)
        return(FALSE)
      }
      super$check(task, memory_limit, large_data_set, devices)
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)
      assert_subset(devices, self$devices)

      require_namespaces("mlr3extralearners")

      device = if ("cuda" %in% devices) "cuda" else "cpu"

      learner = lrn(sprintf("%s.tabpfn", task$task_type), id = "tabpfn", device = device)

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
      ceiling(memory_size)
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
  ),

  private = list(
    .default_values = list(
      tabpfn.n_estimators = 4L,
      tabpfn.softmax_temperature = 1.0,
      tabpfn.balance_probabilities = FALSE,
      tabpfn.average_before_softmax = FALSE
    )
  )
)

mlr_auto$add("tabpfn", function() AutoTabpfn$new())


