#' @title Auto Class
#'
#' @description
#' This class is the base class for all autos.
#'
#' @include mlr_auto.R
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_memory_limit
#' @template param_large_data_set
#' @template param_size
#'
#' @export
Auto = R6Class("Auto",
  public = list(

    #' @field id (`character(1)`).
    id = NULL,

    #' @field properties (`character()`).
    properties = NULL,

    #' @field task_types (`character()`).
    task_types = NULL,

    #' @field packages (`character()`).
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id) {
      self$id = assert_string(id)
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE) {
      if (!task$task_type %in% self$task_types) {
       lg$info("Learner '%s' is not compatible with task type '%s'", self$id, task$task_type)
       return(FALSE)
      }
      if (self$estimate_memory(task) > memory_limit) {
        lg$info("Learner '%s' violates the memory limit of %i MB", self$id, memory_limit)
        return(FALSE)
      }
      if (large_data_set && !("large_data_sets" %in% self$properties)) {
        lg$info("Learner '%s' is not compatible with large data sets", self$id)
        return(FALSE)
      }
      TRUE
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout) {
      stop("Abstract")
    },

    #' @description
    #' Estimate the number of early stopping rounds.
    early_stopping_rounds = function(task) {
      min_early_stopping_rounds = 20L
      max_early_stopping_rounds = 200L

      if (task$nrow < 1e4) return(max_early_stopping_rounds)

      floor((max(min_early_stopping_rounds, 1e4 / task$nrow * max_early_stopping_rounds)))
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      -Inf
    },

    #' @description
    #' Get the default values for the auto.
    default_values = function(task) {
      stop("Abstract")
    },

    #' @description
    #' Default hyperparameters for the learner.
    design_default = function(task) {
      default_values =
      xdt = as.data.table(private$.default_values)
      set(xdt, j = "branch.selection", value = self$id)
      xdt
    },

    #' @description
    #' Generate random design for the learner.
    design_random = function(task, size) {
      assert_task(task)
      assert_count(size)

      internal_tune_ids = self$search_space(task)$ids(any_tags = "internal_tuning")
      xdt = if (self$search_space(task)$length) {
        generate_design_random(self$search_space(task), size)$data
      } else {
        data.table()
      }

      set(xdt, j = "branch.selection", value = self$id)

      lg$info("Learner '%s' random design size: %i", self$id, nrow(xdt))

      xdt[, setdiff(c("branch.selection", self$search_space(task)$ids()), internal_tune_ids), with = FALSE]
    },

    #' @description
    #' Generate lhs design for the learner.
    design_lhs = function(task, size) {
      assert_task(task)
      assert_count(size)

      internal_tune_ids = self$search_space(task)$ids(any_tags = "internal_tuning")
      xdt = if (self$search_space(task)$length) {
        generate_design_lhs(self$search_space(task), n = size)$data
      } else {
        data.table()
      }

      set(xdt, j = "branch.selection", value = self$id)

      lg$info("Learner '%s' lhs design size: %i", self$id, nrow(xdt))

      xdt[, setdiff(c("branch.selection", self$search_space(task)$ids()), internal_tune_ids), with = FALSE]
    },

    #' @description
    #' Get the initial hyperparameter set for the learner.
    design_set = function(task, measure, size) {
      assert_task(task)
      assert_measure(measure)
      assert_count(size)

      # read data of best hyperparameters
      file = system.file("ex_data", sprintf("best_%s.csv", self$id), package = "mlr3automl")
      if (!file.exists(file)) return(data.table())
      data = fread(file)

      # exclude tasks
      exclude_tasks = getOption("mlr3automl.exclude_tasks", "")
      if (any(exclude_tasks %in% data$task)) {
        lg$info("Excluding tasks from initial design: %s", as_short_string(exclude_tasks[exclude_tasks %in% data$task]))
        data = data[task %nin% exclude_tasks]
      }

      # subset to relevant measure
      measure_id = sub(sprintf("^%s\\.", task$task_type), "", measure$id)
      measure_id = if (measure_id %in% data$measure) measure_id else "mcc"
      data = data[measure_id, , on = "measure"]

      # subset to relevant parameters
      param_ids = self$search_space(task)$ids()
      param_internal_ids = self$search_space(task)$ids(any_tags = "internal_tuning")
      param_ids = setdiff(param_ids, param_internal_ids)
      data = data[, param_ids, with = FALSE]

      xdt = data[sample(nrow(data), min(size, nrow(data)))]
      set(xdt, j = "branch.selection", value = self$id)

      lg$info("Learner '%s' set design size: %i", self$id, nrow(xdt))

      xdt
    },

    #' @description
    #' Get the search space for the learner.
    search_space = function(task) {
      private$.search_space
    }
  ),

  private = list(
    .search_space = ps(),
    .default_values = list()
  )
)
