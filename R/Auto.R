#' @title Auto Class
#'
#' @description
#' This class is the base class for all autos.
#'
#' @include mlr_auto.R Auto.R
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_memory_limit
#' @template param_large_data_set
#' @template param_size
#' @template param_devices
#' @template param_pv
#' @template param_graph
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

    #' @field devices (`character()`).
    devices = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param properties (`character()`).
    #' @param task_types (`character()`).
    #' @param packages (`character()`).
    #' @param devices (`character()`).
    initialize = function(id,
      properties = character(0),
      task_types = character(0),
      packages = character(0),
      devices = character(0)
      ) {
      self$id = assert_string(id)
      self$properties = assert_character(properties)
      self$task_types = assert_character(task_types)
      self$packages = assert_character(packages)
      self$devices = assert_character(devices)
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE, devices) {
      if (!task$task_type %in% self$task_types) {
       lg$info("Learner '%s' is not compatible with task type '%s'", self$id, task$task_type)
       return(FALSE)
      }
      if (self$estimate_memory(task) > memory_limit) {
        lg$info("Learner '%s' violates the memory limit of %i MB", self$id, ceiling(memory_limit))
        return(FALSE)
      }
      if (large_data_set && !("large_data_sets" %in% self$properties)) {
        lg$info("Learner '%s' is not compatible with large data sets", self$id)
        return(FALSE)
      }
      if (any(devices %nin% self$devices)) {
        lg$info("Learner '%s' is not compatible with devices '%s'", self$id, as_short_string(devices))
        return(FALSE)
      }

      TRUE
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices, pv) {
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
    #' Default hyperparameters for the learner.
    design_default = function(task) {
      xdt = as.data.table(private$.default_values)
      set(xdt, j = "branch.selection", value = self$id)
      xdt
    },

    #' @description
    #' Get the initial hyperparameter set for the learner.
    design_set = function(task, measure, size) {
      assert_task(task)
      assert_measure(measure)
      assert_count(size)

      # read data of best hyperparameters
      file = system.file("ex_data", sprintf("best_%s.csv", self$id), package = "mlr3automl")
      if (!file.exists(file)) {
        # return empty data.table
        return(self$design_default(task)[0])
      }
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
    },

    #' @description
    #' Modify the graph for the final model.
    final_graph = function(graph, task, pv) {
      graph
    }
  ),

  private = list(
    .search_space = ps(),
    .default_values = list()
  )
)

