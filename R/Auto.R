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
#' @template param_devices
#'
#' @return Object of class [R6::R6Class] and `Auto`.
#'
#' @export
Auto = R6Class(
  "Auto",
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

    #' @field n_cpu (`integer(1)`)\cr
    #' Number of CPUs a single training of the learner uses.
    n_cpu = NULL,

    #' @field n_gpu (`integer(1)`)\cr
    #' Number of GPUs a single training of the learner uses.
    n_gpu = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param properties (`character()`).
    #' @param task_types (`character()`).
    #' @param packages (`character()`).
    #' @param devices (`character()`).
    #' @param n_cpu (`integer(1)`).
    #' @param n_gpu (`integer(1)`).
    initialize = function(
      id,
      properties = character(0),
      task_types = character(0),
      packages = character(0),
      devices = character(0),
      n_cpu = 1L,
      n_gpu = 0L
    ) {
      self$id = assert_string(id)
      self$properties = assert_character(properties)
      self$task_types = assert_character(task_types)
      self$packages = assert_character(packages)
      self$devices = assert_character(devices)
      self$n_cpu = assert_int(n_cpu, lower = 1L, coerce = TRUE)
      self$n_gpu = assert_int(n_gpu, lower = 0L, upper = 1L, coerce = TRUE)
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE, devices) {
      assert_task(task)
      assert_number(memory_limit)
      assert_flag(large_data_set)
      assert_character(devices)

      # find.package() checks installation without loading the package.
      missing_packages = self$packages[
        !map_lgl(self$packages, function(package) {
          length(find.package(package, quiet = TRUE)) > 0L
        })
      ]
      if (length(missing_packages)) {
        lg$info("Learner '%s' is not available. Missing packages: %s", self$id, str_collapse(missing_packages))
        return(FALSE)
      }

      if (!task$task_type %in% self$task_types) {
        lg$info("Learner '%s' is not compatible with task type '%s'", self$id, task$task_type)
        return(FALSE)
      }
      if (self$estimate_memory(task, devices) > memory_limit) {
        lg$info("Learner '%s' violates the memory limit of %i MB", self$id, ceiling(memory_limit))
        return(FALSE)
      }
      if (large_data_set && !("large_data_sets" %in% self$properties)) {
        lg$info("Learner '%s' is not compatible with large data sets", self$id)
        return(FALSE)
      }
      if (!any(devices %in% self$devices)) {
        lg$info("Learner '%s' is not compatible with devices '%s'", self$id, as_short_string(devices))
        return(FALSE)
      }

      TRUE
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices) {
      stop("Abstract")
    },

    #' @description
    #' Create the bagged graph for the auto.
    #' Wraps the graph of `$graph()` in a [PipeOpLearnerBagged],
    #' so a configuration is evaluated as `folds * repeats` cross-validated child models
    #' and scored with their out-of-fold predictions.
    #'
    #' In contrast to `$graph()`, `timeout` is the timeout of the complete configuration.
    #' It is divided among the child models.
    #'
    #' @param folds (`integer(1)`)\cr
    #'   Number of cross-validation folds.
    #' @param repeats (`integer(1)`)\cr
    #'   Number of repetitions of the cross-validation.
    graph_bagged = function(task, measure, n_threads, timeout, devices, folds, repeats = 1L) {
      assert_int(folds, lower = 2L)
      assert_int(repeats, lower = 1L)

      # a configuration trains `folds * repeats` children, so each child receives a share of the timeout
      fit_timeout = max(1L, timeout %/% (folds * repeats))

      search_space = self$search_space(task)
      internal_ids = search_space$ids(any_tags = "internal_tuning")

      PipeOpLearnerBagged$new(
        self$graph(task, measure, n_threads, fit_timeout, devices),
        id = self$id,
        measure = measure,
        internal_search_space = if (length(internal_ids)) search_space$clone(deep = TRUE)$subset(internal_ids),
        param_vals = list(bagging.folds = folds, bagging.repeats = repeats)
      )
    },

    #' @description
    #' Estimate the number of early stopping rounds (the patience) for a learner.
    #' `budget` is the maximum number of training rounds (boosting iterations or epochs) the learner may use.
    #' The patience is capped well below the budget, otherwise early stopping and validation-based internal tuning
    #' can never trigger and the learner always trains for the full budget.
    #'
    #' @param budget (`integer(1)`)\cr
    #'   Maximum number of training rounds (boosting iterations or epochs) the learner may use.
    early_stopping_rounds = function(task, budget = Inf) {
      min_early_stopping_rounds = 20L
      max_early_stopping_rounds = 200L

      patience = if (task$nrow < 1e4) {
        max_early_stopping_rounds
      } else {
        floor(max(min_early_stopping_rounds, 1e4 / task$nrow * max_early_stopping_rounds))
      }

      min(patience, max(min_early_stopping_rounds, budget %/% 5L))
    },

    #' @description
    #' Estimate the memory for the auto.
    #' The estimate is the host memory in MB, so learners that allocate on the gpu return `-Inf`.
    estimate_memory = function(task, devices = "cpu") {
      # -Inf allows learners without a memory estimate to always pass memory checks
      -Inf
    },

    #' @description
    #' Prepare the graph learner for the final model fit.
    #' Called after tuning to undo tuning-only setup (e.g., timeout callbacks).
    #'
    #' @param graph_learner ([mlr3pipelines::GraphLearner]).
    finalize_model = function(graph_learner) {
      invisible(graph_learner)
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
      exclude_tasks = getOption("mlr3automl.exclude_tasks", character(0))
      if (any(exclude_tasks %in% data$task)) {
        lg$info("Excluding tasks from initial design: %s", as_short_string(exclude_tasks[exclude_tasks %in% data$task]))
        data = data[task %nin% exclude_tasks]
      }

      # subset to relevant measure
      measure_id = sub(sprintf("^%s\\.", task$task_type), "", measure$id)
      if (measure_id %nin% data$measure) {
        # warm-start data does not cover this measure
        # return empty design
        lg$info("Learner '%s' has no initial design data for measure '%s'; returning empty design", self$id, measure$id)
        return(self$design_default(task)[0])
      }
      data = data[measure_id, , on = "measure"]

      # subset to relevant parameters
      search_space = self$search_space(task)
      param_ids = search_space$ids()
      param_internal_ids = search_space$ids(any_tags = "internal_tuning")
      param_ids = setdiff(param_ids, param_internal_ids)
      data = data[, param_ids, with = FALSE]

      # drop warm-start points that violate the (possibly task-dependent) search space bounds
      # e.g. kknn.k upper is log(min(100, nrow - 1)), but the stored values can go up to log(100)
      lower = search_space$lower
      upper = search_space$upper
      for (param_id in param_ids) {
        if (is.na(lower[[param_id]]) || is.na(upper[[param_id]])) {
          next
        }
        in_bounds = data[[param_id]] >= lower[[param_id]] & data[[param_id]] <= upper[[param_id]]
        if (!all(in_bounds)) {
          lg$info(
            "Learner '%s' drops %i initial design point(s) out of bounds for parameter '%s'",
            self$id,
            sum(!in_bounds),
            param_id
          )
          data = data[in_bounds]
        }
      }

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
