#' @title Auto Class
#'
#' @description
#' This class is the base class for all autos.
#'
#' @include mlr_auto.R
#'
#' @template param_id
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

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id) {
      self$id = assert_string(id)
    },

    #' @description
    #' Check if the auto is compatible with the task.
    #'
    #' @param task ([mlr3::Task]).
    #' @param memory_limit (`numeric(1)`).
    #' @param large_data_set (`logical(1)`).
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
    #'
    #' @param task ([mlr3::Task]).
    #' @param measure ([mlr3::Measure]).
    #' @param n_threads (`numeric(1)`).
    #' @param timeout (`numeric(1)`).
    graph = function(task, measure, n_threads, timeout) {
      stop("Abstract")
    },

    #' @description
    #' Estimate the number of early stopping rounds.
    #'
    #' @param task [mlr3::Task].
    early_stopping_rounds = function(task) {
      min_early_stopping_rounds = 20L
      max_early_stopping_rounds = 200L

      if (task$nrow < 1e4) return(max_early_stopping_rounds)

      floor((max(min_early_stopping_rounds, 1e4 / task$nrow * max_early_stopping_rounds)))
    },

    #' @description
    #' Estimate the memory for the auto.
    #'
    #' @param task ([mlr3::Task]).
    estimate_memory = function(task) {
      -Inf
    },

    #' @description
    #' Get the default values for the auto.
    #'
    #' @param task ([mlr3::Task]).
    default_values = function(task) {
      stop("Abstract")
    },

    #' @description
    #' Default hyperparameters for the learner.
    #'
    #' @param task ([mlr3::Task]).
    design_default = function(task) {
      default_values = self$default_values(task)
      xdt = as.data.table(default_values)
      set(xdt, j = "branch.selection", value = self$id)
      xdt
    },

    #' @description
    #' DGenerate lhs design for the learner.
    #'
    #' @param task ([mlr3::Task]).
    #' @param size (`integer(1)`).
    design_lhs = function(task, size) {
      assert_task(task)
      assert_count(size)

      if (!self$search_space$length) return(data.table())
      internal_tune_ids = self$search_space$ids(any_tags = "internal_tuning")
      n_levels = self$search_space$nlevels[self$search_space$ids(class = c("ParamFct", "ParamLgl"))]
      xdt = generate_design_lhs(self$search_space, as.integer(max(n_levels, size)))$data
      if (length(internal_tune_ids)) {
        xdt = xdt[, setdiff(self$search_space$ids(), internal_tune_ids), with = FALSE]
      }
      set(xdt, j = "branch.selection", value = self$id)
      xdt
    }
  ),

  active = list(
    #' @field search_space (`ParamSet`).
    search_space = function() {
      stop("Abstract")
    },

    #' @field packages (`character()`).
    packages = function(rhs) {
     assert_ro_binding(rhs)
     lrn(sprintf("classif.%s", self$id))$packages
    }
  )
)
