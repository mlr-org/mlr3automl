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
#' @template param_devices
#'
#' @export
AutoTabpfn = R6Class(
  "AutoTabpfn",
  inherit = Auto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "tabpfn") {
      super$initialize(
        id = id,
        properties = character(0),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3extralearners", "callr"),
        devices = c("cpu", "cuda")
      )
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE, devices = "cpu") {
      ok = check_python_packages(c("torch", "tabpfn"))
      if (!isTRUE(ok)) {
        lg$info(ok)
        lg$info("Remove tabpfn from search space")
        return(FALSE)
      }
      if ("cuda" %nin% devices && task$nrow > 1e3) {
        lg$info(
          "Learner '%s' is not compatible with tasks with more than 1,000 rows when using 'cpu' as device",
          self$id
        )
        return(FALSE)
      }
      if (task$ncol > 500) {
        lg$info("Learner '%s' is not compatible with tasks with more than 500 features", self$id)
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
      assert_subset(devices, c("cpu", "cuda"))

      require_namespaces("mlr3extralearners")

      device = if ("cuda" %in% devices) "cuda" else "cpu"

      learner = if (task$task_type == "classif") {
        LearnerClassifTabPFNIsolated$new()
      } else {
        LearnerRegrTabPFNIsolated$new()
      }
      learner$id = "tabpfn"
      learner$param_set$set_values(device = device)

      set_threads(learner, n_threads)

      # tabpfn loads python torch via reticulate which is incompatible with mlr3torch
      # train and predict in a short-lived callr session
      fallback = lrn(sprintf("%s.featureless", task$task_type))
      fallback$predict_type = measure$predict_type
      learner$predict_type = measure$predict_type
      learner$encapsulate(method = "callr", fallback = fallback)

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
    #' Default hyperparameters for the learner.
    design_default = function(task) {
      values = if (task$task_type == "classif") {
        list(
          tabpfn.n_estimators = 4L,
          tabpfn.softmax_temperature = 1.0,
          tabpfn.balance_probabilities = FALSE,
          tabpfn.average_before_softmax = FALSE
        )
      } else {
        list(
          tabpfn.n_estimators = 4L,
          tabpfn.average_before_softmax = FALSE
        )
      }
      xdt = as.data.table(values)
      set(xdt, j = "branch.selection", value = self$id)
      xdt
    },

    #' @description
    #' Get the search space for the auto.
    search_space = function(task) {
      if (task$task_type == "classif") {
        ps(
          tabpfn.n_estimators = p_int(1, 8),
          tabpfn.softmax_temperature = p_dbl(0.75, 1.0),
          tabpfn.balance_probabilities = p_lgl(),
          tabpfn.average_before_softmax = p_lgl()
        )
      } else if (task$task_type == "regr") {
        ps(
          tabpfn.n_estimators = p_int(1, 8),
          tabpfn.average_before_softmax = p_lgl()
        )
      }
    }
  ),

  private = list()
)

mlr_auto$add("tabpfn", function() AutoTabpfn$new())

# the tabpfn learner imports python torch via reticulate.
# these subclasses keep python strictly inside the callr sessions used for process isolation
# * .train() registers the python requirements, which the tabpfn learner does
#   not do itself, and wraps the model as an isolated model, so it leaves the
#   session as raw bytes.
# * .predict() unpickles the model inside the callr session.
LearnerClassifTabPFNIsolated = R6Class(
  "LearnerClassifTabPFNIsolated",
  inherit = mlr3extralearners::LearnerClassifTabPFN,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
      # the callr session must load mlr3automl to find this class
      self$packages = union(self$packages, "mlr3automl")
    }
  ),
  private = list(
    .train = function(task) {
      clean_reticulate_env()
      reticulate::py_require(c("torch", "tabpfn"))
      new_isolated_model(super$.train(task))
    },
    .predict = function(task) {
      clean_reticulate_env()
      reticulate::py_require(c("torch", "tabpfn"))
      if (inherits(self$model, "isolated_model_pickled")) {
        self$model = unmarshal_model(self$model$pickled)
      }
      super$.predict(task)
    }
  )
)

LearnerRegrTabPFNIsolated = R6Class(
  "LearnerRegrTabPFNIsolated",
  inherit = mlr3extralearners::LearnerRegrTabPFN,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
      # the callr session must load mlr3automl to find this class
      self$packages = union(self$packages, "mlr3automl")
    }
  ),
  private = list(
    .train = function(task) {
      clean_reticulate_env()
      reticulate::py_require(c("torch", "tabpfn"))
      new_isolated_model(super$.train(task))
    },
    .predict = function(task) {
      clean_reticulate_env()
      reticulate::py_require(c("torch", "tabpfn"))
      if (inherits(self$model, "isolated_model_pickled")) {
        self$model = unmarshal_model(self$model$pickled)
      }
      super$.predict(task)
    }
  )
)
