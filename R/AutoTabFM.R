#' @title TabFM Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Tabfm auto.
#'
#' Tabfm predicts in context, so every prediction runs the backbone over the training rows once per estimator.
#' This is too slow to be useful on the CPU, so the auto is registered for `"cuda"` only and
#' [Auto]`$check()` removes it from the search space whenever `devices` does not include `"cuda"`.
#' Construct it with `AutoTabFM$new(devices = c("cpu", "cuda"))` and re-register it in [mlr_auto] to run it
#' on the CPU anyway.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_memory_limit
#' @template param_large_data_set
#' @template param_devices
#' @template section_python
#'
#' @return Object of class [R6::R6Class] and `AutoTabFM`.
#'
#' @templateVar id tabfm
#' @template example_auto
#'
#' @export
AutoTabFM = R6Class(
  "AutoTabFM",
  inherit = Auto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param devices (`character()`)\cr
    #'   Devices the auto is allowed to run on.
    #'   Defaults to `"cuda"` only, because tabfm is too slow to be useful on the CPU.
    initialize = function(id = "tabfm", devices = "cuda") {
      super$initialize(
        id = id,
        properties = character(0),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3extralearners", "callr"),
        devices = assert_subset(devices, c("cpu", "cuda"), empty.ok = FALSE),
        n_cpu = 1L,
        n_gpu = 1L
      )
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE, devices = "cpu") {
      # the device gate of super$check() removes the auto whenever `devices` and `self$devices` are disjoint,
      # so the cuda-only registration is what keeps tabfm off the cpu.
      # run the cheap checks (R packages, task type, memory, devices) before the expensive python probe
      ok = super$check(task, memory_limit, large_data_set, devices)
      if (!isTRUE(ok)) {
        return(FALSE)
      }
      ok = check_python_packages(tabfm_python_packages)
      if (!isTRUE(ok)) {
        lg$info(ok)
        lg$info("Remove tabfm from search space")
        return(FALSE)
      }
      TRUE
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
        LearnerClassifTabFMIsolated$new()
      } else {
        LearnerRegrTabFMIsolated$new()
      }
      learner$id = "tabfm"
      # the pytorch backend is the only one that honors `device`, so the resource accounting of the workers
      # only holds for this backend
      learner$param_set$set_values(backend = "pytorch", device = device)

      set_threads(learner, n_threads)

      # the learner trains and predicts in an isolated callr session (see isolated_model.R),
      # so the encapsulation only adds the fallback and log capture
      fallback = lrn(sprintf("%s.featureless", task$task_type))
      fallback$predict_type = measure$predict_type
      learner$predict_type = measure$predict_type
      learner$encapsulate(method = "evaluate", fallback = fallback)

      # tabfm ordinal encodes categorical features and imputes missing values in its own preprocessing
      # pipeline, so the graph only has to keep the factor levels stable across train and predict
      po("colapply", id = "tabfm_character", applicator = as.factor, affect_columns = selector_type("character")) %>>%
        po("removeconstants", id = "tabfm_removeconstants") %>>%
        po("fixfactors", id = "tabfm_fixfactors") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task, devices = "cpu") {
      memory_size = task$nrow * task$ncol * 8 * 10 / 1e6
      lg$info("Tabfm memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    },

    #' @description
    #' Default hyperparameters for the learner.
    design_default = function(task) {
      values = list(
        # the default of 32 estimators is far above the upper bound of the search space,
        # because a single fit already runs the backbone once per estimator
        tabfm.n_estimators = 4L,
        tabfm.feat_shuffle_method = "random",
        tabfm.permute_categorical = FALSE,
        tabfm.cat_encoder_mode = "appearance"
      )
      if (task$task_type == "classif") {
        values = c(
          values,
          list(
            tabfm.softmax_temperature = 0.9,
            tabfm.average_logits = TRUE,
            tabfm.class_shift = TRUE
          )
        )
      }
      xdt = as.data.table(values)
      set(xdt, j = "branch.selection", value = self$id)
      xdt
    },

    #' @description
    #' Get the search space for the auto.
    search_space = function(task) {
      params = list(
        tabfm.n_estimators = p_int(1, 8),
        tabfm.feat_shuffle_method = p_fct(c("random", "none")),
        tabfm.permute_categorical = p_lgl(),
        tabfm.cat_encoder_mode = p_fct(c("appearance", "frequency"))
      )
      if (task$task_type == "classif") {
        params = c(
          params,
          list(
            tabfm.softmax_temperature = p_dbl(0.75, 1.0),
            tabfm.average_logits = p_lgl(),
            tabfm.class_shift = p_lgl()
          )
        )
      }
      invoke(ps, .args = params)
    }
  ),

  private = list()
)

mlr_auto$add("tabfm", function() AutoTabFM$new())

# the "pytorch" backend of the tabfm learner is the only one that honors the `device` parameter.
# "safetensors" is not part of the "pytorch" extra but is needed to load the weights.
tabfm_python_packages = c("tabfm[pytorch]", "safetensors")

# the tabfm learner imports python torch via reticulate.
# these subclasses keep python strictly inside the isolated callr sessions
# started with isolated_session() (see isolated_model.R)
# * .train() and .predict() run .session_train() and .session_predict() in an
#   isolated session and never touch python themselves.
# * .session_train() pickles the model, so it leaves the session as raw bytes.
# * .session_predict() registers the python requirements, which the tabfm
#   learner only does when training, and unpickles the model inside the session.

#' @title TabFM Learner Isolated
#'
#' @description
#' A subclass of [mlr3extralearners::LearnerClassifTabFM] that isolates the Python environment in a callr session.
#'
#' @return Object of class [R6::R6Class] and `LearnerClassifTabFMIsolated`.
#'
#' @export
LearnerClassifTabFMIsolated = R6Class(
  "LearnerClassifTabFMIsolated",
  inherit = mlr3extralearners::LearnerClassifTabFM,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
      # the isolated session must load mlr3automl to find this class
      self$packages = union(self$packages, "mlr3automl")
    }
  ),
  private = list(
    .train = function(task) {
      result = isolated_session(self, task, ".session_train")
      structure(list(pickled = result$marshaled), class = "isolated_model_pickled")
    },
    .predict = function(task) {
      isolated_session(self, task, ".session_predict")
    },
    # runs in the isolated session
    .session_train = function(task) {
      clean_reticulate_env()
      reticulate::py_require(tabfm_python_packages)
      model = super$.train(task)
      list(marshaled = marshal_model(model, inplace = TRUE))
    },
    # runs in the isolated session
    .session_predict = function(task) {
      clean_reticulate_env()
      reticulate::py_require(tabfm_python_packages)
      if (inherits(self$model, "isolated_model_pickled")) {
        self$model = unmarshal_model(self$model$pickled)
      }
      super$.predict(task)
    }
  )
)

#' @title TabFM Regressor Learner Isolated
#'
#' @description
#' A subclass of [mlr3extralearners::LearnerRegrTabFM] that isolates the Python environment in a callr session.
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrTabFMIsolated`.
#'
#' @export
LearnerRegrTabFMIsolated = R6Class(
  "LearnerRegrTabFMIsolated",
  inherit = mlr3extralearners::LearnerRegrTabFM,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
      # the isolated session must load mlr3automl to find this class
      self$packages = union(self$packages, "mlr3automl")
    }
  ),
  private = list(
    .train = function(task) {
      result = isolated_session(self, task, ".session_train")
      structure(list(pickled = result$marshaled), class = "isolated_model_pickled")
    },
    .predict = function(task) {
      isolated_session(self, task, ".session_predict")
    },
    # runs in the isolated session
    .session_train = function(task) {
      clean_reticulate_env()
      reticulate::py_require(tabfm_python_packages)
      model = super$.train(task)
      list(marshaled = marshal_model(model, inplace = TRUE))
    },
    # runs in the isolated session
    .session_predict = function(task) {
      clean_reticulate_env()
      reticulate::py_require(tabfm_python_packages)
      if (inherits(self$model, "isolated_model_pickled")) {
        self$model = unmarshal_model(self$model$pickled)
      }
      super$.predict(task)
    }
  )
)
