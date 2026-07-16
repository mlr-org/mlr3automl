#' @title Fastai Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Fastai auto.
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
AutoFastai = R6Class(
  "AutoFastai",
  inherit = Auto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "fastai") {
      # fastai package is not listed
      # to avoid initializing python in the main session (see isolated_model.R)
      super$initialize(
        id = id,
        properties = "internal_tuning",
        task_types = "classif",
        packages = c("mlr3", "mlr3extralearners", "callr"),
        devices = c("cpu", "cuda")
      )
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE, devices = "cpu") {
      ok = super$check(task, memory_limit, large_data_set, devices)
      if (!isTRUE(ok)) {
        return(FALSE)
      }
      # find.package() checks installation without loading
      if (!length(find.package("fastai", quiet = TRUE))) {
        lg$info("Learner '%s' is not available. Missing packages: fastai", self$id)
        return(FALSE)
      }
      ok = check_python_packages(fastai_python_packages)
      if (!isTRUE(ok)) {
        lg$info(ok)
        lg$info("Remove fastai from search space")
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

      learner = LearnerClassifFastaiIsolated$new()
      learner$id = "fastai"
      learner$param_set$set_values(
        patience = self$early_stopping_rounds(task),
        layers = c(200, 100),
        eval_metric = self$internal_measure(measure, task)
      )
      set_threads(learner, n_threads)

      # fastai loads python torch via reticulate which is incompatible with mlr3torch
      # train and predict in a short-lived callr session
      fallback = lrn("classif.featureless")
      fallback$predict_type = measure$predict_type
      learner$predict_type = measure$predict_type
      learner$encapsulate(method = "callr", fallback = fallback)

      po("removeconstants", id = "fastai_removeconstants") %>>%
        po("imputeoor", id = "fastai_imputeoor") %>>%
        po("fixfactors", id = "fastai_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "fastai_imputesample") %>>%
        po("encodeimpact", id = "fastai_encode") %>>%
        po("removeconstants", id = "fastai_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      memory_size = task$nrow * task$ncol * 8 * 10 / 1e6
      lg$info("Fastai memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    },

    #' @description
    #' Get the internal measure for the auto.
    internal_measure = function(measure, task) {
      if ("twoclass" %in% task$properties) {
        switch(
          measure$id,
          "classif.ce" = "error_rate",
          "classif.acc" = "accuracy",
          "classif.bacc" = "BalancedAccuracy",
          "classif.brier" = "BrierScore",
          "classif.auc" = "RocAucBinary",
          "classif.mcc" = "MatthewsCorrCoef",
          "error_rate" # default
        )
      } else if ("multiclass" %in% task$properties) {
        switch(
          measure$id,
          "classif.ce" = "error_rate",
          "classif.acc" = "accuracy",
          "classif.mauc_aunp" = "RocAuc",
          "error_rate" # default
        )
      }
    }
  ),
  private = list(
    .default_values = list(
      fastai.lr = log(1e-3),
      fastai.bs = 50,
      fastai.layers = "c(200, 100)"
    ),

    .search_space = ps(
      fastai.lr = p_dbl(1e-4, 1e-1, logscale = TRUE),
      fastai.bs = p_int(50, 500),
      fastai.n_epoch = p_int(1, 100, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x))))),
      fastai.layers = p_fct(
        levels = list(c(200, 100), c(200, 100, 50), c(500, 200), c(500, 200, 100), c(1000, 500), c(1000, 500, 200)),
        default = "c(200, 100)"
      )
    )
  )
)

mlr_auto$add("fastai", function() AutoFastai$new())

# fastai (<= 2.8.7) is incompatible with fastcore 2.0 but does not declare an upper bound
fastai_python_packages = c("IPython", "torch", "torchvision", "fastai", "fastcore<2.0.0", "pydicom", "kornia")

# the fastai learner imports python torch via reticulate.
# this subclass keeps python strictly inside the callr sessions used for process isolation
# * loading the fastai R package initializes python, so "fastai" is removed
#   from the packages. mlr3 loads the packages in the calling process and the
#   callr session before the python requirements are registered. the fastai
#   namespace loads lazily during .train(), after py_require().
# * .train() registers the python requirements, which the fastai learner does
#   not do itself, and wraps the model as an isolated model, so it leaves the
#   session as raw bytes. the model stays live until then, so mlr3 can extract
#   the internal tuning values and validation scores from it.
# * .predict() unpickles the model inside the callr session.
LearnerClassifFastaiIsolated = R6Class(
  "LearnerClassifFastaiIsolated",
  inherit = mlr3extralearners::LearnerClassifFastai,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
      # the callr session must load mlr3automl to find this class
      self$packages = setdiff(union(self$packages, "mlr3automl"), "fastai")
    }
  ),
  private = list(
    .train = function(task) {
      clean_reticulate_env()
      reticulate::py_require(fastai_python_packages)
      require_namespaces("fastai")
      new_isolated_model(super$.train(task))
    },
    .predict = function(task) {
      clean_reticulate_env()
      reticulate::py_require(fastai_python_packages)
      # loading fastai registers the s3 predict method for the python learner
      require_namespaces("fastai")
      if (inherits(self$model, "isolated_model_pickled")) {
        self$model = unmarshal_model(self$model$pickled)
      }
      super$.predict(task)
    }
  )
)
