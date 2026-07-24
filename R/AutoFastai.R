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
#' @template section_python
#'
#' @return Object of class [R6::R6Class] and `AutoFastai`.
#'
#' @templateVar id fastai
#' @template example_auto
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
        patience = self$early_stopping_rounds(task, budget = self$search_space(task)$upper[["fastai.n_epoch"]]),
        layers = c(200, 100),
        eval_metric = self$internal_measure(measure, task)
      )
      set_threads(learner, n_threads)

      # the learner trains and predicts in an isolated callr session (see isolated_model.R),
      # so the encapsulation only adds the fallback and log capture
      fallback = lrn("classif.featureless")
      fallback$predict_type = measure$predict_type
      learner$predict_type = measure$predict_type
      learner$encapsulate(method = "evaluate", fallback = fallback)

      po("colapply", id = "fastai_character", applicator = as.factor, affect_columns = selector_type("character")) %>>%
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
# this subclass keeps python strictly inside the isolated callr sessions
# started with isolated_session() (see isolated_model.R)
# * .train() and .predict() run .session_train() and .session_predict() in an
#   isolated session and never touch python themselves.
# * loading the fastai R package initializes python, so "fastai" is removed
#   from the packages, which mlr3 loads in the calling process. the fastai
#   namespace loads lazily inside the session, after py_require().
# * .session_train() registers the python requirements, which the fastai
#   learner does not do itself, extracts the internal tuning values and
#   validation scores while the python model is live, and pickles the model,
#   so it leaves the session as raw bytes.
# * the extractor overrides return the stashed values in the calling process.
# * .session_predict() unpickles the model inside the session.

#' @title Fastai Learner Isolated
#'
#' @description
#' A subclass of [mlr3extralearners::LearnerClassifFastai] that isolates the Python environment in a callr session.
#'
#' @return Object of class [R6::R6Class] and `LearnerClassifFastaiIsolated`.
#'
#' @export
LearnerClassifFastaiIsolated = R6Class(
  "LearnerClassifFastaiIsolated",
  inherit = mlr3extralearners::LearnerClassifFastai,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize()
      # the isolated session must load mlr3automl to find this class
      self$packages = setdiff(union(self$packages, "mlr3automl"), "fastai")
    }
  ),
  private = list(
    .train = function(task) {
      result = isolated_session(self, task, ".session_train")
      structure(
        list(
          pickled = result$marshaled,
          internal_tuned_values = result$internal_tuned_values,
          internal_valid_scores = result$internal_valid_scores
        ),
        class = "isolated_model_pickled"
      )
    },
    .predict = function(task) {
      isolated_session(self, task, ".session_predict")
    },
    # runs in the isolated session
    .session_train = function(task) {
      clean_reticulate_env()
      reticulate::py_require(fastai_python_packages)
      require_namespaces("fastai")
      model = super$.train(task)
      # the extractors need the live python model, so they run in this session
      self$state = list(model = model, param_vals = self$param_set$values)
      internal_tuned_values = super$.extract_internal_tuned_values()
      internal_valid_scores = if (!is.null(get0("validate", self))) {
        super$.extract_internal_valid_scores()
      }
      list(
        marshaled = marshal_model(model, inplace = TRUE),
        internal_tuned_values = internal_tuned_values,
        internal_valid_scores = internal_valid_scores
      )
    },
    # runs in the isolated session
    .session_predict = function(task) {
      clean_reticulate_env()
      reticulate::py_require(fastai_python_packages)
      # loading fastai registers the s3 predict method for the python learner
      require_namespaces("fastai")
      if (inherits(self$model, "isolated_model_pickled")) {
        self$model = unmarshal_model(self$model$pickled)
      }
      super$.predict(task)
    },
    # the values are extracted in the isolated session and stashed in the model
    .extract_internal_tuned_values = function() {
      self$model$internal_tuned_values
    },
    .extract_internal_valid_scores = function() {
      self$model$internal_valid_scores
    }
  )
)
