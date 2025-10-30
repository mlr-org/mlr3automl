#' @title Regression AutoML Learner
#'
#' @description
#' The [LearnerRegrAuto] is an automated machine learning (AutoML) system for regression tasks.
#' It combines preprocessing, a switch between multiple learners and hyperparameter tuning to find the best model for the given task.
#'
#' @template param_id
#' @template param_learner_ids
#' @template section_debugging
#' @template section_parameters
#'
#' @export
LearnerRegrAuto = R6Class("LearnerRegrAuto",
  inherit = Learner,
  public = list(

    #' @field tuning_space (`list()`).
    tuning_space = NULL,

    #' @field instance ([mlr3tuning::TuningInstanceAsyncSingleCrit]).
    instance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "regr.auto",
      learner_ids
      ) {
      all_learner_ids = mlr_auto$keys()
      if (missing(learner_ids)) learner_ids = all_learner_ids
      assert_subset(learner_ids, all_learner_ids)
      private$.learner_ids = learner_ids

      param_set = ps(
        learner_timeout = p_int(lower = 1L, init = 900L, tags = c("train", "super")),
        # system
        n_threads = p_int(lower = 1L, init = 1L, tags = c("train", "catboost", "lightgbm", "ranger", "xgboost")),
        memory_limit = p_int(lower = 1L, init = 32000L, tags = c("train", "catboost", "lightgbm", "ranger", "xgboost")),
        devices = p_uty(init = "cpu", tags = c("train", "super"), custom_check = crate({function(x) check_subset(x, c("cpu", "cuda"))})),
        # large data
        large_data_size = p_int(lower = 1L, init = 1e6, tags = c("train", "super")),
        # small data
        small_data_size = p_int(lower = 1L, init = 5000L, tags = c("train", "super")),
        small_data_resampling = p_uty(init = rsmp("cv", folds = 10), tags = c("train", "super")),
        # initial design
        initial_design_default = p_lgl(init = FALSE, tags = c("train", "super")),
        initial_design_set = p_int(lower = 0L, init = 0L, tags = c("train", "super")),
        initial_design_type = p_fct(init = "sobol", levels = c("lhs", "sobol", "random"), tags = c("train", "super")),
        initial_design_size = p_int(lower = 0L, init = 256L, tags = c("train", "super")),
        initial_design_fraction = p_dbl(lower = 0.1, upper = 0.9, init = 0.25, tags = c("train", "super")),
        # tuner
        resampling = p_uty(init = rsmp("holdout"), tags = c("train", "super")),
        terminator = p_uty(init = trm("run_time", secs = 3600), tags = c("train", "super")),
        measure = p_uty(tags = c("train", "super")),
        callbacks = p_uty(tags = c("train", "super")),
        store_benchmark_result = p_lgl(init = FALSE, tags = c("train", "super")),
        store_models = p_lgl(init = FALSE, tags = c("train", "super")),
        # debugging
        encapsulate_learner = p_lgl(init = TRUE, tags = c("train", "super")),
        encapsulate_mbo = p_lgl(init = TRUE, tags = c("train", "super")),
        check_learners = p_lgl(init = TRUE, tags = c("train", "super"))
      )
      # subset to relevant parameters for selected learners
      param_set = param_set$subset(ids = unique(param_set$ids(any_tags = c("super", learner_ids))))

      autos = mlr_auto$mget(learner_ids)
      packages = unlist(map(autos, "packages"))

      super$initialize(
        id = id,
        task_type = "regr",
        param_set = param_set,
        packages = union(c("mlr3", "mlr3tuning","mlr3pipelines", "mlr3learners"), packages),
        feature_types = c("logical", "integer", "numeric", "character", "factor"),
        predict_types = "response",
        properties = c("missings", "weights"),
      )
    }
  ),

  private = list(
    .learner_ids = NULL,

   .train = function(task) {
      train_auto(self, private, task)
    },

    .predict = function(task) {
      lg$debug("Predicting with '%s' on task '%s'", self$id, task$id)
      self$model$graph_learner$predict(task)
    },

    deep_clone = function(name, value) {
      if (is.R6(value)) {
        return(value$clone(deep = TRUE))
      } else if (name == "state") {
        if (!is.null(value)) {
          value = list(
            instance = value$instance$clone(deep = TRUE))
        }
        return(value)
      } else {
        super$deep_clone(name, value)
      }
    }
  )
)

#' @include aaa.R
learners[["regr.auto"]] = LearnerRegrAuto
