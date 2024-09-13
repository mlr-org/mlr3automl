#' @title Regression Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#'
#' @export
LearnerRegrAuto = R6Class("LearnerRegrAuto",
  inherit = Learner,
  public = list(

    #' @field graph ([mlr3pipelines::Graph]).
    graph = NULL,

    #' @field tuning_space (`list()`).
    tuning_space = NULL,

    #' @field instance ([mlr3tuning::TuningInstanceAsyncSingleCrit]).
    instance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto") {
      param_set = ps(
        # learner
        learner_ids = p_uty(default = c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"),
          custom_check = function(x) {
            if (length(x) == 1 && x == "extra_trees") {
              return("Learner 'extra_trees' must be combined with other learners")
            }
            check_subset(x, c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"))
        }),
        learner_timeout = p_int(lower = 1L, default = 900L),
        xgboost_eval_metric = p_uty(),
        catboost_eval_metric = p_uty(),
        lightgbm_eval_metric = p_uty(),
        # system
        max_nthread = p_int(lower = 1L, default = 1L),
        max_memory = p_int(lower = 1L, default = 32000L),
        # large data
        large_data_size = p_int(lower = 1L, default = 1e6),
        large_data_learner_ids = p_uty(),
        large_data_nthread = p_int(lower = 1L, default = 4L),
        # small data
        small_data_size = p_int(lower = 1L, default = 5000L),
        small_data_resampling = p_uty(),
        max_cardinality = p_int(lower = 1L, default = 100L),
        extra_trees_max_cardinality = p_int(lower = 1L, default = 40L),
        # tuner
        resampling = p_uty(),
        terminator = p_uty(),
        measure = p_uty(),
        lhs_size = p_int(lower = 1L, default = 4L),
        callbacks = p_uty(),
        store_benchmark_result = p_lgl(default = FALSE))

      param_set$set_values(
        learner_ids = c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"),
        learner_timeout = 900L,
        max_nthread = 1L,
        max_memory = 32000L,
        large_data_size = 1e6L,
        large_data_learner_ids = c("ranger", "xgboost", "catboost", "extra_trees", "lightgbm"),
        large_data_nthread = 4L,
        small_data_size = 5000L,
        small_data_resampling = rsmp("cv", folds = 10L),
        max_cardinality = 100L,
        extra_trees_max_cardinality = 40L,
        resampling = rsmp("cv", folds = 3L),
        terminator = trm("run_time", secs = 14400L),
        measure = msr("regr.mse"),
        lhs_size = 4L,
        store_benchmark_result = FALSE)

      super$initialize(
        id = id,
        task_type = "regr",
        param_set = param_set,
        packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", "xgboost", "catboost", "lightgbm", "ranger", "nnet", "kknn", "glmnet", "e1071"),
        feature_types = c("logical", "integer", "numeric", "character", "factor"),
        predict_types = "response",
        properties = c("missings", "weights")
      )
    }
  ),
  private = list(
    .train = function(task) {
      train_auto(self, task, task_type = "regr")
    },

    .predict = function(task) {
      lg$debug("Predicting with '%s' on task '%s'", self$id, task$id)
      self$model$graph_learner$predict(task)
    }
  )
)

#' @include aaa.R
learners[["regr.auto"]] = LearnerRegrAuto
