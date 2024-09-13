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
    initialize = function(
      id = "regr.auto",
      learner_ids = c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm")
      ) {
      assert_subset(learner_ids, c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"))
      if (all(learner_ids %in% "extra_trees")) {
        stop("Learner 'lda' and 'extra_trees' must be combined with other learners")
      }
      private$.learner_ids = learner_ids
      self$tuning_space = tuning_space[private$.learner_ids]

      param_set = ps(
        learner_timeout = p_int(lower = 1L, default = 900L, tags = c("train", "super")),
        # internal eval metric
        xgboost_eval_metric = p_uty(tags = c("train", "xgboost")),
        catboost_eval_metric = p_uty(tags = c("train", "catboost")),
        lightgbm_eval_metric = p_uty(tags = c("train", "lightgbm")),
        # system
        max_nthread = p_int(lower = 1L, default = 1L, tags = c("train", "catboost", "lightgbm", "ranger", "xgboost")),
        max_memory = p_int(lower = 1L, default = 32000L, tags = c("train", "catboost", "lightgbm", "ranger", "xgboost")),
        # large data
        large_data_size = p_int(lower = 1L, default = 1e6, tags = c("train", "super")),
        large_data_learner_ids = p_uty(tags = c("train", "super")),
        large_data_nthread = p_int(lower = 1L, default = 4L, tags = c("train", "catboost", "lightgbm", "ranger", "xgboost")),
        # small data
        small_data_size = p_int(lower = 1L, default = 5000L, tags = c("train", "super")),
        small_data_resampling = p_uty(tags = c("train", "super")),
        # cardinality
        max_cardinality = p_int(lower = 1L, default = 100L, tags = c("train", "super")),
        extra_trees_max_cardinality = p_int(lower = 1L, default = 40L, tags = c("train", "extratrees")),
        # tuner
        resampling = p_uty(tags = c("train", "super")),
        terminator = p_uty(tags = c("train", "super")),
        measure = p_uty(tags = c("train", "super")),
        lhs_size = p_int(lower = 1L, default = 4L, tags = c("train", "super")),
        callbacks = p_uty(tags = c("train", "super")),
        store_benchmark_result = p_lgl(default = FALSE, tags = c("train", "super")))

      param_set$set_values(
        learner_timeout = 900L,
        max_nthread = 1L,
        max_memory = 32000L,
        large_data_size = 1e6L,
        large_data_learner_ids = intersect(c("ranger", "xgboost", "catboost", "extra_trees", "lightgbm"), private$.learner_ids),
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

      # subset to relevant parameters for selected learners
      param_set = param_set$subset(ids = unique(param_set$ids(any_tags = c("super", learner_ids))))

      self$graph = build_graph(private$.learner_ids, "regr")

      super$initialize(
        id = id,
        task_type = "regr",
        param_set = param_set,
        packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", self$graph$packages),
        feature_types = c("logical", "integer", "numeric", "character", "factor"),
        predict_types = "response",
        properties = c("missings", "weights")
      )
    }
  ),
  private = list(
    .train = function(task) {
      train_auto(self, task)
    },

    .predict = function(task) {
      lg$debug("Predicting with '%s' on task '%s'", self$id, task$id)
      self$model$graph_learner$predict(task)
    }
  )
)

#' @include aaa.R
learners[["regr.auto"]] = LearnerRegrAuto
