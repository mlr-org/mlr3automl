#' @title Classification AutoML Learner
#'
#' @description
#' The [LearnerClassifAuto] is an automated machine learning (AutoML) system for classification tasks.
#' It combines preprocessing, a switch between multiple learners and hyperparameter tuning to find the best model for the given task.
#'
#' @template param_id
#' @template param_learner_ids
#' @template section_debugging
#' @template section_parameters
#'
#' @export
LearnerClassifAuto = R6Class("LearnerClassifAuto",
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
      id = "classif.auto",
      learner_ids = c("glmnet", "kknn", "lda", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm")
      ) {
      assert_subset(learner_ids, c("glmnet", "kknn", "lda", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"))
      if (all(learner_ids %in% c("lda", "extra_trees"))) {
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
        extra_trees_max_cardinality = p_int(lower = 1L, default = 40L, tags = c("train", "extra_trees")),
        # tuner
        resampling = p_uty(tags = c("train", "super")),
        terminator = p_uty(tags = c("train", "super")),
        measure = p_uty(tags = c("train", "super")),
        lhs_size = p_int(lower = 1L, default = 4L, tags = c("train", "super")),
        callbacks = p_uty(tags = c("train", "super")),
        store_benchmark_result = p_lgl(default = FALSE, tags = c("train", "super")),
        store_models = p_lgl(default = FALSE, tags = c("train", "super")),
        # debugging
        encapsulate_learner = p_lgl(default = TRUE, tags = c("train", "super")),
        encapsulate_mbo = p_lgl(default = TRUE, tags = c("train", "super"))
      )

      param_set$set_values(
        learner_timeout = 900L,
        max_nthread = 1L,
        max_memory = 32000L,
        large_data_size = 1e6L,
        large_data_learner_ids = intersect(c("lda", "ranger", "xgboost", "catboost", "extra_trees", "lightgbm"), private$.learner_ids),
        large_data_nthread = 4L,
        small_data_size = 5000L,
        small_data_resampling = rsmp("cv", folds = 10L),
        max_cardinality = 100L,
        extra_trees_max_cardinality = 40L,
        resampling = rsmp("cv", folds = 3L),
        terminator = trm("run_time", secs = 14400L),
        measure = msr("classif.ce"),
        lhs_size = 4L,
        store_benchmark_result = FALSE,
        store_models = FALSE,
        encapsulate_learner = TRUE,
        encapsulate_mbo = TRUE)

      # subset to relevant parameters for selected learners
      param_set = param_set$subset(ids = unique(param_set$ids(any_tags = c("super", learner_ids))))

      self$graph = build_graph(private$.learner_ids, "classif")

      super$initialize(
        id = id,
        task_type = "classif",
        param_set = param_set,
        packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", self$graph$packages),
        feature_types = c("logical", "integer", "numeric", "character", "factor"),
        predict_types = c("response", "prob"),
        properties = c("missings", "weights", "twoclass", "multiclass"),
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
            graph_learner = value$graph_learner$clone(deep = TRUE),
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
learners[["classif.auto"]] = LearnerClassifAuto
