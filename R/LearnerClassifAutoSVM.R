#' @title SVM Classification Auto Learner
#'
#' @description
#' AutoML pipeline for an SVM classifier with pre-processing operations.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#'
#' @export
LearnerClassifAutoSVM = R6Class("LearnerClassifAutoSVM",
  inherit = LearnerAutoSingle,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.automl_svm") {
      param_set = ps(
        # learner
        learner_id = p_uty(),
        learner_timeout = p_int(lower = 1L, default = 900L),
        # system
        max_nthread = p_int(lower = 1L, default = 1L),
        max_memory = p_int(lower = 1L, default = 32000L),
        # large data
        large_data_size = p_int(lower = 1L, default = 1e6),
        large_data_nthread = p_int(lower = 1L, default = 4L),
        # small data
        small_data_size = p_int(lower = 1L, default = 5000L),
        small_data_resampling = p_uty(),
        max_cardinality = p_int(lower = 1L, default = 100L),
        # tuner
        resampling = p_uty(),
        terminator = p_uty(),
        measure = p_uty(),
        lhs_size = p_int(lower = 1L, default = 4L),
        callbacks = p_uty(),
        store_benchmark_result = p_lgl(default = FALSE))

      param_set$set_values(
        learner_id = "svm",
        learner_timeout = 900L,
        max_nthread = 1L,
        max_memory = 32000L,
        large_data_size = 1e6L,
        large_data_nthread = 4L,
        small_data_size = 5000L,
        small_data_resampling = rsmp("cv", folds = 10L),
        max_cardinality = 100L,
        resampling = rsmp("cv", folds = 3L),
        terminator = trm("run_time", secs = 14400L),
        measure = msr("classif.ce"),
        lhs_size = 4L,
        store_benchmark_result = FALSE)

      graph = po("removeconstants", id = "svm_removeconstants") %>>%
        po("imputehist", id = "svm_imputehist") %>>%
        po("imputeoor", id = "svm_imputeoor") %>>%
        po("fixfactors", id = "svm_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "svm_collapse") %>>%
        po("encode", method = "one-hot", id = "svm_encode") %>>%
        po("removeconstants", id = "svm_post_removeconstants") %>>%
        lrn("classif.svm", id = "svm", type = "C-classification")

      # helper functions only support list-of-lists tuning spaces
      tuning_space = list(
        svm = list(
          svm.cost    = to_tune(1e-4, 1e4, logscale = TRUE),
          svm.kernel  = to_tune(c("polynomial", "radial", "sigmoid", "linear")),
          svm.degree  = to_tune(2, 5),
          svm.gamma   = to_tune(1e-4, 1e4, logscale = TRUE)
        )
      )

      super$initialize(
        id = id,
        task_type = "classif",
        param_set = param_set,
        graph = graph,
        tuning_space = tuning_space)
    }
  )
)

#' @include aaa.R
learners[["classif.automl_svm"]] = LearnerClassifAutoSVM
