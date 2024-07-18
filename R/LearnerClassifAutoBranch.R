#' @title Classification Auto Learner with Multiple Learners
#'
#' @description
#' Classification Auto learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#'
#' @export
LearnerClassifAutoBranch = R6Class("LearnerClassifAutoBranch",
  inherit = LearnerAutoBranch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.automl_branch") {
      param_set = ps(
        # learner
        learner_ids = p_uty(),
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

      learner_ids = c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm")
      param_set$set_values(
        learner_ids = learner_ids,
        learner_timeout = 900L,
        max_nthread = 1L,
        max_memory = 32000L,
        large_data_size = 1e6L,
        large_data_learner_ids = c("lda", "ranger", "xgboost", "catboost", "extra_trees", "lightgbm"),
        large_data_nthread = 4L,
        small_data_size = 5000L,
        small_data_resampling = rsmp("cv", folds = 10L),
        max_cardinality = 100L,
        extra_trees_max_cardinality = 40L,
        resampling = rsmp("cv", folds = 3L),
        terminator = trm("run_time", secs = 14400L),
        measure = msr("classif.ce"),
        lhs_size = 4L,
        store_benchmark_result = FALSE)

      # glmnet
      branch_glmnet =
        po("removeconstants", id = "glmnet_removeconstants") %>>%
        po("imputehist", id = "glmnet_imputehist") %>>%
        po("imputeoor", id = "glmnet_imputeoor") %>>%
        po("fixfactors", id = "glmnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "glmnet_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "glmnet_collapse") %>>%
        po("encode", method = "one-hot", id = "glmnet_encode") %>>%
        po("removeconstants", id = "glmnet_post_removeconstants") %>>%
        lrn("classif.glmnet", id = "glmnet")

      # kknn
      branch_kknn = po("removeconstants", id = "kknn_removeconstants") %>>%
        po("imputehist", id = "kknn_imputehist") %>>%
        po("imputeoor", id = "kknn_imputeoor") %>>%
        po("fixfactors", id = "kknn_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "kknn_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "kknn_collapse") %>>%
        po("removeconstants", id = "kknn_post_removeconstants") %>>%
        lrn("classif.kknn", id = "kknn")

      # lda
      branch_lda = po("removeconstants", id = "lda_removeconstants") %>>%
        po("imputehist", id = "lda_imputehist") %>>%
        po("imputeoor", id = "lda_imputeoor") %>>%
        po("fixfactors", id = "lda_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "lda_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "lda_collapse") %>>%
        po("removeconstants", id = "lda_post_removeconstants") %>>%
        lrn("classif.lda", id = "lda")

      # nnet
      branch_nnet = po("removeconstants", id = "nnet_removeconstants") %>>%
        po("imputehist", id = "nnet_imputehist") %>>%
        po("imputeoor", id = "nnet_imputeoor") %>>%
        po("fixfactors", id = "nnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "nnet_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "nnet_collapse") %>>%
        po("removeconstants", id = "nnet_post_removeconstants") %>>%
        lrn("classif.nnet", id = "nnet")

      # ranger
      branch_ranger = po("removeconstants", id = "ranger_removeconstants") %>>%
        po("imputeoor", id = "ranger_imputeoor") %>>%
        po("fixfactors", id = "ranger_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "ranger_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "ranger_collapse") %>>%
        po("removeconstants", id = "ranger_post_removeconstants") %>>%
        lrn("classif.ranger", id = "ranger", num.trees = 2000) # use upper bound of search space for memory estimation

      # svm
      branch_svm = po("removeconstants", id = "svm_removeconstants") %>>%
        po("imputehist", id = "svm_imputehist") %>>%
        po("imputeoor", id = "svm_imputeoor") %>>%
        po("fixfactors", id = "svm_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "svm_collapse") %>>%
        po("encode", method = "one-hot", id = "svm_encode") %>>%
        po("removeconstants", id = "svm_post_removeconstants") %>>%
        lrn("classif.svm", id = "svm", type = "C-classification")

      # xgboost
      branch_xgboost = po("removeconstants", id = "xgboost_removeconstants") %>>%
        po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("fixfactors", id = "xgboost_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
        po("encodeimpact", id = "xgboost_encode") %>>%
        po("removeconstants", id = "xgboost_post_removeconstants") %>>%
        lrn("classif.xgboost", id = "xgboost", nrounds = 5000, early_stopping_rounds = 10)

      # catboost
      branch_catboost = po("colapply", applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
        lrn("classif.catboost", id = "catboost", iterations = 500, early_stopping_rounds = 10, use_best_model = TRUE)

      # extra trees
      branch_extra_trees = po("removeconstants", id = "extra_trees_removeconstants") %>>%
        po("imputeoor", id = "extra_trees_imputeoor") %>>%
        po("fixfactors", id = "extra_trees_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "extra_trees_imputesample") %>>%
        po("collapsefactors", target_level_count = 40, id = "extra_trees_collapse") %>>%
        po("removeconstants", id = "extra_trees_post_removeconstants") %>>%
        lrn("classif.ranger", id = "extra_trees", splitrule = "extratrees", num.trees = 100, replace = FALSE, sample.fraction = 1)

      # lightgbm
      branch_lightgbm = lrn("classif.lightgbm", id = "lightgbm", num_iterations = 5000, early_stopping_rounds = 10)

      # branch graph
      graph = po("branch", options = learner_ids) %>>%
        gunion(list(
          branch_glmnet,
          branch_kknn,
          branch_lda,
          branch_nnet,
          branch_ranger,
          branch_svm,
          branch_xgboost,
          branch_catboost,
          branch_extra_trees,
          branch_lightgbm)) %>>%
        po("unbranch", options = learner_ids)

      super$initialize(
        id = id,
        task_type = "classif",
        param_set = param_set,
        graph = graph,
        tuning_space = tuning_space)
    }
  )
)

tuning_space = list(
  glmnet = list(
    glmnet.s     = to_tune(1e-4, 1e4, logscale = TRUE),
    glmnet.alpha = to_tune(0, 1)
  ),

  kknn = list(
    kknn.k = to_tune(1, 50, logscale = TRUE),
    kknn.distance = to_tune(1, 5),
    kknn.kernel = to_tune(c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank"))
  ),

  nnet = list(
      nnet.maxit = to_tune(1e1, 1e3, logscale = TRUE),
      nnet.decay = to_tune(1e-4, 1e-1, logscale = TRUE),
      nnet.size  = to_tune(2, 50, logscale = TRUE)
  ),

  ranger = list(
    ranger.mtry.ratio      = to_tune(0, 1),
    ranger.replace         = to_tune(),
    ranger.sample.fraction = to_tune(1e-1, 1),
    ranger.num.trees       = to_tune(500, 2000)
  ),

  svm = list(
    svm.cost    = to_tune(1e-4, 1e4, logscale = TRUE),
    svm.kernel  = to_tune(c("polynomial", "radial", "sigmoid", "linear")),
    svm.degree  = to_tune(2, 5),
    svm.gamma   = to_tune(1e-4, 1e4, logscale = TRUE)
  ),

  xgboost = list(
    xgboost.eta               = to_tune(1e-4, 1, logscale = TRUE),
    xgboost.max_depth         = to_tune(1, 20),
    xgboost.colsample_bytree  = to_tune(1e-1, 1),
    xgboost.colsample_bylevel = to_tune(1e-1, 1),
    xgboost.lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
    xgboost.alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
    xgboost.subsample         = to_tune(1e-1, 1),
    xgboost.nrounds           = to_tune(1, 5000, internal = TRUE)
  ),

  catboost = list(
    catboost.depth          = to_tune(5, 8),
    catboost.learning_rate  = to_tune(5e-3, 0.2, logscale = TRUE),
    catboost.l2_leaf_reg    = to_tune(1, 5),
    catboost.iterations     = to_tune(1, 500, internal = TRUE)
  ),


  lightgbm = list(
    lightgbm.learning_rate    = to_tune(5e-3, 0.2, logscale = TRUE),
    lightgbm.feature_fraction = to_tune(0.75, 1),
    lightgbm.min_data_in_leaf = to_tune(2, 60),
    lightgbm.num_leaves       = to_tune(16, 96),
    lightgbm.num_iterations   = to_tune(1, 5000, internal = TRUE)
  )
)

#' @include aaa.R
learners[["classif.automl_branch"]] = LearnerClassifAutoBranch
