build_graph = function(learner_ids, task_type) {
  branches = list()

  # glmnet
  if ("glmnet" %in% learner_ids) {
    branch_glmnet = po("removeconstants", id = "glmnet_removeconstants") %>>%
      po("imputehist", id = "glmnet_imputehist") %>>%
      po("imputeoor", id = "glmnet_imputeoor") %>>%
      po("fixfactors", id = "glmnet_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "glmnet_imputesample") %>>%
      po("collapsefactors", target_level_count = 100, id = "glmnet_collapse") %>>%
      po("encode", method = "one-hot", id = "glmnet_encode") %>>%
      po("removeconstants", id = "glmnet_post_removeconstants") %>>%
      lrn(paste0(task_type, ".glmnet"), id = "glmnet")
    branches = c(branches, branch_glmnet)
  }

  # kknn
  if ("kknn" %in% learner_ids) {
    branch_kknn = po("removeconstants", id = "kknn_removeconstants") %>>%
      po("imputehist", id = "kknn_imputehist") %>>%
      po("imputeoor", id = "kknn_imputeoor") %>>%
      po("fixfactors", id = "kknn_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "kknn_imputesample") %>>%
      po("collapsefactors", target_level_count = 100, id = "kknn_collapse") %>>%
      po("removeconstants", id = "kknn_post_removeconstants") %>>%
      lrn(paste0(task_type, ".kknn"), id = "kknn")
    branches = c(branches, branch_kknn)
  }

  # lda
  # only for classification
  if ("lda" %in% learner_ids) {
    branch_lda = po("removeconstants", id = "lda_removeconstants") %>>%
      po("imputehist", id = "lda_imputehist") %>>%
      po("imputeoor", id = "lda_imputeoor") %>>%
      po("fixfactors", id = "lda_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "lda_imputesample") %>>%
      po("collapsefactors", target_level_count = 100, id = "lda_collapse") %>>%
      po("removeconstants", id = "lda_post_removeconstants") %>>%
      lrn("classif.lda", id = "lda")
    branches = c(branches, branch_lda)
  }

  # nnet
  if ("nnet" %in% learner_ids) {
    branch_nnet = po("removeconstants", id = "nnet_removeconstants") %>>%
      po("imputehist", id = "nnet_imputehist") %>>%
      po("imputeoor", id = "nnet_imputeoor") %>>%
      po("fixfactors", id = "nnet_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "nnet_imputesample") %>>%
      po("collapsefactors", target_level_count = 100, id = "nnet_collapse") %>>%
      po("removeconstants", id = "nnet_post_removeconstants") %>>%
      lrn(paste0(task_type, ".nnet"), id = "nnet")
    branches = c(branches, branch_nnet)
  }

  # ranger
  if ("ranger" %in% learner_ids) {
    branch_ranger = po("removeconstants", id = "ranger_removeconstants") %>>%
      po("imputeoor", id = "ranger_imputeoor") %>>%
      po("fixfactors", id = "ranger_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "ranger_imputesample") %>>%
      po("collapsefactors", target_level_count = 100, id = "ranger_collapse") %>>%
      po("removeconstants", id = "ranger_post_removeconstants") %>>%
      # use upper bound of search space for memory estimation
      lrn(paste0(task_type, ".ranger"), id = "ranger", num.trees = 2000)
    branches = c(branches, branch_ranger)
  }

  # svm
  if ("svm" %in% learner_ids) {
    svm_type = if (task_type == "classif") {
      "C-classification"
    } else {
      "eps-regression"
    }
    branch_svm = po("removeconstants", id = "svm_removeconstants") %>>%
      po("imputehist", id = "svm_imputehist") %>>%
      po("imputeoor", id = "svm_imputeoor") %>>%
      po("fixfactors", id = "svm_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
      po("collapsefactors", target_level_count = 100, id = "svm_collapse") %>>%
      po("encode", method = "one-hot", id = "svm_encode") %>>%
      po("removeconstants", id = "svm_post_removeconstants") %>>%
      lrn(paste0(task_type, ".svm"), id = "svm", type = svm_type)
    branches = c(branches, branch_svm)
  }

  # xgboost
  if ("xgboost" %in% learner_ids) {
    branch_xgboost = po("removeconstants", id = "xgboost_removeconstants") %>>%
      po("imputeoor", id = "xgboost_imputeoor") %>>%
      po("fixfactors", id = "xgboost_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
      po("encodeimpact", id = "xgboost_encode") %>>%
      po("removeconstants", id = "xgboost_post_removeconstants") %>>%
      lrn(paste0(task_type, ".xgboost"), id = "xgboost", nrounds = 5000, early_stopping_rounds = 100)
    branches = c(branches, branch_xgboost)
  }

  # catboost
  if ("catboost" %in% learner_ids) {
    branch_catboost = po("colapply", id = "catboost_colapply", applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
      lrn(paste0(task_type, ".catboost"), id = "catboost", iterations = 500, early_stopping_rounds = 100, use_best_model = TRUE)
    branches = c(branches, branch_catboost)
  }

  # extra trees
  if ("extra_trees" %in% learner_ids) {
    branch_extra_trees = po("removeconstants", id = "extra_trees_removeconstants") %>>%
      po("imputeoor", id = "extra_trees_imputeoor") %>>%
      po("fixfactors", id = "extra_trees_fixfactors") %>>%
      po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "extra_trees_imputesample") %>>%
      po("collapsefactors", target_level_count = 40, id = "extra_trees_collapse") %>>%
      po("removeconstants", id = "extra_trees_post_removeconstants") %>>%
      lrn(paste0(task_type, ".ranger"), id = "extra_trees", splitrule = "extratrees", num.trees = 100, replace = FALSE, sample.fraction = 1)
    branches = c(branches, branch_extra_trees)
  }

  # lightgbm
  if ("lightgbm" %in% learner_ids) {
    branch_lightgbm = lrn(paste0(task_type, ".lightgbm"), id = "lightgbm", num_iterations = 5000, early_stopping_rounds = 100)
    branches = c(branches, branch_lightgbm)
  }

  # branch graph
  po("branch", options = learner_ids) %>>%
    gunion(branches) %>>%
    po("unbranch", options = learner_ids)
}

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

  lda = list(),

  extra_trees = list(),

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
    xgboost.nrounds           = to_tune(1, 10, internal = TRUE)
  ),

  catboost = list(
    catboost.depth          = to_tune(5, 8),
    catboost.learning_rate  = to_tune(5e-3, 0.2, logscale = TRUE),
    catboost.l2_leaf_reg    = to_tune(1, 5),
    catboost.iterations     = to_tune(1, 10, internal = TRUE)
  ),

  lightgbm = list(
    lightgbm.learning_rate    = to_tune(5e-3, 0.2, logscale = TRUE),
    lightgbm.feature_fraction = to_tune(0.75, 1),
    lightgbm.min_data_in_leaf = to_tune(2, 60),
    lightgbm.num_leaves       = to_tune(16, 96),
    lightgbm.num_iterations   = to_tune(1, 10, internal = TRUE)
  )
)
