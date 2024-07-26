build_graph = function(learner_ids, task_type) {
  assert_choice(task_type, c("classif", "regr"))
  learners_reg = c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm")
  if (task_type == "regr") {
    assert_subset(learner_ids, learners_reg)
  } else {
    assert_subset(learner_ids, c(learners_reg, "lda"))
  }

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
      lrn(paste0(task_type, ".xgboost"), id = "xgboost", nrounds = 5000, early_stopping_rounds = 10)
    branches = c(branches, branch_xgboost)
  }

  # catboost
  if ("catboost" %in% learner_ids) {
    branch_catboost = po("colapply", id = "catboost_colapply", applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
      lrn(paste0(task_type, ".catboost"), id = "catboost", iterations = 500, early_stopping_rounds = 10, use_best_model = TRUE)
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
    branch_lightgbm = lrn(paste0(task_type, ".lightgbm"), id = "lightgbm", num_iterations = 5000, early_stopping_rounds = 10)
    branches = c(branches, branch_lightgbm)
  }

  # branch graph
  po("branch", options = learner_ids) %>>%
    gunion(branches) %>>%
    po("unbranch", options = learner_ids)
}

generate_default_design = function(task_type, learner_ids, task, tuning_space, branch = TRUE) {
  map_dtr(learner_ids, function(learner_id) {
    if (paste0(task_type, ".", learner_id) %nin% mlr_learners$keys()) {
      return(data.table(branch.selection = learner_id))
    }

    learner = lrn(sprintf("%s.%s", task_type, learner_id))
    token = tuning_space[[learner_id]]

    # learner without tuning space
    if (!length(token)) {
      return(data.table(branch.selection = learner_id))
    }

    names(token) = gsub(paste0("^", learner_id, "."), "", names(token))
    learner$param_set$set_values(.values = token)
    search_space = learner$param_set$search_space()
    xss = default_values(learner, search_space = search_space, task = task)
    has_logscale = map_lgl(search_space$params$.trafo, function(x) identical(x, exp))
    xdt = as.data.table(map_if(xss, has_logscale, function(value) if (value > 0) log(value) else value))

    setnames(xdt, sprintf("%s.%s", learner_id, names(xdt)))

    if (branch) {
      set(xdt, j = "branch.selection", value = learner_id)
    }
    xdt
  }, .fill = TRUE)
}

generate_lhs_design = function(size, task_type, learner_ids, tuning_space, branch = TRUE) {
  if (!size) return(data.table())
  learner_ids = learner_ids[learner_ids %in% names(tuning_space)]

  map_dtr(learner_ids, function(learner_id) {
    learner = lrn(sprintf("%s.%s", task_type, learner_id))

    token = tuning_space[[learner_id]]
    # learner without tuning space
    if (!length(token)) {
      return(data.table(branch.selection = learner_id))
    }

    names(token) = gsub(paste0("^", learner_id, "."), "", names(token))
    learner$param_set$set_values(.values = token)
    search_space = learner$param_set$search_space()
    xdt = generate_design_lhs(search_space, size)$data
    setnames(xdt, sprintf("%s.%s", learner_id, names(xdt)))
    if (branch) {
      set(xdt, j = "branch.selection", value = learner_id)
    }
    xdt
  }, .fill = TRUE)
}

default_surrogate = function(instance = NULL, learner = NULL, n_learner = NULL, search_space = NULL, noisy = NULL) {
  assert_r6(instance, "OptimInstance", null.ok = TRUE)
  assert_r6(learner, "Learner", null.ok = TRUE)
  if (is.null(instance)) {
    noisy = assert_flag(noisy)
    search_space = assert_param_set(search_space)
    assert_int(n_learner, lower = 1L)
  } else {
    noisy = "noisy" %in% instance$objective$properties
    search_space = instance$search_space
    assert_int(n_learner, lower = 1L, null.ok = TRUE)
  }

  if (is.null(learner)) {
    is_mixed_space = !all(search_space$class %in% c("ParamDbl", "ParamInt"))
    has_deps = nrow(search_space$deps) > 0L
    require_namespaces("mlr3learners")
    if (!is_mixed_space) {
      require_namespaces("DiceKriging")
      learner = mlr3learners::LearnerRegrKM$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE))
      )
      if (noisy) {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.estim = TRUE, jitter = 1e-12))
      } else {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.stability = 10^-8))
      }
    } else {
      require_namespaces("ranger")
      learner = mlr3learners::LearnerRegrRanger$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(num.trees = 500L, keep.inbag = TRUE, se.method = "jack")
      )
    }
    # Stability: evaluate and add a fallback
    learner$encapsulate[c("train", "predict")] = "evaluate"
    require_namespaces("ranger")
    fallback = mlr3learners::LearnerRegrRanger$new()
    fallback$param_set$values = insert_named(
      fallback$param_set$values,
      list(num.trees = 20L, keep.inbag = TRUE, se.method = "jack")
    )
    learner$fallback = fallback

    if (has_deps) {
      require_namespaces("mlr3pipelines")
      learner = mlr3pipelines::GraphLearner$new(
        mlr3pipelines::"%>>%"(
          mlr3pipelines::"%>>%"(
            mlr3pipelines::po("imputesample", affect_columns = mlr3pipelines::selector_type("logical")),
              mlr3pipelines::"%>>%"(
                mlr3pipelines::po("imputeoor", multiplier = 3, affect_columns = mlr3pipelines::selector_type(c("integer", "numeric", "character", "factor", "ordered"))),
                mlr3pipelines::po("colapply", applicator = as.factor, affect_columns = mlr3pipelines::selector_type("character"))
              )
          ),
          learner
        )
      )
      learner$encapsulate[c("train", "predict")] = "evaluate"
      learner$fallback = LearnerRegrFeatureless$new()
    }
  }

  if (is.null(n_learner)) n_learner = length(instance$archive$cols_y)
  if (n_learner == 1L) {
    SurrogateLearner$new(learner)
  } else  {
    learners = replicate(n_learner, learner$clone(deep = TRUE), simplify = FALSE)
    SurrogateLearnerCollection$new(learners)
  }
}

cb_timeout_xgboost = function(timeout) {
  callback = function(env = parent.frame()) {
    if (is.null(env$start_time)) {
      env$start_time <- Sys.time()
    }

    if (difftime(Sys.time(), env$start_time, units = "secs") > timeout) {
      message("Timeout reached")
      env$stop_condition <- TRUE
    } else {
      env$stop_condition <- FALSE
    }
  }
  attr(callback, 'call') = match.call()
  attr(callback, 'name') = 'cb_timeout_xgboost'
  callback
}


cb_timeout_lightgbm <- function(timeout) {

  callback = function(env) {
    if (!exists("start_time")) start_time <<- Sys.time()

    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      message("Timeout reached")
      env$met_early_stop = TRUE
    } else {
      env$met_early_stop = FALSE
    }
  }

  attr(callback, "call") <- match.call()
  attr(callback, "name") <- "cb_timeout_lightgbm"

  return(callback)
}
