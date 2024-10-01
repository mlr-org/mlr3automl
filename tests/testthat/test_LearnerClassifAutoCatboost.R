test_that("LearnerClassifAutoCatboost is initialized", {
  learner = lrn("classif.auto_catboost",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoCatboost is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto_catboost",
    catboost_eval_metric = "Accuracy",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoCatboost")
  expect_equal(learner$graph$param_set$values$branch.selection, "catboost")
  expect_equal(learner$model$instance$result$branch.selection, "catboost")
})

test_that("LearnerClassifAutoCatboost twoclass internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("pima")

  msrs_twoclass = rbindlist(list(
    list(measure = "classif.ce", metric = "Accuracy"),
    list(measure = "classif.acc", metric = "Accuracy"),
    list(measure = "classif.bacc", metric = "BalancedAccuracy"),
    list(measure = "classif.auc", metric = "AUC"),
    list(measure = "classif.prauc", metric = "aucpr"),
    list(measure = "classif.bbrier", metric = "BrierScore"),
    list(measure = "classif.logloss", metric = "Logloss"),
    list(measure = "classif.precision", metric = "Precision"),
    list(measure = "classif.recall", metric = "Recall"),
    list(measure = "classif.mcc", metric = "MCC")
  ))
  walk(seq_row(msrs_twoclass), function(i) {
    learner = lrn("classif.auto_catboost",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_twoclass$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$catboost$param_vals$eval_metric,
      msrs_twoclass$metric[[i]]
    )
  })
})

test_that("LearnerClassifAutoCatboost multiclass internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task_multiclass = tsk("penguins")

  msrs_multiclass = rbindlist(list(
    list(measure = "classif.ce", metric = "Accuracy"),
    list(measure = "classif.acc", metric = "Accuracy"),
    list(measure = "classif.mauc_mu", metric = "AUC"),
    list(measure = "classif.logloss", metric = "MultiClass"),
    list(measure = "classif.mcc", metric = "MCC")
  ))
  walk(seq_row(msrs_multiclass), function(i) {
    learner = lrn("classif.auto_catboost",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_multiclass$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task_multiclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$catboost$param_vals$eval_metric,
      msrs_multiclass$metric[[i]]
    )
  })
})  
