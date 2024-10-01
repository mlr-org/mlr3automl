test_that("LearnerClassifAutoXgboost is initialized", {
  learner = lrn("classif.auto_xgboost",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoXgboost is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto_xgboost",
    xgboost_eval_metric = "merror",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  expect_class(learner$train(task), "LearnerClassifAutoXgboost")
  expect_equal(learner$graph$param_set$values$branch.selection, "xgboost")
  expect_equal(learner$model$instance$result$branch.selection, "xgboost")
})

test_that("LearnerClassifAutoXgboost internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  # twoclass
  task_twoclass = tsk("pima")
  msrs_twoclass = rbindlist(list(
    list(measure = "classif.ce", metric = "error"),
    list(measure = "classif.acc", metric = "error"),
    list(measure = "classif.auc", metric = "auc"),
    list(measure = "classif.prauc", metric = "aucpr"),
    list(measure = "classif.logloss", metric = "logloss")
  ))
  walk(seq_row(msrs_twoclass), function(i) {
    learner = lrn("classif.auto_xgboost",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_twoclass$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task_twoclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$xgboost$param_vals$eval_metric,
      # only for xgboost, it is called `eval` instead of `eval_metric`
      msrs_twoclass$metric[[i]]
    )
  })

  # multiclass
  task_multiclass = tsk("penguins")
  msrs_multiclass = rbindlist(list(
    list(measure = "classif.ce", metric = "merror"),
    list(measure = "classif.acc", metric = "merror"),
    list(measure = "classif.mauc_aunp", metric = "auc"),
    list(measure = "classif.logloss", metric = "mlogloss")
  ))
  walk(seq_row(msrs_multiclass), function(i) {
    # twoclass
    learner = lrn("classif.auto_xgboost",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_multiclass$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task_multiclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$xgboost$param_vals$eval_metric,
      # only for xgboost, it is called `eval` instead of `eval_metric`
      msrs_multiclass$metric[[i]]
    )
  })
})
