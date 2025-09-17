test_that("LearnerClassifAutoXgboost is initialized", {
  learner = lrn("classif.auto_xgboost",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_learner(learner)
})

test_that("LearnerClassifAutoXgboost is trained", {
  skip_on_cran()
  skip_if_not_installed("xgboost")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto_xgboost",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAutoXgboost")
  expect_equal(learner$model$instance$result$branch.selection, "xgboost")
})

test_that("LearnerClassifAutoXgboost twoclass internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("xgboost")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 1)

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
      terminator = trm("evals", n_evals = 1),
      store_benchmark_result = TRUE,
      store_models = TRUE,
      encapsulate_learner = FALSE,
      encapsulate_mbo = FALSE
    )
    learner$train(task_twoclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$xgboost$param_vals$eval_metric,
      msrs_twoclass$metric[[i]]
    )
  })
})

test_that("LearnerClassifAutoXgboost multiclass internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("xgboost")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 1)

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
      terminator = trm("evals", n_evals = 1),
      store_benchmark_result = TRUE,
      store_models = TRUE,
      encapsulate_learner = FALSE,
      encapsulate_mbo = FALSE
    )
    learner$train(task_multiclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$xgboost$param_vals$eval_metric,
      msrs_multiclass$metric[[i]]
    )
  })
})
