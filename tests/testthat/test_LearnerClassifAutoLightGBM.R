test_that("LearnerClassifAutoLightGBM is initialized", {
  learner = lrn("classif.auto_lightgbm",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoLightGBM is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto_lightgbm",
    lightgbm_eval_metric = "multi_error",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoLightGBM")
  expect_equal(learner$graph$param_set$values$branch.selection, "lightgbm")
  expect_equal(learner$model$instance$result$branch.selection, "lightgbm")
})

test_that("LearnerClassifAutoLightGBM internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  # twoclass
  task_twoclass = tsk("pima")
  msrs_twoclass = rbindlist(list(
    list(measure = "classif.ce", metric = "binary_error"),
    list(measure = "classif.acc", metric = "binary_error"),
    list(measure = "classif.logloss", metric = "binary_logloss"),
    list(measure = "classif.auc", metric = "auc")
  ))  
  walk(seq_row(msrs_twoclass), function(i) {
    learner = lrn("classif.auto_lightgbm",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_twoclass$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task_twoclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$lightgbm$param_vals$eval,
      # only for lightgbm, it is called `eval` instead of `eval.metric`
      msrs_twoclass$metric[[i]]
    )
  })

  # multi class  
  task_multiclass = tsk("penguins")
  msrs_multiclass = rbindlist(list(
    list(measure = "classif.ce", metric = "multi_error"),
    list(measure = "classif.acc", metric = "multi_error"),
    list(measure = "classif.logloss", metric = "multi_logloss"),
    list(measure = "classif.mauc_mu", metric = "auc_mu")
  ))
  walk(seq_row(msrs_multiclass), function(i) {
    learner = lrn("classif.auto_lightgbm",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_multiclass$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task_multiclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$lightgbm$param_vals$eval,
      # only for lightgbm, it is called `eval` instead of `eval.metric`
      msrs_multiclass$metric[[i]]
    )
  })
})
