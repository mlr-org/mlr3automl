test_that("classif lightgbm internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task_twoclass = tsk("pima")
  task_multiclass = tsk("penguins")

  msrs_classif_dt = rbindlist(list(
    list(measure = "classif.ce", twoclass = "binary_error", multiclass = "multi_error"),
    list(measure = "classif.acc", twoclass = "binary_error", multiclass = "multi_error"),
    list(measure = "classif.logloss", twoclass = "binary_logloss", multiclass = "multi_logloss"),
    list(measure = "classif.auc", twoclass = "auc", multiclass = "auc_mu")
  ))

  walk(seq_row(msrs_classif_dt), function(i) {
    # twoclass
    learner = lrn("classif.auto_lightgbm",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_classif_dt$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task_twoclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$lightgbm$param_vals$eval,
      # only for lightgbm, it is called `eval` instead of `eval.metric`
      msrs_classif_dt$twoclass[[i]]
    )

    # multiclass
    learner = lrn("classif.auto_lightgbm",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_classif_dt$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task_multiclass)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$lightgbm$param_vals$eval,
      # only for lightgbm, it is called `eval` instead of `eval.metric`
      msrs_classif_dt$multiclass[[i]]
    )
  })
})

test_that("regr lightgbm internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("mtcars")

  msrs_regr_dt = rbindlist(list(
    list(measure = "regr.mse", metric = "mse"),
    list(measure = "regr.rmse", metric = "rmse"),
    list(measure = "regr.mae", metric = "mae"),
    list(measure = "regr.mape", metric = "mape")
  ))

  walk(seq_row(msrs_regr_dt), function(i) {
    learner = lrn("regr.auto_lightgbm",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_regr_dt$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$lightgbm$param_vals$eval,
      # only for lightgbm, it is called `eval` instead of `eval.metric`
      msrs_regr_dt$metric[[i]]
    )
  })
})

# test_that("lightgbm not supported internal eval metric throws error", {
#   skip_on_cran()
#   skip_if_not_installed("rush")
#   flush_redis()

#   rush_plan(n_workers = 2)

#   task = tsk("penguins")
#   learner = lrn("classif.auto_lightgbm",
#     small_data_size = 1,
#     resampling = rsmp("holdout"),
#     measure = msr("classif.mbrier"),
#     terminator = trm("evals", n_evals = 6),
#     store_benchmark_result = TRUE,
#     store_models = TRUE
#   )

#   expect_error(learner$train(task), "No suitable lightgbm eval metric found")
# })
