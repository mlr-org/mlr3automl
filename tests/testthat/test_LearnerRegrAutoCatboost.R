test_that("LearnerRegrAutoCatboost is initialized", {
  learner = lrn("regr.auto_catboost",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerRegrAutoCatboost is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("mtcars")
  learner = lrn("regr.auto_catboost",
    catboost_eval_metric = "RMSE",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10)
  )

  expect_class(learner$train(task), "LearnerRegrAutoCatboost")
  expect_equal(learner$graph$param_set$values$branch.selection, "catboost")
  expect_equal(learner$model$instance$result$branch.selection, "catboost")
})

test_that("LearnerRegrAutoCaboost internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("mtcars")
  msrs_regr = rbindlist(list(
    list(measure = "regr.rmse", metric = "RMSE"),
    list(measure = "regr.mae", metric = "MAE"),
    list(measure = "regr.mape", metric = "MAPE"),
    list(measure = "regr.smape", metric = "SMAPE"),
    list(measure = "regr.medae", metric = "MedianAbsoluteError"),
    list(measure = "regr.rsq", metric = "R2")
  ))
  walk(seq_row(msrs_regr), function(i) {
    learner = lrn("regr.auto_catboost",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_regr$measure[[i]]),
      terminator = trm("evals", n_evals = 6),
      store_benchmark_result = TRUE,
      store_models = TRUE
    )
    learner$train(task)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$catboost$param_vals$eval,
      msrs_regr$metric[[i]]
    )
  })
})
