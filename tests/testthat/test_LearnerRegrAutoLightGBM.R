test_that("LearnerRegrAutoLightGBM is initialized", {
  learner = lrn("regr.auto_lightgbm",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerRegrAutoLightGBM is trained", {
  skip_on_cran()
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("mtcars")
  learner = lrn("regr.auto_lightgbm",
    lightgbm_eval_metric = "rmse",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 6),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerRegrAutoLightGBM")
  expect_equal(learner$graph$param_set$values$branch.selection, "lightgbm")
  expect_equal(learner$model$instance$result$branch.selection, "lightgbm")
})

test_that("LearnerRegrAutoLightGBM internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 1)

  task = tsk("mtcars")
  msrs_regr = rbindlist(list(
    list(measure = "regr.mse", metric = "mse"),
    list(measure = "regr.rmse", metric = "rmse"),
    list(measure = "regr.mae", metric = "mae"),
    list(measure = "regr.mape", metric = "mape")
  ))
  walk(seq_row(msrs_regr), function(i) {
    learner = lrn("regr.auto_lightgbm",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr(msrs_regr$measure[[i]]),
      terminator = trm("evals", n_evals = 1),
      store_benchmark_result = TRUE,
      store_models = TRUE,
      encapsulate_learner = FALSE,
      encapsulate_mbo = FALSE
    )
    learner$train(task)

    expect_equal(
      learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$lightgbm$param_vals$eval,
      # only for lightgbm, it is called `eval` instead of `eval_metric`
      msrs_regr$metric[[i]]
    )
  })
})
