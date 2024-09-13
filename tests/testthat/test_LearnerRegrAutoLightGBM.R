test_that("LearnerRegrAutoLightGBM is initialized", {
  learner = lrn("regr.auto_lightgbm",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerRegrAutoLightGBM is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("mtcars")
  learner = lrn("regr.auto_lightgbm",
    lightgbm_eval_metric = "rmse",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAutoLightGBM")
  expect_equal(learner$graph$param_set$values$branch.selection, "lightgbm")
  expect_equal(learner$model$instance$result$branch.selection, "lightgbm")
})
