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
  lgr::get_logger("mlr3automl")$set_threshold("debug")

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
