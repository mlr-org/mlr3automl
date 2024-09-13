test_that("LearnerRegrAutoRanger is initialized", {
  learner = lrn("regr.auto_ranger",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerRegrAutoRanger is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("mtcars")
  learner = lrn("regr.auto_ranger",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAutoRanger")
  expect_equal(learner$graph$param_set$values$branch.selection, "ranger")
  expect_equal(learner$model$instance$result$branch.selection, "ranger")
})
