test_that("LearnerRegrAutoGlmnet is initialized", {
  learner = lrn("regr.auto_glmnet",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerRegrAutoGlmnet is trained", {
  skip_on_cran()
  skip_if_not_installed("glmnet")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("mtcars")
  learner = lrn("regr.auto_glmnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 6),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerRegrAutoGlmnet")
  expect_equal(learner$graph$param_set$values$branch.selection, "glmnet")
  expect_equal(learner$model$instance$result$branch.selection, "glmnet")
})
