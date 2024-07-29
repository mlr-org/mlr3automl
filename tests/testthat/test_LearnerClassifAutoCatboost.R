test_that("LearnerClassifAutoCatboost is initialized", {
  learner = lrn("classif.auto_catboost",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_null(learner$graph)
  expect_null(learner$tuning_space)
})

test_that("LearnerClassifAutoCatboost is trained", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.auto_catboost",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoCatboost")
  expect_equal(learner$graph$param_set$values$branch.selection, "catboost")
  expect_equal(learner$model$instance$result$branch.selection, "catboost")
})
