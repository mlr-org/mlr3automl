test_that("LearnerClassifAutoXgboost is initialized", {
  learner = lrn("classif.auto_xgboost",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_null(learner$graph)
  expect_null(learner$tuning_space)
})

test_that("LearnerClassifAutoXgboost is trained", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.auto_xgboost",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  expect_class(learner$train(task), "LearnerClassifAutoXgboost")
  expect_equal(learner$graph$param_set$values$branch.selection, "xgboost")
  expect_equal(learner$model$instance$result$branch.selection, "xgboost")
})
