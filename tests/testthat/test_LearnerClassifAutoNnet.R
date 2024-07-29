test_that("LearnerClassifAutoNnet is initialized", {
  learner = lrn("classif.auto_nnet",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_null(learner$graph)
  expect_null(learner$tuning_space)
})

test_that("LearnerClassifAutoNnet is trained", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.auto_nnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoNnet")
  expect_equal(learner$graph$param_set$values$branch.selection, "nnet")
  expect_equal(learner$model$instance$result$branch.selection, "nnet")
})
