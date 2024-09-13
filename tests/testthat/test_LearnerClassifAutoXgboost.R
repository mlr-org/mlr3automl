test_that("LearnerClassifAutoXgboost is initialized", {
  learner = lrn("classif.auto_xgboost",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoXgboost is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto_xgboost",
    xgboost_eval_metric = "merror",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3)
  )

  expect_class(learner$train(task), "LearnerClassifAutoXgboost")
  expect_equal(learner$graph$param_set$values$branch.selection, "xgboost")
  expect_equal(learner$model$instance$result$branch.selection, "xgboost")
})
