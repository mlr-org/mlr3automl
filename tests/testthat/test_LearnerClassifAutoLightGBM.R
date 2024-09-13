test_that("LearnerClassifAutoLightGBM is initialized", {
  learner = lrn("classif.auto_lightgbm",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoLightGBM is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto_lightgbm",
    lightgbm_eval_metric = "multi_error",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoLightGBM")
  expect_equal(learner$graph$param_set$values$branch.selection, "lightgbm")
  expect_equal(learner$model$instance$result$branch.selection, "lightgbm")
})
