test_that("LearnerClassifAutoCatboost is initialized", {
  learner = lrn("classif.auto_catboost",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoCatboost is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.auto_catboost",
    catboost_eval_metric = "Accuracy",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoCatboost")
  expect_equal(learner$graph$param_set$values$branch.selection, "catboost")
  expect_equal(learner$model$instance$result$branch.selection, "catboost")
})

test_that("internal eval metric is found", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.auto_catboost",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6),
    store_benchmark_result = TRUE,
    store_models = TRUE
  )

  learner$train(task)
  expect_equal()

  expect_equal(learner$instance$archive$benchmark_result$resample_result(1)$learners[[1]]$model$catboost$param_vals$eval_metric, "Accuracy")

  learner = lrn("classif.auto_catboost",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.mbrier"),
    terminator = trm("evals", n_evals = 6),
    store_benchmark_result = TRUE,
    store_models = TRUE
  )

  expect_error(learner$train(task), "No suitable catboost eval metric found")
})
