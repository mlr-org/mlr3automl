test_that("glmnet works (regr)", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("glmnet")

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "glmnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$graph$param_set$values$branch.selection, "glmnet")
  expect_equal(learner$model$instance$result$branch.selection, "glmnet")
})

test_that("kknn works (regr)", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("kknn")

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "kknn",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$graph$param_set$values$branch.selection, "kknn")
  expect_equal(learner$model$instance$result$branch.selection, "kknn")
})

test_that("nnet works (regr)", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("nnet")

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "nnet",
    resampling = rsmp("holdout"),
    small_data_size = 1,
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$result$branch.selection, "nnet")
})

test_that("ranger works (regr)", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("ranger")


  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "ranger",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$result$branch.selection, "ranger")
})

test_that("svm works (regr)", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("e1071")


  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "svm",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$result$branch.selection, "svm")
})

test_that("xgboost works (regr)", {
  skip_if_not_installed("xgboost")
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "xgboost",
    small_data_size = 1,
    xgboost_eval_metric = "mlogloss",
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$result$branch.selection, "xgboost")
})

test_that("catboost works (regr)", {
  skip_if_not_installed("catboost")
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "catboost",
    small_data_size = 1,
    # catboost_eval_metric = "MultiClass",
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$result$branch.selection, "catboost")
})

test_that("only extra_trees fails", {
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  expect_error(lrn("regr.auto",
    learner_ids = "extra_trees",
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  ), "must be combined with other learners")
})

test_that("extra_trees and glmnet works (regr)", {
  skip_if_not_installed("glmnet")
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = c("extra_trees", "glmnet"),
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_choice(learner$model$instance$result$branch.selection, c("extra_trees", "glmnet"))
})

test_that("lightgbm works (regr)", {
  skip_if_not_installed("lightgbm")
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = "lightgbm",
    lightgbm_eval_metric = "multi_logloss",
    resampling = rsmp("holdout"),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$result$branch.selection, "lightgbm")
})

test_that("xgboost, catboost and lightgbm work (regr)", {
  skip_if_not_installed(c("xgboost", "catboost", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    learner_ids = c("xgboost", "catboost", "lightgbm"),
    # catboost_eval_metric = "MultiClass",
    # lightgbm_eval_metric = "multi_logloss",
    # xgboost_eval_metric = "mlogloss",
    resampling = rsmp("holdout"),
    lhs_size = 1,
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 20),
    callbacks = clbk("mlr3tuning.async_save_logs")
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
})

test_that("all learner work (regr)", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    small_data_size = 100,
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 20),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_class(learner$model$instance, "TuningInstanceAsyncSingleCrit")
  expect_prediction(learner$predict(task))
})

# test_that("memory limit works", {
#   skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
#   rush_plan(n_workers = 2)

#   task = tsk("spam")
#   learner = lrn("regr.auto",
#     max_memory = 50,
#     small_data_size = 100,
#     measure = msr("regr.mse"),
#     terminator = trm("evals", n_evals = 20),
#     resampling = rsmp("holdout"),
#     lhs_size = 1
#   )

#   learner$train(task)
# })

test_that("small data set switch works (regr)", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    small_data_size = 1000,
    small_data_resampling = rsmp("cv", folds = 2),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$archive$benchmark_result$resamplings$resampling[[1]]$iters, 2)
})

test_that("large data set switch works (regr)", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    large_data_size = 100,
    large_data_nthread = 4,
    large_data_learner_ids = "ranger",
    small_data_size = 100,
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, "ranger")
})

test_that("max_cardinality works (regr)", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 2,
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
})

test_that("max_cardinality works for extra trees (regr)", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("boston_housing")
  learner = lrn("regr.auto",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 3,
    extra_trees_max_cardinality = 2,
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
})
