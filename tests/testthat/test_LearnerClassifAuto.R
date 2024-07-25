test_that("initial design is generated", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost"))

  learner_ids = c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees")
  xdt = generate_default_design(
    task_type = "classif",
    learner_ids,
    task = tsk("sonar"),
    tuning_space)
  expect_data_table(xdt, nrows = length(learner_ids))
})

test_that("lhs design is generated", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost"))

  learner_ids =  c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost")
  xdt = generate_lhs_design(10, "classif", learner_ids, tuning_space)
  expect_data_table(xdt, nrows = 70)
})

test_that("LearnerClassifAuto is initialized", {
  learner = lrn("classif.auto",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_null(learner$graph)
  expect_null(learner$tuning_space)
})

test_that("glmnet works", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("glmnet")


  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "glmnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$graph$param_set$values$branch.selection, "glmnet")
  expect_equal(learner$model$instance$result$branch.selection, "glmnet")
})

test_that("kknn works", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("kknn")

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "kknn",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$graph$param_set$values$branch.selection, "kknn")
  expect_equal(learner$model$instance$result$branch.selection, "kknn")
})

test_that("only lda fails", {
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  expect_error(lrn("classif.auto",
    learner_ids = "lda",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  ), "must be combined with other learners")
})

test_that("lda and glmnet works", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("glmnet")


  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = c("lda", "glmnet"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_subset(learner$graph$param_set$values$branch.selection, c("glmnet", "lda"))
  expect_subset(learner$model$instance$result$branch.selection, c("glmnet", "lda"))
})

test_that("nnet works", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("nnet")

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "nnet",
    resampling = rsmp("holdout"),
    small_data_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$result$branch.selection, "nnet")
})

test_that("ranger works", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("ranger")


  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "ranger",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$result$branch.selection, "ranger")
})

test_that("svm works", {
  rush_plan(n_workers = 2)
  skip_if_not_installed("e1071")


  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "svm",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$result$branch.selection, "svm")
})

test_that("xgboost works", {
  skip_if_not_installed("xgboost")
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "xgboost",
    small_data_size = 1,
    xgboost_eval_metric = "mlogloss",
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$result$branch.selection, "xgboost")
})

test_that("catboost works", {
  skip_if_not_installed("catboost")
  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "catboost",
    small_data_size = 1,
    catboost_eval_metric = "MultiClass",
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$result$branch.selection, "catboost")
})

test_that("only extra_trees fails", {
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  expect_error(lrn("classif.auto",
    learner_ids = "extra_trees",
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  ), "must be combined with other learners")
})

test_that("extra_trees and glmnet works", {
  skip_if_not_installed("glmnet")
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = c("extra_trees", "glmnet"),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$result$branch.selection, "extra_trees")
})

test_that("lightgbm works", {
  skip_if_not_installed("lightgbm")
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "lightgbm",
    lightgbm_eval_metric = "multi_logloss",
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$result$branch.selection, "lightgbm")
})

test_that("xgboost, catboost and lightgbm work", {
  skip_if_not_installed(c("xgboost", "catboost", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = c("xgboost", "catboost", "lightgbm"),
    catboost_eval_metric = "MultiClass",
    lightgbm_eval_metric = "multi_logloss",
    xgboost_eval_metric = "mlogloss",
    resampling = rsmp("holdout"),
    lhs_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    callbacks = clbk("mlr3tuning.async_save_logs")
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("all learner work", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_class(learner$model$instance, "TuningInstanceAsyncSingleCrit")
  expect_prediction(learner$predict(task))
})

# test_that("memory limit works", {
#   skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
#   rush_plan(n_workers = 2)

#   task = tsk("spam")
#   learner = lrn("classif.auto",
#     max_memory = 50,
#     small_data_size = 100,
#     measure = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 20),
#     resampling = rsmp("holdout"),
#     lhs_size = 1
#   )

#   learner$train(task)
# })

test_that("small data set switch works", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    small_data_size = 1000,
    small_data_resampling = rsmp("cv", folds = 2),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$archive$benchmark_result$resamplings$resampling[[1]]$iters, 2)
})

test_that("large data set switch works", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    large_data_size = 100,
    large_data_nthread = 4,
    large_data_learner_ids = "ranger",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, "ranger")
})

test_that("max_cardinality works", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 2,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("max_cardinality works for extra trees", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 3,
    extra_trees_max_cardinality = 2,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("logger callback works", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    lhs_size = 1,
    callbacks = clbk("mlr3tuning.async_save_logs")
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_list(learner$instance$archive$data$log)
  expect_list(learner$instance$archive$data$log[[1]], len = 1)
})

# test_that("integer columns work", {
#   library(mlr3oml)
#   rush_plan(n_workers = 2)


#   task = tsk("oml", data_id = 1464)
#   learner = lrn("classif.auto",
#     learner_ids = "catboost",
#     small_data_size = 100,
#     measure = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 20),
#     lhs_size = 1
#   )

#   expect_class(learner$train(task), "LearnerClassifAuto")
# })

# test_that("constant columns work", {
#   library(mlr3oml)
#   rush_plan(n_workers = 2, lgr_thresholds = c(mlr3 = "info"))


#   task = tsk("oml", data_id = 41143)
#   learner = lrn("classif.auto",
#     learner_ids = "catboost",
#     small_data_size = 100,
#     measure = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 20),
#     lhs_size = 1
#   )

#   expect_class(learner$train(task), "LearnerClassifAuto")
# })
