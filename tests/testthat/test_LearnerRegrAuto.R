test_that("initial design is generated", {
  skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost"))

  learner_ids = c("glmnet", "kknn", "lda", "ranger", "svm", "xgboost", "catboost", "extra_trees")
  xdt = generate_default_design(
    task_type = "regr",
    learner_ids,
    task = tsk("sonar"),
    tuning_space)
  expect_data_table(xdt, nrows = length(learner_ids))
})

test_that("lhs design is generated", {
  skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost"))

  learner_ids =  c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost")
  xdt = generate_lhs_design(10, "regr", learner_ids, tuning_space)
  expect_data_table(xdt, nrows = 60)
})

test_that("lhs design is generated with size smaller than the maximum number of levels", {
  skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost"))

  learner_ids =  c("glmnet", "kknn",  "ranger", "svm", "xgboost", "catboost", "lightgbm")
  xdt = generate_lhs_design(1, "regr", learner_ids, tuning_space)
  expect_data_table(xdt, nrows = 19)
})

test_that("LearnerRegrAuto is initialized", {
  learner = lrn("regr.auto",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("glmnet works", {
  test_regr_learner("glmnet")
})

test_that("kknn works", {
  test_regr_learner("kknn", n_evals = 10)
})

test_that("ranger works", {
  test_regr_learner("ranger")
})

test_that("svm works", {
  test_regr_learner("svm")
})

test_that("xgboost works", {
  test_regr_learner("xgboost")

})

test_that("catboost works", {
  test_regr_learner("catboost")

})

test_that("only extra_trees fails", {
  rush_plan(n_workers = 2)

  task = tsk("california_housing")$filter(sample(1000))
  expect_error(lrn("regr.auto",
    learner_ids = "extra_trees",
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 6)
  ), "must be combined with other learners")
})

test_that("extra_trees and glmnet works", {
  test_regr_learner(c("extra_trees", "glmnet"))
})

test_that("lightgbm works", {
  test_regr_learner("lightgbm")
})

test_that("xgboost, catboost and lightgbm work", {
  test_regr_learner(c("xgboost", "catboost", "lightgbm"), n_evals = 20)
})

test_that("all learner work", {
  skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  rush_plan(n_workers = 2)

  task = tsk("california_housing")$filter(sample(1000))
  learner = lrn("regr.auto",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 20),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_class(learner$model$instance, "TuningInstanceAsyncSingleCrit")
  expect_prediction(learner$predict(task))
})

# test_that("memory limit works", {
#   skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
#   rush_plan(n_workers = 2)

#   task = tsk("spam")
#   learner = lrn("regr.auto",
#     max_memory = 50,
#     small_data_size = 100,
#     measure = msr("regr.rmse"),
#     terminator = trm("evals", n_evals = 20),
#     resampling = rsmp("holdout"),
#     lhs_size = 1
#   )

#   learner$train(task)
# })

test_that("small data set switch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("glmnet")
  flush_redis()

  task = tsk("california_housing")$filter(sample(1000))
  learner = lrn("regr.auto",
    learner_ids = "glmnet",
    small_data_size = 1000,
    small_data_resampling = rsmp("cv", folds = 2),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$model$instance$archive$benchmark_result$resamplings$resampling[[1]]$iters, 2)
})

test_that("large data set switch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  flush_redis()


  task = tsk("california_housing")$filter(sample(1000))
  learner = lrn("regr.auto",
    large_data_size = 100,
    large_data_nthread = 1,
    large_data_learner_ids = "ranger",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, "ranger")
})

test_that("max_cardinality works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("glmnet")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("california_housing")$filter(sample(1000))
  learner = lrn("regr.auto",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 2,
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
})

test_that("max_cardinality works for extra trees", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(c("glmnet", "ranger"))
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("california_housing")$filter(sample(1000))
  learner = lrn("regr.auto",
    learner_ids = c("glmnet", "extra_trees"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 3,
    extra_trees_max_cardinality = 2,
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
})

# test_that("logger callback works", {
#   skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
#   rush_plan(n_workers = 2)

#   task = tsk("california_housing")$filter(sample(1000))
#   learner = lrn("regr.auto",
#     small_data_size = 1,
#     resampling = rsmp("holdout"),
#     measure = msr("regr.rmse"),
#     terminator = trm("evals", n_evals = 10),
#     lhs_size = 1,
#     callbacks = clbk("mlr3tuning.async_save_logs")
#   )

#   expect_class(learner$train(task), "LearnerRegrAuto")
#   expect_list(learner$instance$archive$data$log)
#   expect_list(learner$instance$archive$data$log[[1]], len = 1)
# })

# test_that("integer columns work", {
#   library(mlr3oml)
#   rush_plan(n_workers = 2)


#   task = tsk("oml", data_id = 1464)
#   learner = lrn("regr.auto",
#     learner_ids = "catboost",
#     small_data_size = 100,
#     measure = msr("regr.rmse"),
#     terminator = trm("evals", n_evals = 20),
#     lhs_size = 1
#   )

#   expect_class(learner$train(task), "LearnerRegrAuto")
# })

# test_that("constant columns work", {
#   library(mlr3oml)
#   rush_plan(n_workers = 2, lgr_thresholds = c(mlr3 = "info"))


#   task = tsk("oml", data_id = 41143)
#   learner = lrn("regr.auto",
#     learner_ids = "catboost",
#     small_data_size = 100,
#     measure = msr("regr.rmse"),
#     terminator = trm("evals", n_evals = 20),
#     lhs_size = 1
#   )

#   expect_class(learner$train(task), "LearnerRegrAuto")
# })
