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

test_that("lhs design is generated with size smaller than the maximum number of levels", {
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost"))

  learner_ids =  c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost", "lightgbm")
  xdt = generate_lhs_design(1, "classif", learner_ids, tuning_space)
  expect_data_table(xdt, nrows = 21)
})

test_that("LearnerClassifAuto is initialized", {
  learner = lrn("classif.auto",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("glmnet works", {
  test_classif_learner("glmnet")
})

test_that("kknn works", {
  test_classif_learner ("kknn", n_evals = 10)
})

test_that("only lda fails", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

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
  test_classif_learner(c("lda", "glmnet"))
})

test_that("nnet works", {
  test_classif_learner("nnet")
})

test_that("ranger works", {
  test_classif_learner("ranger")
})

test_that("svm works", {
  test_classif_learner("svm")
})

test_that("xgboost works", {
  test_classif_learner("xgboost")
})

test_that("catboost works", {
  test_classif_learner("catboost")
})

test_that("only extra_trees fails", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  task = tsk("penguins")
  expect_error(lrn("classif.auto",
    learner_ids = "extra_trees",
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  ), "must be combined with other learners")
})

test_that("extra_trees and glmnet works", {
  test_classif_learner(c("extra_trees", "glmnet"))
})

test_that("lightgbm works", {
  test_classif_learner("lightgbm")
})

test_that("xgboost, catboost and lightgbm work", {
  test_classif_learner(c("xgboost", "catboost", "lightgbm"))
})

test_that("all learner work", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    small_data_size = 1,
    lhs_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 25),
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_class(learner$model$instance, "TuningInstanceAsyncSingleCrit")
  expect_prediction(learner$predict(task))
})

# test_that("memory limit works", {
#   skip_on_cran()
#   skip_if_not_installed("rush")
#   skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
#   flush_redis()

#   rush_plan(n_workers = 2)

#   task = tsk("spam")
#   learner = lrn("classif.auto",
#     max_memory = 50,
#     small_data_size = 100,
#     measure = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 20),
#     resampling = rsmp("holdout"),
#     lhs_size = 1,
#     encapsulate_learner = FALSE,
#     encapsulate_mbo = FALSE
#   )

#   learner$train(task)
# })

test_that("small data set switch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("glmnet")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "glmnet",
    small_data_size = 1000,
    small_data_resampling = rsmp("cv", folds = 2),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$model$instance$archive$benchmark_result$resamplings$resampling[[1]]$iters, 2)
})

test_that("large data set switch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    large_data_size = 100,
    large_data_nthread = 1,
    large_data_learner_ids = "ranger",
    small_data_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, "ranger")
})

test_that("max_cardinality works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("glmnet")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "glmnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 2,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("max_cardinality works for extra trees", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(c("glmnet", "ranger"))
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = c("glmnet", "extra_trees"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    max_cardinality = 3,
    extra_trees_max_cardinality = 2,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("resample works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("glmnet")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "glmnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  rr = resample(task, learner, rsmp("cv", folds = 2), store_models = TRUE)
  expect_resample_result(rr)
})

# test_that("logger callback works", {
#   skip_if_not_installed(c("glmnet", "kknn", "nnet", "ranger", "e1071", "xgboost", "catboost", "MASS", "lightgbm"))
#   rush_plan(n_workers = 2)

#   task = tsk("penguins")
#   learner = lrn("classif.auto",
#     small_data_size = 1,
#     resampling = rsmp("holdout"),
#     measure = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 10),
#     lhs_size = 1,
#     callbacks = clbk("mlr3tuning.async_save_logs")
#   )

#   expect_class(learner$train(task), "LearnerClassifAuto")
#   expect_list(learner$instance$archive$data$log)
#   expect_list(learner$instance$archive$data$log[[1]], len = 1)
# })

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
