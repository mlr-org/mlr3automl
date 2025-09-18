all_packages = c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost","lightgbm", "fastai", "mlr3torch")
all_learners = c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost","lightgbm", "lda", "extra_trees")

test_that("initial design is generated", {
  skip_if_not_installed(all_packages)

  learner_ids = c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost","lightgbm", "fastai", "mlp", "extra_trees", "lda")
  autos = mlr_auto$mget(learner_ids)
  xdt = map_dtr(autos, function(auto) auto$design_default(tsk("penguins")), .fill = TRUE)
  expect_data_table(xdt, nrows = length(learner_ids))
  expect_set_equal(xdt$branch.selection, learner_ids)
})

test_that("lhs design is generated", {
  skip_if_not_installed(all_packages)

  learner_ids = c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost","lightgbm", "fastai", "mlp", "extra_trees", "lda")

  autos = mlr_auto$mget(learner_ids)
  xdt = map_dtr(autos, function(auto) auto$design_lhs(tsk("penguins"), 10L), .fill = TRUE)
  expect_data_table(xdt, nrows = 90)
  expect_set_equal(xdt$branch.selection, c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost","lightgbm", "fastai", "mlp"))
})

test_that("lhs design is generated with size smaller than the maximum number of levels", {
  skip_if_not_installed(all_packages)

  learner_ids = c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost","lightgbm", "fastai", "mlp", "extra_trees", "lda")

  autos = mlr_auto$mget(learner_ids)
  xdt = map_dtr(autos, function(auto) auto$design_lhs(tsk("penguins"), 1L), .fill = TRUE)
  expect_data_table(xdt, nrows = 26)
  expect_set_equal(xdt$branch.selection, c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost","lightgbm", "fastai", "mlp"))
})

test_that("LearnerClassifAuto is initialized", {
  learner = lrn("classif.auto",
    learner_ids = all_learners,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_learner(learner)
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
    terminator = trm("evals", n_evals = 6))$train(task),
    "All learners have no hyperparameters to tune")
})

test_that("lda and glmnet works", {
  test_classif_learner(c("lda", "glmnet"))
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
    terminator = trm("evals", n_evals = 6))$train(task),
    "All learners have no hyperparameters to tune")
})

test_that("extra_trees and glmnet works", {
  test_classif_learner(c("extra_trees", "glmnet"))
})

test_that("lightgbm works", {
  test_classif_learner("lightgbm")
})

test_that("mlp works", {
  test_classif_learner("mlp")
})

# test_that("fastai works", {
#   test_classif_learner("fastai")
# })

test_that("xgboost, catboost and lightgbm work", {
  test_classif_learner(c("xgboost", "catboost", "lightgbm"))
})

test_that("all learner work", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = all_learners,
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

test_that("memory limit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("spam")
  learner = lrn("classif.auto",
    learner_ids = all_learners,
    memory_limit = 50,
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    resampling = rsmp("holdout"),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_true(all(c("ranger", "xgboost", "catboost") %nin% learner$model$instance$archive$data$branch.selection))
})

test_that("small data set switch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("glmnet")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

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
  skip_if_not_installed(all_packages)
  flush_redis()

    rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = all_learners,
    large_data_size = 100,
    small_data_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    lhs_size = 1,
    store_benchmark_result = TRUE,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, c("ranger", "xgboost", "catboost", "lightgbm", "extra_trees"))
})

test_that("resample works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed("glmnet")
  flush_redis()

    rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

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

