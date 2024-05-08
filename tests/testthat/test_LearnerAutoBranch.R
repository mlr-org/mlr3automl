test_that("initial design is generated", {
  learner_ids = c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees")
  xdt = generate_default_design(
    task_type = "classif",
    learner_ids,
    task = tsk("sonar"),
    tuning_space)
  n_hp = sum(map_dbl(tuning_space, length))
  expect_data_table(xdt, nrows = length(learner_ids), ncols = n_hp + 1)
})

test_that("lhs design is generated", {
  learner_ids =  c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees")
  xdt = generate_lhs_design(10, "classif", learner_ids, tuning_space)
  n_hp = sum(map_dbl(tuning_space, length))
  expect_data_table(xdt, nrows = 80, ncols = n_hp + 1)
})

test_that("glmnet works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "glmnet",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "glmnet")
})

test_that("kknn works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "kknn",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "kknn")
})

test_that("lda works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "lda",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "lda")
})

test_that("nnet works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "nnet",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "nnet")
})

test_that("ranger works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "ranger",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "ranger")
})

test_that("svm works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "svm",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "svm")
})

test_that("xgboost works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "xgboost",
    xgboost_eval_metric = "mlogloss",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "xgboost")
  expect_numeric(learner$model$instance$archive$data$max_nrounds, lower = 1)
})

test_that("catboost works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "catboost",
    catboost_eval_metric = "MultiClass",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "catboost")
  expect_numeric(learner$model$instance$archive$data$max_nrounds, lower = 1)
})

test_that("extra_trees works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "extra_trees",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "extra_trees")
})

test_that("lightgbm works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = "lightgbm",
    lightgbm_eval_metric = "multi_logloss",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$result$branch.selection, "lightgbm")
})

test_that("xgboost, catboost and lightgbm work", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    learner_ids = c("xgboost", "catboost", "lightgbm"),
    catboost_eval_metric = "MultiClass",
    lightgbm_eval_metric = "multi_logloss",
    xgboost_eval_metric = "mlogloss",
    small_data_size = 100,
    lhs_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    callbacks = clbk("mlr3tuning.async_save_logs")
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
})

test_that("all learner work", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_class(learner$model$instance, "TuningInstanceAsyncSingleCrit")
  expect_prediction(learner$predict(task))
})

test_that("memory limit works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("spam")
  learner = lrn("classif.automl_branch",
    max_memory = 50,
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    resampling = rsmp("holdout"),
    lhs_size = 1
  )

  learner$train(task)
})

test_that("small data set switch works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    small_data_size = 1000,
    small_data_resampling = rsmp("cv", folds = 2),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_equal(learner$model$instance$archive$benchmark_result$resamplings$resampling[[1]]$iters, 2)
})

test_that("large data set switch works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    large_data_size = 100,
    large_data_nthread = 4,
    large_data_learner_ids = "ranger",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1,
    store_benchmark_result = TRUE
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, "ranger")
})

test_that("max_cardinality works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    small_data_size = 100,
    max_cardinality = 2,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
})

test_that("max_cardinality works for extra trees", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    small_data_size = 100,
    max_cardinality = 3,
    extra_trees_max_cardinality = 2,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 1),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
})

test_that("logger callback works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_branch",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    lhs_size = 1,
    callbacks = clbk("mlr3tuning.async_save_logs")
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")

  expect_list(learner$instance$archive$data$log)
  expect_list(learner$instance$archive$data$log[[1]], len = 3)
})

test_that("integer columns work", {
  library(mlr3oml)
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("oml", data_id = 1464)
  learner = lrn("classif.automl_branch",
    learner_ids = "catboost",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
})

test_that("constant columns work", {
  library(mlr3oml)
  rush_plan(n_workers = 2, lgr_thresholds = c(mlr3 = "info"))
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("oml", data_id = 41143)
  learner = lrn("classif.automl_branch",
    learner_ids = "catboost",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    lhs_size = 1
  )

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
})
