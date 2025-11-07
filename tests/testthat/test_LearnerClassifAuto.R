test_that("LearnerClassifAuto is initialized", {
  learner = lrn("classif.auto",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_learner(learner, task = tsk("penguins"))
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

test_that("all learner on cpu work", {
  skip_on_cran()
  skip_if_not_installed(unlist(map(mlr_auto$mget(c("catboost", "glmnet", "kknn", "lightgbm", "ranger", "svm", "xgboost", "lda", "extra_trees")), "packages")))
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = c("catboost", "glmnet", "kknn", "lightgbm", "ranger", "svm", "xgboost", "lda", "extra_trees"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    initial_design_size = 30,
    initial_design_type = "sobol",
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, c("catboost", "glmnet", "kknn", "lightgbm", "ranger", "svm", "xgboost", "lda", "extra_trees"))
  expect_null(learner$model$graph_learner$param_set$values$xgboost.callbacks)
  expect_null(learner$model$graph_learner$param_set$values$lightgbm.callbacks)
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
    learner_ids = c("ranger", "xgboost", "catboost", "kknn"),
    memory_limit = 5,
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    resampling = rsmp("holdout"),
    initial_design_type = "random",
    initial_design_size = 2,
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
    initial_design_size = 2,
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

  options(bbotk.debug = TRUE)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = c("catboost", "glmnet", "kknn", "lightgbm", "ranger", "svm", "xgboost", "lda", "extra_trees"),
    initial_design_type = "sobol",
    large_data_size = 100,
    small_data_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    initial_design_size = 0,
    initial_design_default = TRUE,
    store_benchmark_result = TRUE,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, c("ranger", "xgboost", "catboost", "lightgbm", "extra_trees"))
  expect_equal(learner$model$graph_learner$param_set$values$ranger_subsample.frac, 1)
  expect_equal(learner$model$graph_learner$param_set$values$xgboost_subsample.frac, 1)
  expect_equal(learner$model$graph_learner$param_set$values$catboost_subsample.frac, 1)
  expect_equal(learner$model$graph_learner$param_set$values$lightgbm_subsample.frac, 1)
  expect_equal(learner$model$graph_learner$param_set$values$extra_trees_subsample.frac, 1)
  expect_true(learner$model$graph_learner$param_set$values$ranger_subsample.stratify)
  expect_true(learner$model$graph_learner$param_set$values$xgboost_subsample.stratify)
  expect_true(learner$model$graph_learner$param_set$values$catboost_subsample.stratify)
  expect_true(learner$model$graph_learner$param_set$values$lightgbm_subsample.stratify)
  expect_true(learner$model$graph_learner$param_set$values$extra_trees_subsample.stratify)
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
    initial_design_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  rr = resample(task, learner, rsmp("cv", folds = 2), store_models = TRUE)
  expect_resample_result(rr)
})

test_that("best initial design works with evals terminator", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = c("kknn", "glmnet"),
    initial_design_set = 1,
    small_data_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("initial design runtime limit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = "glmnet",
    small_data_size = 1,
    initial_design_type = "random",
    initial_design_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("run_time", secs = 40)
  )

   expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("devices works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    devices = "cpu",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    initial_design_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("devices works", {
  skip_if(TRUE)
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    devices = "cuda",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    initial_design_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("lightgbm time limit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("spam")

  learner = lrn("classif.auto",
    learner_ids = "lightgbm",
    learner_timeout = 1,
    measure = msr("classif.ce"),
    small_data_size = 1,
    terminator = trm("evals", n_evals = 10),
    initial_design_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

   learner$train(task)
   expect_true(all(learner$instance$archive$data[state == "finished"]$runtime_learners < 3))
})

test_that("xgboost time limit works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("spam")

  learner = lrn("classif.auto",
    learner_ids = "xgboost",
    learner_timeout = 1,
    measure = msr("classif.ce"),
    small_data_size = 1,
    terminator = trm("evals", n_evals = 10),
    initial_design_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)
  expect_true(all(learner$instance$archive$data[state == "finished"]$runtime_learners < 3))
})

test_that("adaptive design works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")

  learner = lrn("classif.auto",
    learner_ids = c("kknn", "ranger"),
    small_data_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("run_time", secs = 20),
    resampling = rsmp("holdout"),
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    initial_design_default = FALSE,
    initial_design_type = "sobol",
    initial_design_size = 256,
    initial_design_fraction = 0.25
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})

test_that("adaptive design works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  skip_if_not_installed(all_packages)
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")

  learner = lrn("classif.auto",
    learner_ids = c("lda", "ranger"),
    small_data_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("run_time", secs = 20),
    resampling = rsmp("holdout"),
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    initial_design_default = FALSE,
    initial_design_type = "sobol",
    initial_design_size = 256,
    initial_design_fraction = 0.25
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})




