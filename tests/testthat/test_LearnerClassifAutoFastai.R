test_that("LearnerClassifAutoFastai is initialized", {
  learner = lrn("classif.auto_fastai",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoFastai is trained", {
  skip_on_cran()
  skip_if_not_installed("fastai")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")

  mirai::daemons(2)

  options(bbotk.debug = TRUE)

  task = tsk("penguins")
  learner = lrn("classif.auto_fastai",
    fastai_eval_metric = fastai::error_rate(),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAutoFastai")
  expect_equal(learner$graph$param_set$values$branch.selection, "fastai")
  expect_equal(learner$model$instance$result$branch.selection, "fastai")
})


# test_that("LearnerClassifAutoFastai is trained", {
#   skip_on_cran()
#   skip_if_not_installed("rush")
#   flush_redis()

#   # lgr::get_logger("mlr3")$set_threshold("debug")
#   # lgr::get_logger("rush")$set_threshold("debug")

#   #options(bbotk_local = TRUE)

#   Sys.setenv(RETICULATE_PYTHON = "managed")

#   rush_plan(n_workers = 2, lgr_thresholds = c(mlr3 = "debug", rush = "debug"))

#   task = tsk("penguins")
#   learner = lrn("classif.auto_fastai",
#     small_data_size = 1,
#     resampling = rsmp("holdout"),
#     measure = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 6),
#     callbacks = clbk("mlr3tuning.async_save_logs")
#   )

#   expect_class(learner$train(task), "LearnerClassifAutoFastai")
#   expect_equal(learner$graph$param_set$values$branch.selection, "fastai")
#   expect_equal(learner$model$instance$result$branch.selection, "fastai")
# })
