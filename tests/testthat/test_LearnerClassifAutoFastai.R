test_that("LearnerClassifAutoFastai is initialized", {
  learner = lrn("classif.auto_fastai",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_learner(learner)
})

# test_that("LearnerClassifAutoFastai is trained", {
#   skip_on_cran()
#   skip_if_not_installed("fastai")
#   skip_if_not_installed("rush")
#   flush_redis()

#   Sys.setenv(RETICULATE_USE_MANAGED_VENV = "yes")

#   rush_plan(n_workers = 2, worker_type = "remote")
#   mirai::daemons(2)

#   task = tsk("penguins")
#   learner = lrn("classif.auto_fastai",
#     small_data_size = 1,
#     resampling = rsmp("cv", folds = 3),
#     measure = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 20),
#     lhs_size = 1,
#     encapsulate_learner = FALSE,
#     encapsulate_mbo = FALSE
#   )

#   expect_class(learner$train(task), "LearnerClassifAutoFastai")
#   expect_equal(learner$model$instance$result$branch.selection, "fastai")
# })

