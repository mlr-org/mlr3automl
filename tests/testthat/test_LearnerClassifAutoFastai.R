test_that("LearnerClassifAutoFastai works", {
  skip_on_cran()
  skip_if_auto_not_installed("fastai")
  skip_if_not_installed("rush")
  skip_if_no_redis()

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  task = tsk("penguins")

  learner = lrn(
    "classif.auto_fastai",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    initial_design_type = "lhs",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    check_learners = TRUE,
    rush = rush
  )

  expect_class(learner$train(task), "LearnerClassifAutoFastai")
})

test_that("LearnerClassifAuto works with fastai and mlr3torch", {
  skip_on_cran()
  skip_if_auto_not_installed(c("fastai", "mlp"))
  skip_if_not_installed("rush")
  skip_if_not_installed("mlr3torch")
  skip_if_no_redis()

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  task = tsk("penguins")

  learner = lrn(
    "classif.auto",
    learner_ids = c("fastai", "mlp"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    initial_design_type = "lhs",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    check_learners = TRUE,
    rush = rush
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
})
