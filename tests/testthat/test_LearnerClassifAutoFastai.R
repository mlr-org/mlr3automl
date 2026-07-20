test_that("LearnerClassifAutoFastai works", {
  result = test_classif_learner("fastai")

  archive = as.data.table(result$learner$instance$archive, unnest = "internal_tuned_values")
  archive_finished = archive[state == "finished"]
  expect_integer(archive_finished$internal_tuned_values_fastai.n_epoch, lower = 1L)
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
