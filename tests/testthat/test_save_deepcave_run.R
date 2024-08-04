test_that("run is saved", {
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
  learner$train(task)

  dir = tempdir()

  save_deepcave_run(learner, path = paste0(dir))

  expect_file_exists(paste0(dir, "/configspace.json"))
  expect_file_exists(paste0(dir, "/configs.json"))
  expect_file_exists(paste0(dir, "/history.json"))
  expect_file_exists(paste0(dir, "/meta.json"))
  expect_file_exists(paste0(dir, "/origins.json"))
})
