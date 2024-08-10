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
  expected_path = file.path(dir, "test-1-run_1")
  if (file.exists(expected_path)) {
    lapply(list.files(expected_path, full.names = TRUE), file.remove)
  }
  file.remove(expected_path)

  save_deepcave_run(learner$instance, path = dir, prefix = "test-1-run", overwrite = FALSE)

  expect_file_exists(file.path(expected_path, "configspace.json"))
  expect_file_exists(file.path(expected_path, "configs.json"))
  # expect_file_exists(file.path(expected_path, "history.jsonl"))
  expect_file_exists(file.path(expected_path, "meta.json"))
  expect_file_exists(file.path(expected_path, "origins.json"))
})

test_that("overwriting works", {
  dir = tempdir()
  expected_path = file.path(dir, "test-run-overwrite")
  file.create(file.path(expected_path, "configs.json"), showWarnings = FALSE)
  file.create(file.path(expected_path, "configspace.json"), showWarnings = FALSE)
  file.create(file.path(expected_path, "history.jsonl"), showWarnings = FALSE)
  file.create(file.path(expected_path, "meta.json"), showWarnings = FALSE)
  file.create(file.path(expected_path, "origins.json"), showWarnings = FALSE)

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

  save_deepcave_run(learner$instance, path = dir, prefix = "test-run-overwrite", overwrite = TRUE)

  expect_file_exists(file.path(expected_path, "configspace.json"))
  expect_file_exists(file.path(expected_path, "configs.json"))
  # expect_file_exists(file.path(expected_path, "history.jsonl"))
  expect_file_exists(file.path(expected_path, "meta.json"))
  expect_file_exists(file.path(expected_path, "origins.json"))
})
