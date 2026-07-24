test_that("LearnerClassifTabPFNIsolated works when mlr3torch is loaded", {
  skip_on_cran()
  skip_if_auto_not_installed("tabpfn")
  skip_if_not_installed("mlr3torch")

  # mlr3 loads mlr3torch in the encapsulation callr session when it is loaded in this process
  loadNamespace("mlr3torch")

  task = tsk("penguins")
  task$filter(c(1:10, 153:162, 277:286))
  auto = mlr_auto$get("tabpfn")
  graph = auto$graph(task, msr("classif.ce"), n_threads = 1L, timeout = 3600L, devices = "cpu")
  learner = as_learner(graph)
  learner$train(task)

  expect_data_table(learner$model$tabpfn$log, nrows = 0L)
})

test_that("LearnerClassifAutoTabPFN works", {
  task = tsk("penguins")
  task$filter(c(1:10, 153:162, 277:286))

  test_classif_learner("tabpfn", task = task)
})

test_that("LearnerClassifAuto works with tabpfn and mlr3torch", {
  skip_on_cran()
  skip_if_auto_not_installed(c("tabpfn", "mlp"))
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
    learner_ids = c("tabpfn", "mlp"),
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
