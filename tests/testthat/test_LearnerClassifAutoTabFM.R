test_that("LearnerClassifTabFMIsolated works when mlr3torch is loaded", {
  skip_if(TRUE)
  skip_on_cran()
  skip_if_auto_not_installed("tabfm")
  skip_if_not_installed("mlr3torch")

  # mlr3 loads mlr3torch in the encapsulation callr session when it is loaded in this process
  loadNamespace("mlr3torch")

  task = tsk("penguins")
  task$filter(c(1:10, 153:162, 277:286))
  # $graph() takes the devices as an argument and does not consult the cuda-only registration,
  # so the cpu graph is available without re-registering the auto
  auto = mlr_auto$get("tabfm")
  graph = auto$graph(task, msr("classif.ce"), n_threads = 1L, timeout = 3600L, devices = "cpu")
  learner = as_learner(graph)
  # the tuner always sets the number of estimators, so the graph keeps the default of 32,
  # which runs the backbone 32 times per fit
  learner$param_set$set_values(tabfm.n_estimators = 1L)
  learner$train(task)

  expect_data_table(learner$model$tabfm$log, nrows = 0L)
})

test_that("LearnerClassifAutoTabFM works", {
  skip_if(TRUE)
  # tabfm is registered for cuda only, so the end-to-end test runs a variant that also allows the cpu
  mlr_auto$add("tabfm", function() AutoTabFM$new(devices = c("cpu", "cuda")))
  on.exit(mlr_auto$add("tabfm", function() AutoTabFM$new()), add = TRUE)

  task = tsk("penguins")
  task$filter(c(1:10, 153:162, 277:286))

  # every evaluation runs the backbone once per estimator in its own callr session,
  # so the budget is kept at the minimum that still exercises the initial design and one mbo proposal
  test_classif_learner("tabfm", task = task, initial_design_size = 1, n_evals = 2)
})
