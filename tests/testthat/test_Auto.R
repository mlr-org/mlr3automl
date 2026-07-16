test_that("default design is generated", {
  skip_if_not_all_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  task = tsk("penguins")
  xdt = map_dtr(autos, function(auto) auto$design_default(task), .fill = TRUE)
  expect_data_table(xdt, nrows = length(autos))
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())

  iwalk(autos, function(auto, id) {
    row = xdt[branch.selection == id]
    search_space = auto$search_space(task)
    param_ids = setdiff(search_space$ids(), search_space$ids(any_tags = "internal_tuning"))
    for (param_id in param_ids) {
      expect_false(is.na(row[[param_id]]), info = sprintf("NA default for %s in %s", param_id, id))
    }
  })
})

test_that("lhs design is generated", {
  skip_if_not_all_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = generate_initial_design("lhs", combine_search_spaces(autos, tsk("penguins")), 256L)
  expect_data_table(xdt, nrows = 256L)
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("sobol design is generated", {
  skip_if_not_all_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = generate_initial_design("sobol", combine_search_spaces(autos, tsk("penguins")), 256L)
  expect_data_table(xdt, nrows = 256L)
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("random design is generated", {
  skip_if_not_all_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = generate_initial_design("random", combine_search_spaces(autos, tsk("penguins")), 256L)
  expect_data_table(xdt, nrows = 256L)
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("set design is generated", {
  skip_if_not_all_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = map_dtr(autos, function(auto) auto$design_set(tsk("penguins"), msr("classif.ce"), 10L), .fill = TRUE)
  expect_data_table(xdt, nrows = 70L)
  expect_set_equal(xdt$branch.selection, c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost", "lightgbm"))
})

test_that("set design drops points outside task-dependent bounds", {
  skip_if_not_all_installed(all_packages)

  auto = mlr_auto$get("kknn")

  # small task shrinks the kknn.k upper bound to log(nrow - 1)
  small_task = tsk("penguins")$filter(1:30)
  upper = auto$search_space(small_task)$upper[["kknn.k"]]
  lower = auto$search_space(small_task)$lower[["kknn.k"]]

  # the warm-start data stores kknn.k up to log(100), above the shrunk bound
  raw = fread(system.file("ex_data", "best_kknn.csv", package = "mlr3automl"))
  expect_true(any(raw$kknn.k > upper))

  xdt = auto$design_set(small_task, msr("classif.ce"), 20L)
  expect_data_table(xdt, min.rows = 1L)
  expect_true(all(xdt$kknn.k >= lower & xdt$kknn.k <= upper))

  # full task keeps all points within bounds and does not drop anything
  full_task = tsk("penguins")
  full_upper = auto$search_space(full_task)$upper[["kknn.k"]]
  xdt_full = auto$design_set(full_task, msr("classif.ce"), 10L)
  expect_data_table(xdt_full, nrows = 10L)
  expect_true(all(xdt_full$kknn.k <= full_upper))
})

test_that("estimate memory works", {
  skip_if_not_all_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  memory = map_dbl(autos, function(auto) auto$estimate_memory(tsk("penguins")))
  expect_numeric(memory)
})

test_that("internal measure falls back to a valid metric", {
  catboost = mlr_auto$get("catboost")
  expect_equal(catboost$internal_measure(msr("regr.mse"), tsk("mtcars")), "RMSE")
  expect_equal(catboost$internal_measure(msr("regr.medse"), tsk("mtcars")), "RMSE")
  expect_equal(catboost$internal_measure(msr("classif.fbeta"), tsk("sonar")), "Accuracy")
  expect_equal(catboost$internal_measure(msr("classif.bacc"), tsk("iris")), "Accuracy")

  xgboost = mlr_auto$get("xgboost")
  expect_equal(xgboost$internal_measure(msr("regr.mse"), tsk("mtcars")), "rmse")
  expect_equal(xgboost$internal_measure(msr("regr.medse"), tsk("mtcars")), "rmse")
  expect_equal(xgboost$internal_measure(msr("classif.fbeta"), tsk("sonar")), "error")
  expect_equal(xgboost$internal_measure(msr("classif.bacc"), tsk("iris")), "merror")

  lightgbm = mlr_auto$get("lightgbm")
  expect_equal(lightgbm$internal_measure(msr("regr.medse"), tsk("mtcars")), "rmse")
  expect_equal(lightgbm$internal_measure(msr("classif.fbeta"), tsk("sonar")), "binary_error")
  expect_equal(lightgbm$internal_measure(msr("classif.bacc"), tsk("iris")), "multi_error")
})

test_that("catboost accepts the fallback eval metrics", {
  skip_on_cran()
  skip_if_not_installed("catboost")
  skip_if_not_installed("mlr3extralearners")
  require_namespaces("mlr3extralearners")

  auto = mlr_auto$get("catboost")
  tasks = list(tsk("mtcars"), tsk("sonar"), tsk("iris"))
  measures = list(msr("regr.mse"), msr("classif.fbeta"), msr("classif.bacc"))

  for (i in seq_along(tasks)) {
    learner = lrn(
      sprintf("%s.catboost", tasks[[i]]$task_type),
      iterations = 3,
      eval_metric = auto$internal_measure(measures[[i]], tasks[[i]]),
      logging_level = "Silent"
    )
    expect_class(learner$train(tasks[[i]]), "Learner")
  }
})
