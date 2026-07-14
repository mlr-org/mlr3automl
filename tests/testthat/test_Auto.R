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
