test_that("default design is generated", {
  skip_if_not_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = map_dtr(autos, function(auto) auto$design_default(tsk("penguins")), .fill = TRUE)
  expect_data_table(xdt, nrows = length(autos))
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("lhs design is generated", {
  skip_if_not_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = generate_initial_design("lhs", combine_search_spaces(autos, tsk("penguins")), 256L)
  expect_data_table(xdt, nrows = 256L)
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("sobol design is generated", {
  skip_if_not_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = generate_initial_design("sobol", combine_search_spaces(autos, tsk("penguins")), 256L)
  expect_data_table(xdt, nrows = 256L)
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("random design is generated", {
  skip_if_not_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = generate_initial_design("random", combine_search_spaces(autos, tsk("penguins")), 256L)
  expect_data_table(xdt, nrows = 256L)
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("set design is generated", {
  skip_if_not_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = map_dtr(autos, function(auto) auto$design_set(tsk("penguins"), msr("classif.ce"), 10L), .fill = TRUE)
  expect_data_table(xdt, nrows = 70L)
  expect_set_equal(xdt$branch.selection, c("glmnet", "kknn", "ranger", "svm", "xgboost", "catboost","lightgbm"))
})

test_that("estimate memory works", {
  skip_if_not_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  memory = map_dbl(autos, function(auto) auto$estimate_memory(tsk("penguins")))
  expect_numeric(memory)
})
