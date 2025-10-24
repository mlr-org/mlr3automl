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
  xdt = map_dtr(autos, function(auto) auto$design_lhs(tsk("penguins"), 10L), .fill = TRUE)
  expect_data_table(xdt, nrows = length(autos) * 10 - 20 + 2)
  expect_set_equal(xdt$branch.selection, mlr_auto$keys())
})

test_that("random design is generated", {
  skip_if_not_installed(all_packages)

  autos = mlr_auto$mget(mlr_auto$keys())
  xdt = map_dtr(autos, function(auto) auto$design_random(tsk("penguins"), 10L), .fill = TRUE)
  expect_data_table(xdt, nrows = length(autos) * 10 - 20 + 2)
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
