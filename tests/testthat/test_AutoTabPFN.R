test_that("AutoTabPFN check rejects tasks with more than 2,000 features", {
  skip_if_not_installed("mlr3extralearners")

  data = as.data.table(matrix(rnorm(10 * 2001), nrow = 10))
  data[, target := factor(rep(c("a", "b"), 5))]
  task = as_task_classif(data, target = "target")

  expect_false(mlr_auto$get("tabpfn")$check(task, devices = "cuda"))
})

test_that("AutoTabPFN check counts the features after impact encoding", {
  skip_if_not_installed("mlr3extralearners")

  # 700 factor features are below the limit but expand to 2,100 columns for 3 classes
  data = as.data.table(replicate(700, factor(sample(c("a", "b"), 30, replace = TRUE)), simplify = FALSE))
  data[, target := factor(rep(c("a", "b", "c"), 10))]
  task = as_task_classif(data, target = "target")

  expect_false(mlr_auto$get("tabpfn")$check(task, devices = "cuda"))
})

test_that("AutoTabPFN check counts categorical features once for regression", {
  skip_if_not_installed("mlr3extralearners")

  data = as.data.table(replicate(2001, factor(sample(c("a", "b"), 10, replace = TRUE)), simplify = FALSE))
  data[, target := rnorm(10)]
  task = as_task_regr(data, target = "target")

  expect_false(mlr_auto$get("tabpfn")$check(task, devices = "cuda"))
})

test_that("AutoTabPFN check rejects tasks with more than 1,000 rows on cpu", {
  skip_if_not_installed("mlr3extralearners")

  task = tgen("2dnormals")$generate(1001)

  expect_false(mlr_auto$get("tabpfn")$check(task, devices = "cpu"))
})
