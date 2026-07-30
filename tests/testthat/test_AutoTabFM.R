test_that("AutoTabFM is registered for cuda only", {
  skip_if_not_installed("mlr3extralearners")

  auto = mlr_auto$get("tabfm")
  expect_equal(auto$devices, "cuda")
  expect_false(auto$check(tsk("penguins"), devices = "cpu"))
})

test_that("AutoTabFM can be constructed for the cpu", {
  skip_if_not_installed("mlr3extralearners")

  auto = AutoTabFM$new(devices = c("cpu", "cuda"))
  expect_set_equal(auto$devices, c("cpu", "cuda"))
  expect_error(AutoTabFM$new(devices = "tpu"), "devices")
})

test_that("AutoTabFM search space depends on the task type", {
  skip_if_not_installed("mlr3extralearners")

  auto = mlr_auto$get("tabfm")
  classif_ids = auto$search_space(tsk("penguins"))$ids()
  regr_ids = auto$search_space(tsk("mtcars"))$ids()

  expect_subset(c("tabfm.softmax_temperature", "tabfm.average_logits", "tabfm.class_shift"), classif_ids)
  expect_disjunct(c("tabfm.softmax_temperature", "tabfm.average_logits", "tabfm.class_shift"), regr_ids)
  expect_subset(regr_ids, classif_ids)
})

test_that("AutoTabFM default design is within the search space", {
  skip_if_not_installed("mlr3extralearners")

  auto = mlr_auto$get("tabfm")
  walk(list(tsk("penguins"), tsk("mtcars")), function(task) {
    design = auto$design_default(task)
    search_space = auto$search_space(task)
    expect_set_equal(setdiff(names(design), "branch.selection"), search_space$ids())
    expect_true(search_space$check_dt(design[, search_space$ids(), with = FALSE]))
  })
})
