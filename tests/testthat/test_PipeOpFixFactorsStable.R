test_that("PipeOpFixFactorsStable fixes levels and keeps the feature order", {
  data = data.table(
    a = factor(c("x", "y", "x", "z")),
    b = c(1, 2, 3, 4),
    y = c(1, 2, 3, 4)
  )
  task = as_task_regr(data, target = "y")
  pop = PipeOpFixFactorsStable$new()

  pop$train(list(task$clone()$filter(1:2)))
  expect_equal(pop$state$levels, list(a = c("x", "y")))

  out = pop$predict(list(task$clone()$filter(3:4)))[[1L]]
  expect_equal(out$feature_names, c("a", "b"))
  expect_equal(levels(out$data()$a), c("x", "y"))
})
