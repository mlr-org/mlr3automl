test_that("new distances calculation works", {
  configs = rbindlist(list(
    list(a = 1, b = 1),
    list(a = 2, b = 1)
  ))
  new_config = c(3, 1)
  metric = function(x, y) sum(abs(x - y))

  new_distances = get_new_distances(configs, new_config, metric, rejection_threshold = 0)
  expect_identical(new_distances, c(2, 1))
})

test_that("new distances rejection mechanism works", {
  configs = rbindlist(list(
    list(a = 1, b = 1),
    list(a = 2, b = 1)
  ))
  new_config = c(2, 1)
  metric = function(x, y) sum(abs(x - y))

  new_distances = get_new_distances(configs, new_config, metric, rejection_threshold = 0.1)
  expect_null(new_distances)
})
