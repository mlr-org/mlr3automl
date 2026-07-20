test_that("cb_timeout_xgboost resets the clock on each training", {
  skip_if_not_installed("xgboost")

  callback = cb_timeout_xgboost(timeout = 100)

  # simulate a previous training that exhausted the timeout
  callback$env$start_time = Sys.time() - 1000

  callback$f_before_training(callback$env, NULL, NULL, NULL, 1L, 10L)
  expect_false(callback$f_after_iter(callback$env, NULL, NULL, NULL, 1L, NULL))

  callback$env$start_time = Sys.time() - 1000
  expect_message(
    expect_true(callback$f_after_iter(callback$env, NULL, NULL, NULL, 2L, NULL)),
    "Timeout reached"
  )
})

test_that("cb_timeout_lightgbm resets the clock on each training", {
  callback = cb_timeout_lightgbm(timeout = 100)
  state = environment(callback)$state

  env = new.env()
  env$begin_iteration = 1L
  env$iteration = 1L

  # simulate a previous training that exhausted the timeout
  state$start_time = Sys.time() - 1000

  callback(env)
  expect_false(env$met_early_stop)

  env$iteration = 2L
  state$start_time = Sys.time() - 1000
  expect_message(callback(env), "Timeout reached")
  expect_true(env$met_early_stop)
})
