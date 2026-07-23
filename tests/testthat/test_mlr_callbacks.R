test_that("encapsulation_daemon callback is registered", {
  callback = clbk("mlr3automl.encapsulation_daemon")
  expect_class(callback, "CallbackAsyncTuning")
  expect_function(callback$on_worker_begin)
  expect_function(callback$on_optimizer_before_eval)
  expect_function(callback$on_worker_end)
})

test_that("encapsulation_daemon callback starts, maintains and stops the daemon", {
  skip_if_not_installed("mirai")
  compute = getOption("mlr3.mirai_encapsulation", "mlr3_encapsulation")
  on.exit(mirai::daemons(0L, .compute = compute), add = TRUE)

  wait_for_connections = function(n) {
    for (i in 1:100) {
      if (mirai::status(.compute = compute)$connections == n) break
      Sys.sleep(0.05)
    }
    mirai::status(.compute = compute)$connections
  }

  callback = clbk("mlr3automl.encapsulation_daemon", n_daemons = 1L)
  mirai::daemons(0L, .compute = compute)
  expect_equal(mirai::status(.compute = compute)$connections, 0L)

  # on_worker_begin starts one daemon
  callback$on_worker_begin(callback, NULL)
  expect_equal(wait_for_connections(1L), 1L)

  # before an evaluation the daemon is kept alive, not duplicated
  callback$on_optimizer_before_eval(callback, NULL)
  expect_equal(mirai::status(.compute = compute)$connections, 1L)

  # the daemon is restarted before an evaluation if it has died
  mirai::daemons(0L, .compute = compute)
  expect_equal(mirai::status(.compute = compute)$connections, 0L)
  callback$on_optimizer_before_eval(callback, NULL)
  expect_equal(wait_for_connections(1L), 1L)

  # on_worker_end stops the daemon
  callback$on_worker_end(callback, NULL)
  expect_equal(mirai::status(.compute = compute)$connections, 0L)
})
