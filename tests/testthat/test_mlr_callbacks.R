test_that("encapsulation_daemon callback is registered", {
  callback = clbk("mlr3automl.encapsulation_daemon")
  expect_class(callback, "CallbackAsyncTuning")
  expect_function(callback$on_worker_begin)
  expect_function(callback$on_optimizer_before_eval)
  expect_function(callback$on_worker_end)
})

test_that("ensure_encapsulation_daemon starts and restarts the daemon", {
  skip_if_not_installed("mirai")
  compute = mirai_encapsulation_compute()
  on.exit(mirai::daemons(0L, .compute = compute), add = TRUE)

  wait_for_connections = function(n) {
    for (i in 1:100) {
      if (mirai::status(.compute = compute)$connections == n) break
      Sys.sleep(0.05)
    }
    mirai::status(.compute = compute)$connections
  }

  # no daemon initially
  mirai::daemons(0L, .compute = compute)
  expect_equal(mirai::status(.compute = compute)$connections, 0L)

  # starts one daemon
  ensure_encapsulation_daemon(1L)
  expect_equal(wait_for_connections(1L), 1L)

  # idempotent when the daemon is still alive
  ensure_encapsulation_daemon(1L)
  expect_equal(mirai::status(.compute = compute)$connections, 1L)

  # restarts the daemon after it has died
  mirai::daemons(0L, .compute = compute)
  expect_equal(mirai::status(.compute = compute)$connections, 0L)
  ensure_encapsulation_daemon(1L)
  expect_equal(wait_for_connections(1L), 1L)
})
