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

test_that("effective_resources applies defaults, overrides, and the devices gate", {
  autos = list(
    debug_cpu = AutoDebug$new(id = "debug_cpu"),
    debug_gpu = AutoDebug$new(id = "debug_gpu", devices = c("cpu", "cuda"), n_gpu = 1L)
  )

  resources = effective_resources(autos, devices = c("cpu", "cuda"))
  expect_equal(resources$n_cpu, c(debug_cpu = 1L, debug_gpu = 1L))
  expect_equal(resources$n_gpu, c(debug_cpu = 0L, debug_gpu = 1L))

  resources = effective_resources(autos, n_gpu = c(debug_cpu = 1L), devices = c("cpu", "cuda"))
  expect_equal(resources$n_gpu, c(debug_cpu = 1L, debug_gpu = 1L))

  resources = effective_resources(autos, n_cpu = c(debug_gpu = 4L), devices = "cpu")
  expect_equal(resources$n_cpu, c(debug_cpu = 1L, debug_gpu = 4L))
  expect_equal(resources$n_gpu, c(debug_cpu = 0L, debug_gpu = 0L))

  expect_error(effective_resources(autos, n_gpu = c(nope = 1L)), "selected learner ids")
})

test_that("reduce_workers reduces a single group of workers", {
  reduced = reduce_workers(NULL, 8L)
  expect_null(reduced$profiles)
  expect_equal(reduced$n_workers, 2L)
  expect_equal(reduced$scale, 4)

  # rounded up so that the workers are not reduced by more than a factor of 4
  reduced = reduce_workers(NULL, 7L)
  expect_equal(reduced$n_workers, 2L)
  expect_equal(reduced$scale, 3.5)

  # at least one worker is kept
  reduced = reduce_workers(NULL, 1L)
  expect_equal(reduced$n_workers, 1L)
  expect_equal(reduced$scale, 1)
})

test_that("reduce_workers reduces the cpu profile but not the gpu profile", {
  profiles = c(mlr3automl_cpu = 7L, mlr3automl_gpu = 1L)

  reduced = reduce_workers(profiles, sum(profiles))
  expect_equal(reduced$profiles, c(mlr3automl_cpu = 2L, mlr3automl_gpu = 1L))
  expect_equal(reduced$n_workers, 3L)
  # the scale only compensates the reduced cpu profile
  expect_equal(reduced$scale, 3.5)

  reduced = reduce_workers(profiles["mlr3automl_cpu"], 7L)
  expect_equal(reduced$profiles, c(mlr3automl_cpu = 2L))
  expect_equal(reduced$scale, 3.5)
})

test_that("reduce_workers keeps the workers when only the gpu profile is set up", {
  profiles = c(mlr3automl_gpu = 2L)

  reduced = reduce_workers(profiles, sum(profiles))
  expect_equal(reduced$profiles, profiles)
  expect_equal(reduced$n_workers, 2L)
  expect_equal(reduced$scale, 1)
})

test_that("assign_learner_profiles runs all learners on the default profile without compute profiles", {
  assignment = assign_learner_profiles(NULL, n_workers = 4L, cpu_ids = c("a", "b"), gpu_ids = "c")
  expect_equal(assignment$subspace_profiles, c(a = "default", b = "default", c = "default"))
  expect_equal(assignment$profiles, c(default = 4L))
})

test_that("assign_learner_profiles runs all learners on a single compute profile", {
  assignment = assign_learner_profiles(c(mlr3automl_cpu = 2L), n_workers = 2L, cpu_ids = "a", gpu_ids = "c")
  expect_equal(assignment$subspace_profiles, c(a = "mlr3automl_cpu", c = "mlr3automl_cpu"))
  expect_equal(assignment$profiles, c(mlr3automl_cpu = 2L))
})

test_that("assign_learner_profiles divides the learners among the cpu and the gpu profile", {
  profiles = c(mlr3automl_cpu = 7L, mlr3automl_gpu = 1L)

  assignment = assign_learner_profiles(profiles, n_workers = 8L, cpu_ids = c("a", "b"), gpu_ids = "c")
  expect_equal(assignment$subspace_profiles, c(a = "mlr3automl_cpu", b = "mlr3automl_cpu", c = "mlr3automl_gpu"))
  expect_equal(assignment$profiles, profiles)

  # a profile without learners is dropped
  assignment = assign_learner_profiles(profiles, n_workers = 8L, cpu_ids = c("a", "b"), gpu_ids = character())
  expect_equal(assignment$subspace_profiles, c(a = "mlr3automl_cpu", b = "mlr3automl_cpu"))
  expect_equal(assignment$profiles, profiles["mlr3automl_cpu"])
})

test_that("assign_learner_profiles rejects other combinations of compute profiles", {
  expect_error(
    assign_learner_profiles(c(cores = 1L, cuda = 1L), n_workers = 2L, cpu_ids = "a", gpu_ids = "c"),
    "not supported",
    class = "Mlr3ErrorConfig"
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
