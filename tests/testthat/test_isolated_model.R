# minimal R6 object with private methods to drive through isolated_session();
# isolated_session() only needs get_private(learner)[[method]](task) to work, so a real
# mlr3 Learner is not required here.
IsolatedTestLearner = R6::R6Class(
  "IsolatedTestLearner",
  public = list(x = 1),
  private = list(
    .double = function(task) task * 2,
    .pid = function(task) Sys.getpid(),
    .fail = function(task) {
      cat("out from child\n")
      message("err from child")
      stop("boom")
    },
    .python_fail = function(task) {
      reticulate::py_run_string("raise ValueError('boom from python')")
    }
  )
)

test_that("isolated_session runs a private method in a separate process", {
  skip_if_not_installed("callr")

  learner = IsolatedTestLearner$new()
  expect_identical(isolated_session(learner, 21, ".double"), 42)

  child_pid = isolated_session(learner, NULL, ".pid")
  expect_false(identical(child_pid, Sys.getpid()))
})

test_that("isolated_session(isolate = FALSE) runs the private method in the calling process", {
  learner = IsolatedTestLearner$new()
  expect_identical(isolated_session(learner, 21, ".double", isolate = FALSE), 42)

  pid = isolated_session(learner, NULL, ".pid", isolate = FALSE)
  expect_identical(pid, Sys.getpid())
})

test_that("isolated_session reports child stdout/stderr when the private method errors", {
  skip_if_not_installed("callr")

  learner = IsolatedTestLearner$new()
  err = tryCatch(isolated_session(learner, 1, ".fail"), error = function(e) conditionMessage(e))

  expect_match(err, "isolated_session\\(\\.fail\\) failed")
  expect_match(err, "out from child", fixed = TRUE)
  expect_match(err, "err from child", fixed = TRUE)
  expect_match(err, "boom", fixed = TRUE)
})

test_that("isolated_session surfaces reticulate::py_last_error() output for python errors", {
  skip_if_not_installed("callr")
  skip_if_not_installed("reticulate")

  learner = IsolatedTestLearner$new()
  err = tryCatch(isolated_session(learner, 1, ".python_fail"), error = function(e) conditionMessage(e))

  expect_match(err, "isolated_session\\(\\.python_fail\\) failed")
  expect_match(err, "reticulate::py_last_error", fixed = TRUE)
  expect_match(err, "boom from python", fixed = TRUE)
})

test_that("isolated_model_pickled objects round-trip through marshal_model/unmarshal_model", {
  model = structure(list(pickled = as.raw(c(1, 2, 3))), class = "isolated_model_pickled")

  marshaled = marshal_model(model, inplace = TRUE)
  expect_class(marshaled, "isolated_model_marshaled")
  expect_identical(marshaled$marshaled, model$pickled)
  expect_identical(marshaled$packages, "mlr3automl")

  unmarshaled = unmarshal_model(marshaled, inplace = TRUE)
  expect_class(unmarshaled, "isolated_model_pickled")
  expect_identical(unmarshaled$pickled, model$pickled)
})
