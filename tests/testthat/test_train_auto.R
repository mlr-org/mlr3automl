test_that("training errors when all evaluations fail", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug", function() AutoDebug$new(error_train = 1))
  on.exit(mlr_auto$add("debug", function() AutoDebug$new()), add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = "debug",
    rush = rush,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  # bbotk errors with a plain `Mlr3Error` because no evaluation finished
  expect_error(learner$train(task), class = "Mlr3Error")
})

test_that("failed final model fit does not silently return a featureless model", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug", function() AutoDebug$new(error_final_train = TRUE))
  on.exit(mlr_auto$add("debug", function() AutoDebug$new()), add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = "debug",
    rush = rush,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_mbo = FALSE
  )

  expect_error(learner$train(task), class = "Mlr3ErrorLearnerTrain")
  expect_gte(sum(learner$instance$archive$data$state == "finished"), 1L)
  expect_error(learner$predict(task), class = "Mlr3ErrorInput")
})

test_that("encapsulated auto learner falls back on a failed final model fit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug", function() AutoDebug$new(error_final_train = TRUE))
  on.exit(mlr_auto$add("debug", function() AutoDebug$new()), add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = "debug",
    rush = rush,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_mbo = FALSE
  )

  learner$encapsulate(method = "mirai", fallback = lrn("classif.featureless"))
  expect_equal(learner$encapsulation, c(train = "none", predict = "none"))

  learner$train(task)
  expect_gte(sum(learner$instance$archive$data$state == "finished"), 1L)

  prediction = learner$predict(task)
  expect_prediction(prediction)
})

test_that("encapsulate rejects a fallback learner without encapsulation", {
  learner = lrn("classif.auto")
  expect_error(
    learner$encapsulate(method = "none", fallback = lrn("classif.featureless")),
    class = "Mlr3ErrorInput"
  )
  expect_error(learner$encapsulate(method = "mirai"), "Learner")
})
