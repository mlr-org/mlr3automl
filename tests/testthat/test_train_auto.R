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

test_that("user requested predict_type is honored even when the measure only needs response", {
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
    encapsulate_mbo = FALSE,
    predict_type = "prob"
  )

  learner$train(task)
  expect_equal(learner$model$graph_learner$predict_type, "prob")

  prediction = learner$predict(task)
  expect_prediction(prediction)
  expect_true("prob" %in% prediction$predict_types)
})

test_that("encapsulate rejects a fallback learner without encapsulation", {
  learner = lrn("classif.auto")
  expect_error(
    learner$encapsulate(method = "none", fallback = lrn("classif.featureless")),
    class = "Mlr3ErrorInput"
  )
  expect_error(learner$encapsulate(method = "mirai"), "Learner")
})

test_that("character and ordered features are converted to factors by the lightgbm branch", {
  # lightgbm supports factor but not character or ordered; its branch must convert both to factors
  set.seed(1)
  n = 60
  data = data.table(
    y = factor(sample(c("a", "b"), n, TRUE)),
    chr = sample(letters[1:3], n, TRUE),
    ord = ordered(sample(c("lo", "mid", "hi"), n, TRUE), c("lo", "mid", "hi")),
    num = rnorm(n)
  )
  task = as_task_classif(data, target = "y")
  expect_subset(c("character", "ordered"), task$feature_types$type)

  result = test_classif_learner("lightgbm", task = task)
  expect_prediction(result$prediction)
})

test_that("mixed cpu and gpu requirements are tuned on subspaces", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug_cpu", function() AutoDebug$new(id = "debug_cpu"))
  mlr_auto$add("debug_gpu", function() AutoDebug$new(id = "debug_gpu", devices = c("cpu", "cuda"), n_gpu = 1L))
  on.exit({
    mlr_auto$remove("debug_cpu")
    mlr_auto$remove("debug_gpu")
  }, add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = c("debug_cpu", "debug_gpu"),
    rush = rush,
    devices = c("cpu", "cuda"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 8),
    initial_design_type = "random",
    initial_design_size = 4,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)

  data = learner$instance$archive$data
  expect_names(names(data), must.include = ".subspace")
  expect_equal(data$.subspace, ifelse(data$branch.selection == "debug_gpu", "gpu", "cpu"))
  expect_set_equal(unique(data$.subspace), c("cpu", "gpu"))
})

test_that("gpu learners fall back to the cpu without a cuda device", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug_cpu", function() AutoDebug$new(id = "debug_cpu"))
  mlr_auto$add("debug_gpu", function() AutoDebug$new(id = "debug_gpu", devices = c("cpu", "cuda"), n_gpu = 1L))
  on.exit({
    mlr_auto$remove("debug_cpu")
    mlr_auto$remove("debug_gpu")
  }, add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = c("debug_cpu", "debug_gpu"),
    rush = rush,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 8),
    initial_design_type = "random",
    initial_design_size = 4,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)

  data = learner$instance$archive$data
  expect_false(".subspace" %in% names(data))
  expect_set_equal(unique(data$branch.selection), c("debug_cpu", "debug_gpu"))
})

test_that("homogeneous gpu requirements keep the single search space", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  rush = start_rush(n_workers = 2)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug_cpu", function() AutoDebug$new(id = "debug_cpu", devices = c("cpu", "cuda")))
  mlr_auto$add("debug_gpu", function() AutoDebug$new(id = "debug_gpu", devices = c("cpu", "cuda"), n_gpu = 1L))
  on.exit({
    mlr_auto$remove("debug_cpu")
    mlr_auto$remove("debug_gpu")
  }, add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = c("debug_cpu", "debug_gpu"),
    rush = rush,
    devices = c("cpu", "cuda"),
    n_gpu = c(debug_cpu = 1L),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 8),
    initial_design_type = "random",
    initial_design_size = 4,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)

  data = learner$instance$archive$data
  expect_false(".subspace" %in% names(data))
})

test_that("a single worker keeps the single search space with mixed requirements", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug_cpu", function() AutoDebug$new(id = "debug_cpu"))
  mlr_auto$add("debug_gpu", function() AutoDebug$new(id = "debug_gpu", devices = c("cpu", "cuda"), n_gpu = 1L))
  on.exit({
    mlr_auto$remove("debug_cpu")
    mlr_auto$remove("debug_gpu")
  }, add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = c("debug_cpu", "debug_gpu"),
    rush = rush,
    devices = c("cpu", "cuda"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6),
    initial_design_type = "random",
    initial_design_size = 4,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)

  data = learner$instance$archive$data
  expect_false(".subspace" %in% names(data))
})
