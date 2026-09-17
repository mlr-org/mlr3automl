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
    bagging_small_size = 1,
    bagging_folds = 2L,
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

test_that("bagging tunes with out-of-fold scores and deploys the ensemble", {
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
    bagging_small_size = 1,
    bagging_folds = 3L,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)
  archive = learner$instance$archive$data
  expect_numeric(archive[state == "finished", internal_valid_score], any.missing = FALSE)
  expect_disjunct("classif.ce", names(archive))

  # the final model is a bagged ensemble trained with the winning configuration
  state = learner$model$graph_learner$graph_model$pipeops$debug$state
  expect_list(state$cv_model_states, len = 3L)
  expect_number(state$internal_valid_scores$classif.ce)

  prediction = learner$predict(task)
  expect_prediction(prediction)
})

test_that("small data sets are bagged with a repeated cross-validation", {
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

  mlr_auto$add("debug", function() AutoDebug$new())

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = "debug",
    rush = rush,
    small_data_size = 1,
    bagging_folds = 8L,
    bagging_small_size = 400L,
    bagging_small_folds = 3L,
    bagging_small_repeats = 2L,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)

  # penguins has fewer rows than `bagging_small_size`, so the small data set folds and repeats apply
  state = learner$model$graph_learner$graph_model$pipeops$debug$state
  expect_list(state$cv_model_states, len = 6L)
  expect_prediction(learner$predict(task))
})

test_that("learners with the bagging_refit property deploy a single final model", {
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

  AutoDebugRefit = R6Class("AutoDebugRefit", inherit = AutoDebug,
    public = list(
      initialize = function() {
        super$initialize()
        self$properties = c(self$properties, "bagging_refit")
      }
    )
  )
  mlr_auto$add("debug", function() AutoDebugRefit$new())
  on.exit(mlr_auto$add("debug", function() AutoDebug$new()), add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = "debug",
    rush = rush,
    small_data_size = 1,
    bagging_small_size = 1,
    bagging_folds = 3L,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)
  # the ensemble is scored during tuning, but a single model is deployed
  archive = learner$instance$archive$data
  expect_numeric(archive[state == "finished", internal_valid_score], any.missing = FALSE)

  state = learner$model$graph_learner$graph_model$pipeops$debug$state
  expect_list(state$cv_model_states, len = 1L)
  expect_null(state$internal_valid_scores)

  prediction = learner$predict(task)
  expect_prediction(prediction)
})

test_that("missing internal valid scores are imputed with the penalty score", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_not_installed("glmnet")
  skip_if_no_redis()

  # a single worker evaluates the initial design in order, so the terminator cannot cancel the
  # glmnet configuration while the much faster failing debug configurations fill up the archive
  rush = start_rush(n_workers = 1)
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  mlr_auto$add("debug", function() AutoDebug$new(error_train = 1))
  on.exit(mlr_auto$add("debug", function() AutoDebug$new()), add = TRUE)

  task = tsk("penguins")
  learner = lrn(
    "classif.auto",
    learner_ids = c("debug", "glmnet"),
    rush = rush,
    small_data_size = 1,
    bagging_small_size = 1,
    bagging_folds = 3L,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    # the default configuration of every learner is evaluated first, so both branches are scored
    initial_design_default = TRUE,
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_mbo = FALSE
  )

  learner$train(task)
  archive = learner$instance$archive$data
  finished = archive[state == "finished"]
  expect_numeric(finished$internal_valid_score, any.missing = FALSE)
  # the failing branch is imputed with the penalized featureless baseline score
  imputed = finished[branch.selection == "debug", internal_valid_score]
  expect_true(all(imputed > 0.5))
  expect_lte(uniqueN(imputed), 1L)
  expect_equal(learner$instance$result$branch.selection, "glmnet")
})

test_that("bagging = FALSE keeps the prediction-based tuning", {
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
    bagging_small_size = 1,
    bagging = FALSE,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)
  archive = learner$instance$archive$data
  expect_subset("classif.ce", names(archive))
  expect_disjunct("internal_valid_score", names(archive))

  prediction = learner$predict(task)
  expect_prediction(prediction)
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
    bagging_small_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_mbo = FALSE,
    bagging_folds = 3L
  )

  # the failing child cancels the remaining folds of the bag, which future.apply reports as a warning
  expect_error(suppressWarnings(learner$train(task)), class = "Mlr3ErrorLearnerTrain")
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
    bagging_small_size = 1,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    initial_design_type = "random",
    initial_design_size = 2,
    encapsulate_mbo = FALSE,
    bagging_folds = 3L
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
    bagging_small_size = 1,
    bagging_folds = 2L,
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

  profiles = c(mlr3automl_cpu = 1, mlr3automl_gpu = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
    bagging_small_size = 1,
    bagging_folds = 2L,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 8),
    initial_design_type = "random",
    initial_design_size = 4,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)

  # the internally tuned parameters live in the internal search space and must not be part of the subspaces
  expect_set_equal(learner$instance$internal_search_space$ids(), c("debug_cpu.iter", "debug_gpu.iter"))
  expect_names(learner$instance$search_space$ids(), disjunct.from = c("debug_cpu.iter", "debug_gpu.iter"))

  data = learner$instance$archive$data
  expect_names(names(data), must.include = ".subspace")
  # the subspace is only written when an evaluation finishes
  finished = data[state == "finished"]
  expect_equal(finished$.subspace, ifelse(finished$branch.selection == "debug_gpu", "gpu", "cpu"))
  expect_set_equal(unique(finished$.subspace), c("cpu", "gpu"))
})

test_that("mixed requirements keep the single search space when the compute profiles do not match", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  profiles = c(cores = 1, cuda = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
    bagging_small_size = 1,
    bagging_folds = 2L,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 8),
    initial_design_default = TRUE,
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

test_that("a large data set reduces the workers of the cpu compute profiles but not of the gpu profile", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  profiles = c(mlr3automl_cpu = 4, mlr3automl_gpu = 2)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
    large_data_size = 1,
    small_data_size = 1,
    bagging_small_size = 1,
    bagging_folds = 2L,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 8),
    initial_design_type = "random",
    initial_design_size = 4,
    # the debug learners are not compatible with large data sets
    check_learners = FALSE,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  learner$train(task)

  # the 4 cpu daemons are served by one worker, while the gpu profile keeps both of its workers
  worker_info = rush$worker_info
  expect_equal(sum(worker_info$profile == "mlr3automl_cpu"), 1L)
  expect_equal(sum(worker_info$profile == "mlr3automl_gpu"), 2L)
  expect_equal(nrow(worker_info), 3L)
})

test_that("mixed requirements keep the single search space without compute profiles", {
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
    bagging_small_size = 1,
    bagging_folds = 2L,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6),
    initial_design_default = TRUE,
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

test_that("gpu learners fall back to the cpu without a cuda device", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  profiles = c(mlr3automl_cpu = 1, mlr3automl_gpu = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
    bagging_small_size = 1,
    bagging_folds = 2L,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 8),
    initial_design_default = TRUE,
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

  profiles = c(mlr3automl_cpu = 1, mlr3automl_gpu = 1)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
    bagging_small_size = 1,
    bagging_folds = 2L,
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

test_that("mixed requirements keep the single search space without a gpu compute profile", {
  skip_on_cran()
  skip_if_not_installed("rush")
  # the surrogate model of the mbo tuner requires ranger
  skip_if_not_installed("ranger")
  skip_if_no_redis()

  profiles = c(mlr3automl_cpu = 2)
  rush = start_rush_profiles(profiles)
  on.exit({
    rush$reset()
    stop_rush_profiles(profiles)
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
    bagging_small_size = 1,
    bagging_folds = 2L,
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
