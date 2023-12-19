test_that("classification graph is constructed", {
  skip_on_ci()

  learner_ids = c("J48", "DecisionTable", "KStar", "LMT", "PART", "SMO","BayesNet", "JRip", "SimpleLogistic",
    "VotedPerceptron", "SGD", "Logistic", "OneR", "MultilayerPerceptron", "REPTree", "IBk", "RandomForestWEKA",
    "RandomTree")

  graph = get_branch_pipeline("classif", learner_ids)
  expect_class(graph, "Graph")
})

test_that("classification search_space is constructed", {
  skip_on_ci()

  learner_ids = c("J48", "DecisionTable", "KStar", "LMT", "PART", "SMO", "BayesNet", "JRip", "SimpleLogistic",
    "VotedPerceptron", "SGD", "Logistic", "OneR", "MultilayerPerceptron", "REPTree", "IBk", "RandomForestWEKA",
    "RandomTree")

  search_space = get_search_space("classif", learner_ids, tuning_space_classif_autoweka)
  expect_class(search_space, "ParamSet")
})

test_that("classification search space can be set", {
  skip_on_ci()

  learner_ids = c("J48", "DecisionTable", "KStar", "LMT", "PART", "SMO", "BayesNet", "JRip", "SimpleLogistic",
    "VotedPerceptron", "SGD", "Logistic", "OneR", "MultilayerPerceptron", "REPTree", "IBk", "RandomForestWEKA",
    "RandomTree")

  graph = get_branch_pipeline("classif", learner_ids)
  learner = as_learner(graph)
  search_space = get_search_space("classif", learner_ids, tuning_space_classif_autoweka)

  design = generate_design_random(search_space,  1000)$data
  xss = transform_xdt_to_xss(design, search_space)

  iwalk(xss, function(xs, i) {
    learner$param_set$values = list()
    expect_class(learner$param_set$set_values(.values = xs), "ParamSet")
  })
})

test_that("regression search space can be set", {
  skip_on_ci()

  learner_ids = c("DecisionTable", "M5P", "KStar", "LinearRegression", "SGD",
    "MultilayerPerceptron", "REPTree", "IBk", "M5Rules", "RandomForestWEKA", "RandomTree",
    "GaussianProcesses","SMOreg")

  graph = get_branch_pipeline("regr", learner_ids)
  learner = as_learner(graph)
  search_space = get_search_space("regr", learner_ids, tuning_space_regr_autoweka)

  design = generate_design_random(search_space,  1000)$data
  xss = transform_xdt_to_xss(design, search_space)

  iwalk(xss, function(xs, i) {
    print(i)
    learner$param_set$values = list()
    expect_class(learner$param_set$set_values(.values = xs), "ParamSet")
  })
})

test_that("LearnerClassifAutoWEKA train works", {
  skip_on_ci()

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_names(learner$packages, must.include = "RWeka")

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerRegrAutoWEKA train works", {
  skip_on_ci()

  task = tsk("mtcars")
  resampling = rsmp("holdout")
  measure = msr("regr.mse")
  terminator = trm("run_time", secs = 10)
  learner = LearnerRegrAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_names(learner$packages, must.include = "RWeka")

  expect_class(learner$train(task), "LearnerRegrAutoWEKA")
  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAutoWEKA resample works", {
  skip_on_ci()

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_resample_result(resample(task, learner, rsmp("holdout")))
})

test_that("LearnerClassifAutoWEKA train works with parallelization", {
  skip_on_ci()

  future::plan("multisession", workers = 2)

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_class(learner$model, "AutoTuner")
})

test_that("callback timeout works", {
  skip_on_ci()

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 20)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    callbacks = clbk("mlr3tuning.timeout", time_limit = 20))

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_lte(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"], 20)
})


test_that("callback timeout works", {
  skip_on_ci()

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 20)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    callbacks = clbk("mlr3tuning.timeout", time_limit = 20, max_time_limit = 5))

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_lte(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"], 5)
})
