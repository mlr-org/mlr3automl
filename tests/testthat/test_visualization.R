skip_on_cran()
skip_if_not_installed("rush")
skip_if_not_installed(c("glmnet", "kknn", "ranger", "e1071"))

# cost over time
test_that("cost over time works", {
  task = tsk("penguins")

  set.seed(1453)
  flush_redis()
  rush_plan(n_workers = 2)

  learner = lrn("classif.auto_ranger",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )
  learner$train(task)

  vdiffr::expect_doppelganger("cot-config-id", cost_over_time(learner$instance))
  vdiffr::expect_doppelganger("cot-timestamp-xs", cost_over_time(learner$instance, time = "timestamp_xs"))
  vdiffr::expect_doppelganger("cot-timestamp-ys", cost_over_time(learner$instance, time = "timestamp_ys"))
})


# marginal plots
test_that("marginal plot works", {
  task = tsk("penguins")

  # numeric vs numeric
  set.seed(1453)
  flush_redis()
  rush_plan(n_workers = 2)

  learner_glmnet = lrn("classif.auto_glmnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )
  learner_glmnet$train(task)
  vdiffr::expect_doppelganger(
    "mp-numeric-numeric",
    marginal_plot(learner_glmnet$instance, x = "glmnet.alpha", y = "glmnet.s")
  )
  vdiffr::expect_doppelganger(
    "mp-numeric-numeric2",
    marginal_plot(learner_glmnet$instance, x = "glmnet.s", y = "glmnet.alpha")
  )

  # numeric vs factor
  set.seed(1453)
  flush_redis()
  rush_plan(n_workers = 2)
  
  learner_kknn = lrn("classif.auto_kknn",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )
  learner_kknn$train(task)
  vdiffr::expect_doppelganger(
    "mp-numeric-factor",
    marginal_plot(learner_kknn$instance, x = "kknn.distance", y = "kknn.kernel")
  )
  vdiffr::expect_doppelganger(
    "mp-factor-numeric",
    marginal_plot(learner_kknn$instance, x = "kknn.kernel", y = "kknn.distance")
  )

  # numeric vs logical
  set.seed(1453)
  flush_redis()
  rush_plan(n_workers = 2)
  
  learner_ranger = lrn("classif.auto_ranger",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )
  learner_ranger$train(task)
  vdiffr::expect_doppelganger(
    "mp-numeric-logical",
    marginal_plot(learner_ranger$instance, x = "ranger.num.trees", y = "ranger.replace")
  )
  vdiffr::expect_doppelganger(
    "mp-logical-numeric",
    marginal_plot(learner_ranger$instance, x = "ranger.replace", y = "ranger.num.trees")
  )
})

test_that("marginal plot accepts params on different branches", {
  task = tsk("penguins")

  set.seed(1453)
  flush_redis()
  rush_plan(n_workers = 2)
  
  learner = lrn("classif.auto",
    learner_ids = c("kknn", "svm"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )
  learner$train(task)
  vdiffr::expect_doppelganger(
    "mp-different-branches",
    marginal_plot(learner$instance, x = "kknn.distance", y = "svm.cost")
  )
})

test_that("marginal plot handles dependence", {
  task = tsk("penguins")

  set.seed(1453)
  flush_redis()
  rush_plan(n_workers = 2)
  
  learner_svm = lrn("classif.auto_svm",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )
  learner_svm$train(task)
  vdiffr::expect_doppelganger(
    "mp-dependence",
    marginal_plot(learner_svm$instance, x = "svm.kernel", y = "svm.degree")
  )
})


# parallel coordinates

# pdp

# pareto front

# footprint

