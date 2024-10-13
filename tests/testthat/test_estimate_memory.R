n_obs = 1e4
n_feat = 100
dt = runif(n_obs * n_feat)
dt = as.data.table(matrix(dt, nrow = n_obs, ncol = n_feat))

# classif
set(dt, j = "y", value = 0)
set(dt, i = seq_len(n_obs %/% 2), j = "y", value = 1)
task = as_task_classif(dt, target = "y")

test_that("LearnerClassifRanger memory estimate is approximately correct", {
  learner = lrn("classif.ranger")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})

test_that("LearnerClassifXgboost memory estimate is approximately correct", {
  learner = lrn("classif.xgboost")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})

test_that("LearnerClassifCatboost memory estimate is approximately correct", {
  learner = lrn("classif.catboost")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})

test_that("LearnerClassifLightGBM memory estimate is approximately correct", {
  learner = lrn("classif.lightgbm")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})


# regr
set(dt, j = "y", value = runif(n_obs))
task = as_task_regr(dt, target = "y")

test_that("LearnerRegrRanger memory estimate is approximately correct", {
  learner = lrn("regr.ranger")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})

test_that("LearnerRegrXgboost memory estimate is approximately correct", {
  learner = lrn("regr.xgboost")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})

test_that("LearnerRegrCatboost memory estimate is approximately correct", {
  learner = lrn("regr.catboost")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})

test_that("LearnerRegrLightGBM memory estimate is approximately correct", {
  learner = lrn("regr.lightgbm")
  learner$train(task)
  true_mem = object.size(learner$model)
  estim_mem = estimate_memory(learner, task)
  
  expect_true(true_mem <= estim_mem)
})

rm(dt)
rm(task)
rm(n_obs)
rm(n_feat)
