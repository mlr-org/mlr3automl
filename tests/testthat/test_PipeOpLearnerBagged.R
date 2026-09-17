bagged_debug_pipeop = function(folds = 3L, predict_type = "prob", measure = msr("classif.ce"), ...) {
  learner = lrn("classif.debug", id = "debug", early_stopping = TRUE, iter = 50L, predict_type = predict_type)
  graph = po("removeconstants", id = "debug_removeconstants") %>>% learner
  internal_search_space = ps(
    debug.iter = p_int(1L, 50L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
  )
  PipeOpLearnerBagged$new(
    graph,
    id = "debug",
    measure = measure,
    internal_search_space = internal_search_space,
    param_vals = list(bagging.folds = folds, ...)
  )
}

test_that("construction exposes the learner parameters unprefixed", {
  pop = bagged_debug_pipeop()

  expect_subset(c("bagging.folds", "bagging.refit"), pop$param_set$ids())
  expect_subset(c("iter", "early_stopping", "x"), pop$param_set$ids())
  # preprocessing parameters stay unexposed
  expect_disjunct("debug_removeconstants.ratio", pop$param_set$ids())
  expect_set_equal(pop$properties, c("validation", "internal_tuning"))
  expect_equal(pop$validate, "predefined")

  glrn = as_learner(po("branch", options = "debug") %>>% pop %>>% po("unbranch", options = "debug"))
  expect_subset(c("debug.iter", "debug.x", "debug.bagging.folds"), glrn$param_set$ids())
})

test_that("values set on the graph reach the inner learner", {
  pop = bagged_debug_pipeop()
  glrn = as_learner(po("branch", options = "debug") %>>% pop %>>% po("unbranch", options = "debug"))

  glrn$param_set$set_values(debug.x = 0.5)
  inner = glrn$graph$pipeops$debug$learner$graph$pipeops$debug$learner
  expect_equal(inner$param_set$values$x, 0.5)

  clone = glrn$clone(deep = TRUE)
  clone$param_set$set_values(debug.x = 0.7)
  inner_clone = clone$graph$pipeops$debug$learner$graph$pipeops$debug$learner
  expect_equal(inner_clone$param_set$values$x, 0.7)
  expect_equal(inner$param_set$values$x, 0.5)
})

test_that("train stores the child states and the out-of-fold score", {
  task = tsk("penguins")
  learner = lrn("classif.featureless", id = "debug", predict_type = "prob")
  graph = po("removeconstants", id = "debug_removeconstants") %>>% learner
  pop = PipeOpLearnerBagged$new(graph, id = "debug", measure = msr("classif.ce"),
    param_vals = list(bagging.folds = 3L))

  expect_null(pop$train(list(task))[[1L]])
  expect_class(pop$state, "pipeop_learner_bagged_state")
  expect_list(pop$state$cv_model_states, len = 3L)
  # featureless always predicts the majority class, so the out-of-fold score is deterministic
  majority = names(which.max(table(task$truth())))
  expect_equal(pop$state$internal_valid_scores$classif.ce, mean(task$truth() != majority))
  expect_null(pop$state$internal_tuned_values)
})

test_that("repeats train folds * repeats child models", {
  task = tsk("penguins")
  learner = lrn("classif.featureless", id = "debug", predict_type = "prob")
  pop = PipeOpLearnerBagged$new(as_graph(learner), id = "debug", measure = msr("classif.ce"),
    param_vals = list(bagging.folds = 3L, bagging.repeats = 2L))

  pop$train(list(task))
  expect_list(pop$state$cv_model_states, len = 6L)
  majority = names(which.max(table(task$truth())))
  expect_equal(pop$state$internal_valid_scores$classif.ce, mean(task$truth() != majority))
})

test_that("internal tuned values are aggregated and reported without double prefix", {
  task = tsk("penguins")
  pop = bagged_debug_pipeop()
  glrn = as_learner(po("branch", options = "debug") %>>% pop %>>% po("unbranch", options = "debug"))
  glrn$validate = "test"

  rr = resample(task, glrn, rsmp("insample"))

  expect_names(names(rr$learners[[1L]]$internal_tuned_values), identical.to = "debug.iter")
  expect_int(rr$learners[[1L]]$internal_tuned_values$debug.iter, lower = 1L, upper = 50L)
  expect_names(names(rr$learners[[1L]]$internal_valid_scores), identical.to = "debug.classif.ce")
  expect_equal(
    unname(rr$aggregate(msr("internal_valid_score", minimize = TRUE))),
    rr$learners[[1L]]$internal_valid_scores$debug.classif.ce
  )
})

test_that("prediction is the equal-weight average of the children", {
  skip_if_not_installed("rpart")
  task = tsk("penguins")
  learner = lrn("classif.rpart", id = "debug", predict_type = "prob")
  pop = PipeOpLearnerBagged$new(as_graph(learner), id = "debug", measure = msr("classif.ce"),
    param_vals = list(bagging.folds = 3L))

  pop$train(list(task))
  prediction = pop$predict(list(task))[[1L]]
  expect_prediction(prediction)

  probs = map(pop$state$cv_model_states, function(state) {
    learner = pop$learner$clone(deep = TRUE)
    learner$state = state
    learner$predict(task)$prob
  })
  expect_equal(prediction$prob, Reduce(`+`, probs) / length(probs))
})

test_that("response-only predictions are aggregated by majority vote", {
  skip_if_not_installed("rpart")
  task = tsk("penguins")
  learner = lrn("classif.rpart", id = "debug")
  pop = PipeOpLearnerBagged$new(as_graph(learner), id = "debug", measure = msr("classif.ce"),
    param_vals = list(bagging.folds = 3L))

  pop$train(list(task))
  prediction = pop$predict(list(task))[[1L]]
  expect_prediction(prediction)
  expect_null(prediction$prob)

  responses = map(pop$state$cv_model_states, function(state) {
    learner = pop$learner$clone(deep = TRUE)
    learner$state = state
    as.character(learner$predict(task)$response)
  })
  # rows with a unique majority must match the majority vote
  counts = pmap(responses, function(...) table(c(...)))
  unique_majority = map_lgl(counts, function(count) sum(count == max(count)) == 1L)
  majority = map_chr(counts, function(count) names(which.max(count)))
  expect_equal(as.character(prediction$response)[unique_majority], majority[unique_majority])
})

test_that("regression responses are averaged", {
  skip_if_not_installed("rpart")
  task = tsk("mtcars")
  learner = lrn("regr.rpart", id = "debug")
  pop = PipeOpLearnerBagged$new(as_graph(learner), id = "debug", measure = msr("regr.mse"),
    param_vals = list(bagging.folds = 3L))

  pop$train(list(task))
  prediction = pop$predict(list(task))[[1L]]
  expect_prediction(prediction)

  responses = map(pop$state$cv_model_states, function(state) {
    learner = pop$learner$clone(deep = TRUE)
    learner$state = state
    learner$predict(task)$response
  })
  expect_equal(prediction$response, Reduce(`+`, responses) / length(responses))
})

test_that("refit mode trains a single model on the complete data", {
  task = tsk("penguins")
  # the tuned values are set from the outside, as the final model fit does after internal tuning is disabled
  pop = bagged_debug_pipeop(bagging.refit = TRUE, early_stopping = FALSE, iter = 7L)

  expect_null(pop$train(list(task))[[1L]])
  expect_class(pop$state, "pipeop_learner_bagged_state")
  expect_list(pop$state$cv_model_states, len = 1L)
  expect_equal(pop$state$cv_model_states[[1L]]$model$debug$model$iter, 7L)
  # no cross-validation ran, so there is nothing to score and nothing to aggregate
  expect_null(pop$state$internal_valid_scores)
  expect_null(pop$state$internal_tuned_values)
})

test_that("refit mode passes the prediction of the single model through", {
  skip_if_not_installed("rpart")
  task = tsk("penguins")
  learner = lrn("classif.rpart", id = "debug", predict_type = "prob")
  pop = PipeOpLearnerBagged$new(as_graph(learner), id = "debug", measure = msr("classif.ce"),
    param_vals = list(bagging.folds = 3L, bagging.refit = TRUE))

  pop$train(list(task))
  single = pop$learner$clone(deep = TRUE)
  single$state = pop$state$cv_model_states[[1L]]

  prediction = pop$predict(list(task))[[1L]]
  expect_prediction(prediction)
  expect_equal(prediction$prob, single$predict(task)$prob)
  expect_equal(prediction$response, single$predict(task)$response)
})

test_that("marshaling round trips the state", {
  task = tsk("penguins")
  pop = bagged_debug_pipeop()
  pop$train(list(task))

  marshaled = marshal_model(pop$state, inplace = FALSE)
  state = unmarshal_model(marshaled)
  expect_class(state, "pipeop_learner_bagged_state")
  expect_equal(state$internal_valid_scores, pop$state$internal_valid_scores)
})

test_that("tuning optimizes the out-of-fold score", {
  task = tsk("penguins")
  pop = bagged_debug_pipeop()
  glrn = as_learner(po("branch", options = "debug") %>>% pop %>>% po("unbranch", options = "debug"))
  glrn$validate = "test"
  glrn$predict_sets = NULL

  search_space = ps(
    debug.x = p_dbl(0, 1),
    debug.iter = p_int(1L, 50L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
  )

  instance = ti(
    task = task,
    learner = glrn,
    resampling = rsmp("insample"),
    measures = msr("internal_valid_score", minimize = TRUE),
    terminator = trm("evals", n_evals = 3L),
    search_space = search_space
  )
  tnr("random_search")$optimize(instance)

  archive = as.data.table(instance$archive, unnest = "internal_tuned_values")
  expect_numeric(archive$internal_valid_score, any.missing = FALSE)
  expect_integer(archive$internal_tuned_values_debug.iter, lower = 1L, upper = 50L)
  expect_int(instance$result_learner_param_vals$debug.iter)
  expect_false(instance$result_learner_param_vals$debug.early_stopping)

  glrn$validate = NULL
  glrn$predict_sets = "test"
  glrn$param_set$set_values(.values = instance$result_learner_param_vals, .insert = FALSE)
  glrn$train(task)
  expect_prediction(glrn$predict(task))
})
