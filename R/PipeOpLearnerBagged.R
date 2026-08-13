#' @title Bagged Learner PipeOp
#'
#' @description
#' Wraps a learner graph into a bagged ensemble in the style of AutoGluon and TabArena.
#' During training, the graph is fitted on `folds` cross-validation folds of the training data,
#' so each child model is trained on all but one fold.
#' The out-of-fold predictions are scored with `measure` and the score is reported as `$internal_valid_scores`.
#' During prediction, the child models predict on the new data and their predictions are combined with equal weights.
#' Probabilities are averaged, responses are aggregated by majority vote, and regression responses are averaged.
#'
#' If the terminal learner supports validation, each child uses its held-out fold as validation set,
#' so internally tuned parameters (e.g., early-stopped boosting rounds) are tuned per child.
#' The tuned values are aggregated across the children with the aggregation rules of `internal_search_space`
#' and reported as `$internal_tuned_values`.
#'
#' When `refit` is set, a single model is trained on the complete data instead of the ensemble,
#' and neither an out-of-fold score nor internally tuned values are produced.
#' This is meant for the final model fit of learners whose ensemble is too expensive to deploy,
#' after the tuned values of the ensemble have been set from the outside.
#'
#' @section Parameters:
#' The parameters of the terminal learner, as well as:
#' * `bagging.folds` :: `integer(1)`\cr
#'   Number of cross-validation folds. Initialized to `8`.
#' * `bagging.refit` :: `logical(1)`\cr
#'   Whether to train a single model on the complete data instead of the cross-validated ensemble.
#'   Initialized to `FALSE`.
#'
#' @return Object of class [R6::R6Class] and `PipeOpLearnerBagged`.
#'
#' @export
PipeOpLearnerBagged = R6Class(
  "PipeOpLearnerBagged",
  inherit = PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param graph ([mlr3pipelines::Graph])\cr
    #'   Learner graph to bag.
    #'   The terminal [mlr3pipelines::PipeOpLearner] must have the id `id`.
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting object.
    #' @param measure ([mlr3::Measure])\cr
    #'   Measure to score the out-of-fold predictions with.
    #' @param internal_search_space ([paradox::ParamSet])\cr
    #'   Internal search space with aggregation rules for the internally tuned parameters.
    #'   The ids must match the names of the `$internal_tuned_values` of the wrapped graph learner.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings.
    initialize = function(graph, id, measure, internal_search_space = NULL, param_vals = list()) {
      private$.learner = as_learner(graph, clone = TRUE)
      assert_string(id)
      private$.terminal_id = id
      terminal = private$.learner$graph$pipeops[[id]]
      if (is.null(terminal)) {
        stopf("Graph of PipeOpLearnerBagged '%s' has no terminal pipeop with id '%s'", id, id)
      }
      private$.measure = assert_measure(measure)$clone(deep = TRUE)
      # pool test predictions across resampling iterations into a single prediction object
      private$.measure$average = "micro"
      private$.internal_search_space = assert_r6(internal_search_space, "ParamSet", null.ok = TRUE)

      type = private$.learner$task_type
      assert_choice(type, c("classif", "regr"))
      task_type = mlr_reflections$task_types[type, mult = "first"]$task
      out_type = mlr_reflections$task_types[type, mult = "first"]$prediction

      private$.bagging_param_set = ps(
        folds = p_int(lower = 2L, upper = Inf, init = 8L, tags = c("train", "required")),
        refit = p_lgl(init = FALSE, tags = c("train", "required"))
      )

      super$initialize(
        id,
        param_set = alist(
          bagging = private$.bagging_param_set,
          private$.learner$graph$pipeops[[private$.terminal_id]]$param_set
        ),
        param_vals = param_vals,
        input = data.table(name = "input", train = task_type, predict = task_type),
        output = data.table(name = "output", train = "NULL", predict = out_type),
        packages = c("mlr3automl", private$.learner$packages),
        properties = c("validation", "internal_tuning"),
        tags = "learner"
      )
    }
  ),

  active = list(
    #' @field validate (`character(1)` or `NULL`)\cr
    #' Only `NULL` and `"predefined"` are accepted.
    #' The pipeop creates its validation data internally from the cross-validation folds,
    #' so an incoming validation task is discarded.
    validate = function(rhs) {
      if (!missing(rhs)) {
        private$.validate = assert_choice(rhs, "predefined", null.ok = TRUE)
      }
      private$.validate
    },

    #' @field internal_valid_scores (named `list()` or `NULL`)\cr
    #' Score of the out-of-fold predictions, named by the measure id.
    internal_valid_scores = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$state) || is_noop(self$state)) {
        return(NULL)
      }
      self$state$internal_valid_scores
    },

    #' @field internal_tuned_values (named `list()` or `NULL`)\cr
    #' Internally tuned values aggregated across the child models.
    internal_tuned_values = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$state) || is_noop(self$state)) {
        return(NULL)
      }
      self$state$internal_tuned_values
    },

    #' @field predict_type (`character(1)`)\cr
    #' Predict type of the wrapped graph learner.
    predict_type = function(rhs) {
      if (!missing(rhs)) {
        private$.learner$predict_type = rhs
      }
      private$.learner$predict_type
    },

    #' @field learner ([mlr3pipelines::GraphLearner])\cr
    #' Wrapped graph learner.
    learner = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.learner)) {
        stop("$learner is read-only.")
      }
      private$.learner
    },

    #' @field learner_model ([mlr3pipelines::GraphLearner] or `list()`)\cr
    #' If trained, the child models as list of graph learners.
    #' Otherwise, the wrapped graph learner.
    learner_model = function(rhs) {
      if (!missing(rhs)) {
        stop("$learner_model is read-only.")
      }
      if (is.null(self$state) || is_noop(self$state)) {
        return(private$.learner)
      }
      map(self$state$cv_model_states, function(state) {
        learner = private$.learner$clone(deep = TRUE)
        learner$state = state
        learner
      })
    }
  ),

  private = list(
    .learner = NULL,
    .terminal_id = NULL,
    .measure = NULL,
    .internal_search_space = NULL,
    .bagging_param_set = NULL,
    .validate = "predefined",
    .state_class = "pipeop_learner_bagged_state",

    .train = function(inputs) {
      pv = private$.bagging_param_set$values

      # complete task since tuning resampling is set to insample
      task = inputs[[1L]]$clone()
      task$internal_valid_task = NULL

      terminal = private$.learner$graph$pipeops[[private$.terminal_id]]
      has_validation = "validation" %in% terminal$properties

      # a single model on the complete data, used for the final model fit of learners that deploy one model.
      # the tuned values of the ensemble are set from the outside, so no scoring and no internal tuning happen here
      if (pv$refit) {
        learner = private$.learner$clone(deep = TRUE)
        if (has_validation) {
          set_validate(learner, NULL)
        }
        learner$train(task)
        self$state = list(
          cv_model_states = list(learner$state),
          internal_valid_scores = NULL,
          internal_tuned_values = NULL
        )
        return(list(NULL))
      }

      if (has_validation) {
        set_validate(private$.learner, "test")
        on.exit(set_validate(private$.learner, NULL), add = TRUE)
      }

      rr = resample(task, private$.learner, rsmp("cv", folds = pv$folds), store_models = TRUE)

      # the measure is micro-averaged, so this scores the pooled out-of-fold prediction
      score = rr$aggregate(private$.measure)

      internal_tuned_values = if (!is.null(private$.internal_search_space)) {
        tuned_values = transpose_list(map(rr$learners, "internal_tuned_values"))
        private$.internal_search_space$aggr_internal_tuned_values(tuned_values)
      }

      # the children are graph learners, so their tuned values are already prefixed with the terminal id.
      # the outer graph learner prefixes with the pipeop id again, so the prefix is stripped here.
      if (!is.null(internal_tuned_values)) {
        names(internal_tuned_values) = sub(
          sprintf("^%s\\.", private$.terminal_id),
          "",
          names(internal_tuned_values)
        )
      }

      self$state = list(
        cv_model_states = map(rr$learners, "state"),
        internal_valid_scores = set_names(list(unname(score)), private$.measure$id),
        internal_tuned_values = internal_tuned_values
      )

      list(NULL)
    },

    .predict = function(inputs) {
      task = inputs[[1L]]

      predictions = map(self$state$cv_model_states, function(state) {
        on.exit({
          private$.learner$state = NULL
        })
        private$.learner$state = state
        private$.learner$predict(task)
      })

      # a refit state holds a single model, whose prediction is passed through unchanged
      if (length(predictions) == 1L) {
        return(list(predictions[[1L]]))
      }

      prediction = if (private$.learner$task_type == "classif") {
        private$.avg_prediction_classif(predictions, task)
      } else {
        private$.avg_prediction_regr(predictions, task)
      }
      list(prediction)
    },

    # equal-weight average of the child predictions.
    # probabilities are averaged, responses are aggregated by majority vote.
    .avg_prediction_classif = function(predictions, task) {
      lvls = task$class_names
      if (every(predictions, function(prediction) "prob" %in% prediction$predict_types)) {
        prob = Reduce(`+`, map(predictions, "prob")) / length(predictions)
        prob = pmin(pmax(prob, 0), 1)
        response = factor(colnames(prob)[max.col(prob, ties.method = "random")], levels = lvls)
        keep_prob = "prob" %in% private$.learner$predict_type
        PredictionClassif$new(task = task, response = response, prob = if (keep_prob) prob)
      } else {
        votes = matrix(0L, nrow = task$nrow, ncol = length(lvls))
        for (prediction in predictions) {
          index = cbind(seq_len(task$nrow), as.integer(factor(prediction$response, levels = lvls)))
          votes[index] = votes[index] + 1L
        }
        response = factor(lvls[max.col(votes, ties.method = "random")], levels = lvls)
        PredictionClassif$new(task = task, response = response)
      }
    },

    .avg_prediction_regr = function(predictions, task) {
      response = Reduce(`+`, map(predictions, "response")) / length(predictions)
      PredictionRegr$new(task = task, response = response)
    },

    .additional_phash_input = function() {
      list(
        private$.learner$phash,
        private$.terminal_id,
        private$.measure$hash,
        if (!is.null(private$.internal_search_space)) private$.internal_search_space$ids()
      )
    }
  )
)

#' @export
marshal_model.pipeop_learner_bagged_state = function(model, inplace = FALSE, ...) {
  model$cv_model_states = map(model$cv_model_states, marshal_model, inplace = inplace)
  # only wrap in a marshaled class if a child state was actually marshaled
  if (some(model$cv_model_states, is_marshaled_model)) {
    model = structure(
      list(marshaled = model, packages = "mlr3automl"),
      class = c(paste0(class(model), "_marshaled"), "marshaled")
    )
  }
  model
}

#' @export
unmarshal_model.pipeop_learner_bagged_state_marshaled = function(model, inplace = FALSE, ...) {
  state = model$marshaled
  state$cv_model_states = map(state$cv_model_states, unmarshal_model, inplace = inplace)
  state
}
