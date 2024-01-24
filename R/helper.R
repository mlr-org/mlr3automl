get_branch_pipeline = function(task_type, learner_ids) {
  learners = set_names(map(learner_ids, function(id) set_threads(lrn(sprintf("%s.%s", task_type, id), id = id), 8)), learner_ids)

  # create branch
  graph = ppl("branch", graphs = learners)

  # fix learners
  if ("svm" %in% learner_ids) {
    if (task_type == "classif") {
      values = list(svm.type = "C-classification")
    } else if (task_type == "regr") {
      values = list(svm.type = "eps-regression")
    }
    graph$param_set$set_values(.values = values)
  }

  graph
}

get_search_space = function(task_type, learner_ids, tuning_space) {
  # create branch
  graph = get_branch_pipeline(task_type, learner_ids)
  graph$param_set$set_values(branch.selection = to_tune(learner_ids))
  graph$param_set$set_values(.values = tuning_space)

  # create search space
  search_space = graph$param_set$search_space()
 # search_space$deps = deps[id %in% search_space$ids()]

  # add dependencies
  walk(learner_ids, function(learner_id) {
    param_ids = search_space$ids()
    param_ids = grep(paste0("^", learner_id), param_ids, value = TRUE)
    walk(param_ids, function(param_id) {
      search_space$add_dep(
        id = param_id,
        on = "branch.selection",
        cond = CondEqual$new(learner_id)
      )
    })
  })

  search_space
}

generate_initial_design = function(task_type, learner_ids, task, tuning_space) {
  map_dtr(learner_ids, function(learner_id) {
    learner = lrn(sprintf("%s.%s", task_type, learner_id))

    token = tuning_space[grep(paste0("^", learner_id), names(tuning_space))]

    # learner without tuning space
    if (!length(token)) {
      return(data.table(branch.selection = learner_id))
    }

    names(token) = gsub(paste0("^", learner_id, "."), "", names(token))
    learner$param_set$set_values(.values = token)
    search_space = learner$param_set$search_space()
    xss = default_values(learner, search_space = search_space, task = task)
    has_logscale = map_lgl(search_space$params, function(param) get_private(param)$.has_logscale)
    xdt = as.data.table(map_if(xss, has_logscale, function(value) if (value > 0) log(value) else value))

    # fix nrounds
    if (learner_id == "xgboost") {
      xdt[, nrounds := 50L]
    }

    setnames(xdt, sprintf("%s.%s", learner_id, names(xdt)))
    xdt[, branch.selection := learner_id]
  }, .fill = TRUE)
}

default_surrogate = function(instance = NULL, learner = NULL, n_learner = NULL, search_space = NULL, noisy = NULL) {
  assert_r6(instance, "OptimInstance", null.ok = TRUE)
  assert_r6(learner, "Learner", null.ok = TRUE)
  if (is.null(instance)) {
    noisy = assert_flag(noisy)
    search_space = assert_param_set(search_space)
    assert_int(n_learner, lower = 1L)
  } else {
    noisy = "noisy" %in% instance$objective$properties
    search_space = instance$search_space
    assert_int(n_learner, lower = 1L, null.ok = TRUE)
  }

  if (is.null(learner)) {
    is_mixed_space = !all(search_space$class %in% c("ParamDbl", "ParamInt"))
    has_deps = nrow(search_space$deps) > 0L
    require_namespaces("mlr3learners")
    if (!is_mixed_space) {
      require_namespaces("DiceKriging")
      learner = mlr3learners::LearnerRegrKM$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE))
      )
      if (noisy) {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.estim = TRUE, jitter = 1e-12))
      } else {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.stability = 10^-8))
      }
    } else {
      require_namespaces("ranger")
      learner = mlr3learners::LearnerRegrRanger$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(num.trees = 500L, keep.inbag = TRUE, se.method = "jack")
      )
    }
    # Stability: evaluate and add a fallback
    learner$encapsulate[c("train", "predict")] = "evaluate"
    require_namespaces("ranger")
    fallback = mlr3learners::LearnerRegrRanger$new()
    fallback$param_set$values = insert_named(
      fallback$param_set$values,
      list(num.trees = 20L, keep.inbag = TRUE, se.method = "jack")
    )
    learner$fallback = fallback

    if (has_deps) {
      require_namespaces("mlr3pipelines")
      learner = mlr3pipelines::GraphLearner$new(
        mlr3pipelines::"%>>%"(
          mlr3pipelines::"%>>%"(
            mlr3pipelines::po("imputesample", affect_columns = mlr3pipelines::selector_type("logical")),
              mlr3pipelines::"%>>%"(
                mlr3pipelines::po("imputeoor", multiplier = 3, affect_columns = mlr3pipelines::selector_type(c("integer", "numeric", "character", "factor", "ordered"))),
                mlr3pipelines::po("colapply", applicator = as.factor, affect_columns = mlr3pipelines::selector_type("character"))
              )
          ),
          learner
        )
      )
      learner$encapsulate[c("train", "predict")] = "evaluate"
      learner$fallback = LearnerRegrFeatureless$new()
    }
  }

  if (is.null(n_learner)) n_learner = length(instance$archive$cols_y)
  if (n_learner == 1L) {
    SurrogateLearner$new(learner)
  } else  {
    learners = replicate(n_learner, learner$clone(deep = TRUE), simplify = FALSE)
    SurrogateLearnerCollection$new(learners)
  }
}
