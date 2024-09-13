#' @title Estimate Memory Usage of a Learner
#'
#' @description
#' This function estimates the memory usage of a learner on a given task.
#'
#' @param learner [mlr3::Learner]\cr
#' The learner to estimate the memory usage for.
#'
#' @param task [mlr3::Task]\cr
#' The task to estimate the memory usage for.
#'
#' @return `numeric(1)`\cr
#' The estimated memory usage in bytes.

#' @export
estimate_memory = function(learner, task, ...) {
  assert_task(task)

  UseMethod("estimate_memory")
}

#' @rdname estimate_memory
#' @export
estimate_memory.Learner = function(learner, task, ...) {
  return(-Inf)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerClassifRanger = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  num_trees = pv$num.trees %??% 500
  tree_size = task$nrow  / 60000 * 1e6

  return(tree_size * num_trees)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerRegrRanger = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  num_trees = pv$num.trees %??% 500
  tree_size = task$nrow  / 60000 * 1e6

  return(tree_size * num_trees)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerClassifXgboost = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  # histogram size
  max_depth = pv$max_depth %??% 6
  max_bin = pv$max_bin %??% 256
  if (max_depth < 6) max_depth = 6
  histogram_size = max_bin * task$ncol * 2^max_depth

  # data size
  data_size = task$nrow * task$ncol * 8

  return(histogram_size + data_size)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerRegrXgboost = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  # histogram size
  max_depth = pv$max_depth %??% 6
  max_bin = pv$max_bin %??% 256
  if (max_depth < 6) max_depth = 6
  histogram_size = max_bin * task$ncol * 2^max_depth

  # data size
  data_size = task$nrow * task$ncol * 8

  return(histogram_size + data_size)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerClassifCatboost = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  # histogram size
  border_count = pv$border_count %??% 254
  depth = pv$depth %??% 6
  histogram_size = 20 * task$ncol * border_count * 2^depth

  # data size
  data_set_size = task$nrow * task$ncol * 8
  data_size = data_set_size * 5 + data_set_size / 4 * length(task$class_names)

  return(histogram_size + data_size)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerRegrCatboost = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  # histogram size
  border_count = pv$border_count %??% 254
  depth = pv$depth %??% 6
  histogram_size = 20 * task$ncol * border_count * 2^depth

  # data size
  data_set_size = task$nrow * task$ncol * 8
  data_size = data_set_size * 5 + data_set_size / 4

  return(histogram_size + data_size)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerClassifLightGBM = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  # histogram size
  max_bin = pv$max_bin %??% 255
  num_leaves = pv$num_leaves %??% 31
  histogram_size = 20 * task$ncol * num_leaves * max_bin

  # data size
  data_set_size = task$nrow * task$ncol * 8
  data_size = data_set_size * 5 + data_set_size / 4 * length(task$class_names)

  return(histogram_size + data_size)
}

#' @rdname estimate_memory
#' @export
estimate_memory.LearnerRegrLightGBM = function(learner, task, ...) {
  pv = learner$param_set$get_values()

  # histogram size
  max_bin = pv$max_bin %??% 255
  num_leaves = pv$num_leaves %??% 31
  histogram_size = 20 * task$ncol * num_leaves * max_bin

  # data size
  data_set_size = task$nrow * task$ncol * 8
  data_size = data_set_size * 5 + data_set_size / 4
  return(histogram_size + data_size)
}



