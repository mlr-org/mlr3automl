#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import mlr3mbo
#' @import mlr3misc
#' @import mlr3pipelines
#' @import mlr3tuning
#' @import paradox
#' @import R6
#' @importFrom rush rush_config
#' @import lhs
#' @import mlr3learners

"_PACKAGE"

#' @include aaa.R
register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(learners, function(obj, nm) x$add(nm, obj))

  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  iwalk(callbacks, function(obj, nm) x$add(nm, obj))
}

.onLoad = function(libname, pkgname) {
  # nocov start
  register_namespace_callback(pkgname, "mlr3", register_mlr3)

  # setup logger
  lg = lgr::get_logger("mlr3/mlr3automl")
  assign("lg", lg, envir = parent.env(environment()))
  f = function(event) {
    event$msg = paste0("[mlr3automl] ", event$msg)
    TRUE
  }
  lg$set_filters(list(f))
} # nocov end

.onUnload = function(libpaths) { # nolint
  mlr_learners = mlr3::mlr_learners
  mlr_callbacks = mlr3misc::mlr_callbacks
  walk(names(learners), function(id) mlr_learners$remove(id))
  walk(names(callbacks), function(id) mlr_callbacks$remove(id))
}

leanify_package()
