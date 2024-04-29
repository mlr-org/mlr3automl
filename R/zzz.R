#' @import bbotk
#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import mlr3learners
#' @import mlr3mbo
#' @import mlr3misc
#' @import mlr3pipelines
#' @import mlr3tuning
#' @import mlr3tuningspaces
#' @import paradox
#' @import R6

"_PACKAGE"

#' @include aaa.R
register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  iwalk(learners, function(obj, nm) x$add(nm, obj))
}

.onLoad = function(libname, pkgname) {
  # nocov start
  register_namespace_callback(pkgname, "mlr3", register_mlr3)

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3automl.branch_nrounds", load_callback_branch_nrounds)
  x$add("mlr3automl.nrounds", load_callback_nrounds)

  # setup logger
  lg = lgr::get_logger(pkgname)
  assign("lg", lg, envir = parent.env(environment()))
  f = function(event) {
    event$msg = paste0("[mlr3automl] ", event$msg)
    TRUE
  }
  lg$set_filters(list(f))
} # nocov end

.onUnload = function(libpaths) { # nolint
  mlr_learners = mlr3::mlr_learners

  walk(names(learners), function(id) mlr_learners$remove(id))
}

leanify_package()
