#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import R6
#' @import mlr3mbo
#' @import mlr3pipelines
#' @import bbotk
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3tuning.timeout", load_callback_timeout)
  x$add("mlr3tuning.initial_design", load_callback_initial_design)
} # nocov end

leanify_package()
