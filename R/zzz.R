#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import R6
#' @import mlr3
#' @import mlr3tuning
#' @import mlr3tuningspaces
#' @import mlr3mbo
#' @import mlr3pipelines
#' @import bbotk
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3tuning.timeout", load_callback_timeout)
} # nocov end

leanify_package()
