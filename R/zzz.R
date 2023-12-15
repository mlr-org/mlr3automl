#' @import bbotk
#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import mlr3learners
#' @import mlr3extralearners
#' @import mlr3mbo
#' @import mlr3misc
#' @import mlr3pipelines
#' @import mlr3tuning
#' @import mlr3tuningspaces
#' @import paradox
#' @import R6

"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3tuning.timeout", load_callback_timeout)
} # nocov end

leanify_package()
