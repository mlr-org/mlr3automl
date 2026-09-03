#' @param isolate_python (`logical(1)`)\cr
#' Whether to train and predict the learner in a fresh `callr` session.
#' Isolation is required when `mlr3torch` is loaded in the same process,
#' because the bundled libtorch versions of the R and Python torch packages are incompatible.
#' Default is `TRUE`.
