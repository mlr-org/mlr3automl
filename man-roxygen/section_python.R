#' @section Python learners:
#' For using the `TabPFN` and `fastai` learners two options are available:
#' 1) Do nothing and let `py_require`
#'
#' 2) Point the `RETICULATE_PYTHON` environment variable to a Python installation
#' that has the required packages installed.
#'
#' We recommend option 2 when many workers are used,
#' as it avoids the overhead downloading and installing the packages on each worker.
