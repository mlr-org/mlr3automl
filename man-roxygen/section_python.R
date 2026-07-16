#' @section Python learners:
#' Python learners like `TabPFN` and `fastai` run via `reticulate` and
#' therefore need a Python installation with their required packages.
#' There are two ways to provide it:
#'
#' 1. Do nothing and let `reticulate::py_require()` install the required packages into an ephemeral virtual
#' environment automatically.
#' 2. Point the `RETICULATE_PYTHON` environment variable to a Python installation that has the required packages
#' installed.
#'
#' We recommend option 2 when running on many workers,
#' as it avoids the overhead of downloading and installing the packages on each worker.
#' Use [install_python_learners()] to create a conda environment with the required packages and set
#' `RETICULATE_PYTHON` to the returned Python binary.
