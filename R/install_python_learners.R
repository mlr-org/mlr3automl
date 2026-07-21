#' @title Install Python Learners
#'
#' @description
#' Creates a conda environment and installs the packages required by the Python learners.
#' Installs the dependencies of the fastai auto learner and the tabpfn auto learner.
#' The learners share a single environment, so one `RETICULATE_PYTHON` covers both.
#'
#' @details
#' The environment is created in the project directory by default.
#'
#' @param learners (`character()`)\cr
#'   Learners to install the Python dependencies for.
#'   One or more of `"fastai"` and `"tabpfn"`.
#' @param envname (`character(1)`)\cr
#'   Path to the conda environment directory.
#'   Defaults to `.conda/mlr3automl-python` in the current working directory.
#' @param python_version (`character(1)`)\cr
#'   Python version to use.
#'   Pinned to `"3.12"` by default so the environment is reproducible across machines.
#'
#' @return Invisibly returns the path to the Python binary in the environment.
#'
#' @examples
#' \dontrun{
#' install_python_learners()
#' }
#' @export
install_python_learners = function(
  learners = c("fastai", "tabpfn"),
  envname = file.path(getwd(), ".conda", "mlr3automl-python"),
  python_version = "3.12"
) {
  assert_subset(learners, c("fastai", "tabpfn"), empty.ok = FALSE)
  assert_string(envname)
  assert_string(python_version)
  require_namespaces("reticulate")

  # torch is shared by both learners
  packages = "torch"
  if ("fastai" %in% learners) {
    packages = c(
      packages,
      "torchvision",
      "torchaudio",
      "fastai",
      # fastai (<= 2.8.7) is incompatible with fastcore 2.0 but does not declare an upper bound
      "fastcore<2.0.0",
      "IPython",
      "pydicom",
      "kornia",
      "opencv-python",
      "scikit-image"
    )
  }
  if ("tabpfn" %in% learners) {
    packages = c(packages, "tabpfn")
  }
  packages = unique(packages)

  reticulate::conda_create(envname = envname, python_version = python_version)
  reticulate::conda_install(envname = envname, packages = packages, pip = TRUE)
  python = reticulate::conda_python(envname)

  messagef("Python environment '%s' created successfully.", envname)
  messagef("Set `options(reticulate.python = \"%s\")` to use it.", python)
  messagef("Or set `Sys.setenv(RETICULATE_PYTHON = \"%s\")` to use it.", python)
  invisible(python)
}
