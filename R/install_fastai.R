#' @title Install Fastai
#'
#' @description
#' Creates a conda environment and installs the Python packages required by the fastai auto learner.
#'
#' @param envname (`character(1)`)\cr
#'   Name of the conda environment.
#' @param python_version (`character(1)`)\cr
#'   Python version to use.
#'
#' @return Invisibly returns the path to the Python binary in the conda environment.
#' @export
install_fastai = function(envname = "mlr3automl-fastai", python_version = "3.11") {
  assert_string(envname)
  assert_string(python_version)
  require_namespaces("reticulate")

  reticulate::conda_create(envname = envname, python_version = python_version)

  packages = c(
    "torch",
    "torchvision",
    "torchaudio",
    "fastai",
    "IPython",
    "pydicom",
    "kornia",
    "opencv-python",
    "scikit-image"
  )

  reticulate::conda_install(
    envname = envname,
    packages = packages,
    pip = TRUE
  )

  python = reticulate::conda_python(envname)
  cli::cli_alert_success("Fastai environment {.val {envname}} created successfully.")
  cli::cli_alert_info("Set {.code options(reticulate.python = \"{python}\")} or {.code Sys.setenv(RETICULATE_PYTHON = \"{python}\")} to use it.")
  invisible(python)
}
