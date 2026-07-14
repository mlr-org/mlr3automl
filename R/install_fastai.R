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
    # fastai (<= 2.8.7) is incompatible with fastcore 2.0 but does not declare an upper bound
    "fastcore<2.0.0",
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
  messagef("Fastai environment '%s' created successfully.", envname)
  messagef("Set `options(reticulate.python = \"%s\")` or `Sys.setenv(RETICULATE_PYTHON = \"%s\")` to use it.", python, python)
  invisible(python)
}
