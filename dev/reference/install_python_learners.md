# Install Python Learners

Creates a conda environment and installs the packages required by the
Python learners. Installs the dependencies of the fastai auto learner,
the tabfm auto learner, and the tabpfn auto learner. The learners share
a single environment, so one `RETICULATE_PYTHON` covers all of them.

## Usage

``` r
install_python_learners(
  learners = c("fastai", "tabfm", "tabpfn"),
  envname = file.path(tools::R_user_dir("mlr3automl", which = "data"),
    "mlr3automl-python"),
  python_version = "3.12"
)
```

## Arguments

- learners:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Learners to install the Python dependencies for. One or more of
  `"fastai"`, `"tabfm"`, and `"tabpfn"`.

- envname:

  (`character(1)`)  
  Path to the conda environment directory. Defaults to
  `mlr3automl-python` under [tools::R_user_dir("mlr3automl",
  "data")](https://rdrr.io/r/tools/userdir.html).

- python_version:

  (`character(1)`)  
  Python version to use. Pinned to `"3.12"` by default so the
  environment is reproducible across machines.

## Value

Invisibly returns the path to the Python binary in the environment.

## Details

The environment is created under
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) by
default, not inside the project directory: the environment can exceed
10GB with torch/CUDA dependencies, and a project-local path risks
getting bundled into source tarballs on `R CMD build`/`renv::install()`.

## Examples

``` r
if (FALSE) { # \dontrun{
install_python_learners()
} # }
```
