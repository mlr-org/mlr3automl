# Install Python Learners

Creates a conda environment and installs the packages required by the
Python learners. Installs the dependencies of the fastai auto learner
and the tabpfn auto learner. The learners share a single environment, so
one `RETICULATE_PYTHON` covers both.

## Usage

``` r
install_python_learners(
  learners = c("fastai", "tabpfn"),
  envname = file.path(getwd(), ".conda", "mlr3automl-python"),
  python_version = "3.12"
)
```

## Arguments

- learners:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Learners to install the Python dependencies for. One or more of
  `"fastai"` and `"tabpfn"`.

- envname:

  (`character(1)`)  
  Path to the conda environment directory. Defaults to
  `.conda/mlr3automl-python` in the current working directory.

- python_version:

  (`character(1)`)  
  Python version to use. Pinned to `"3.12"` by default so the
  environment is reproducible across machines.

## Value

Invisibly returns the path to the Python binary in the environment.

## Details

The environment is created in the project directory by default.
