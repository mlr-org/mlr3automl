
# mlr3automl <img src="man/figures/logo.png" align="right" width = "120" />

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3automl/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3automl/actions/workflows/r-cmd-check.yml)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3automl)](https://cran.r-project.org/package=mlr3automl)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

*mlr3automl* is the Automated Machine Learning (AutoML) package of the
[mlr3](https://mlr-org.com/) ecosystem.

## Installation

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3automl")
```

## Examples

``` r
library("mlr3automl")

rush_plan(n_workers = 2)

task = tsk("spam")

learner = lrn("classif.auto",
  terminator = trm("evals", n_evals = 100)
)

learner$train(task)
```
