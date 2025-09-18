
# mlr3automl <img src="man/figures/logo.png" align="right" width = "120" />

Package website: [release](https://mlr3automl.mlr-org.com/) \|
[dev](https://mlr3automl.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3automl/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3automl/actions/workflows/r-cmd-check.yml)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3automl)](https://cran.r-project.org/package=mlr3automl)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

**mlr3automl** is the Automated Machine Learning (AutoML) package of the
[mlr3](https://mlr-org.com/) ecosystem. It automatically selects the
most suitable machine learning algorithm and tunes its hyperparameters
for a given task. The package includes 10 learners from the
`mlr3learners` package, ranging from simple models like `glmnet` to more
powerful algorithms such as `ranger` and `xgboost`. Leveraging the
`mlr3pipelines` package, it constructs sophisticated preprocessing
graphs with multiple parallel branches, which are jointly optimized
using the `mlr3tuning` package. The optimization is driven by
Asynchronous Decentralized Bayesian Optimization (ADBO), enabling
efficient and scalable AutoML.

## Installation

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3automl")
```

## Examples

``` r
library("mlr3automl")

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

task = tsk("spam")

learner = lrn("classif.auto",
  terminator = trm("evals", n_evals = 100)
)

learner$train(task)
```
