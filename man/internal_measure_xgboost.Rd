% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal_measure.R
\name{internal_measure_xgboost}
\alias{internal_measure_xgboost}
\title{Internal Measure XGBoost}
\usage{
internal_measure_xgboost(measure, task)
}
\arguments{
\item{measure}{\link[mlr3:Measure]{mlr3::Measure}\cr
Measure to get the internal measure for.}

\item{task}{\link[mlr3:Task]{mlr3::Task}\cr
Task to get the internal measure for.}
}
\description{
Function to get the internal xgboost measure for a given \link[mlr3:Task]{mlr3::Task} and \link[mlr3:Measure]{mlr3::Measure}.
For example, the measure "classif.auc" will return "auc" for a binary classification task.
}
\examples{
internal_measure_xgboost(msr("classif.auc"), tsk("pima"))
}
