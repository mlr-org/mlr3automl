% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LearnerAutoBranch.R
\name{LearnerAutoBranch}
\alias{LearnerAutoBranch}
\title{Auto Learner}
\description{
Abstract base class for Auto like learner.
}
\section{Super class}{
\code{\link[mlr3:Learner]{mlr3::Learner}} -> \code{LearnerAutoBranch}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{graph}}{(\link[mlr3pipelines:Graph]{mlr3pipelines::Graph}).}

\item{\code{tuning_space}}{(\code{list()}).}

\item{\code{instance}}{(\link{TuningInstanceRushSingleCrit}).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LearnerAutoBranch-new}{\code{LearnerAutoBranch$new()}}
\item \href{#method-LearnerAutoBranch-clone}{\code{LearnerAutoBranch$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="base_learner"><a href='../../mlr3/html/Learner.html#method-Learner-base_learner'><code>mlr3::Learner$base_learner()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="estimate_memory_usage"><a href='../../mlr3/html/Learner.html#method-Learner-estimate_memory_usage'><code>mlr3::Learner$estimate_memory_usage()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="format"><a href='../../mlr3/html/Learner.html#method-Learner-format'><code>mlr3::Learner$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="help"><a href='../../mlr3/html/Learner.html#method-Learner-help'><code>mlr3::Learner$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict"><a href='../../mlr3/html/Learner.html#method-Learner-predict'><code>mlr3::Learner$predict()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict_newdata"><a href='../../mlr3/html/Learner.html#method-Learner-predict_newdata'><code>mlr3::Learner$predict_newdata()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="print"><a href='../../mlr3/html/Learner.html#method-Learner-print'><code>mlr3::Learner$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="reset"><a href='../../mlr3/html/Learner.html#method-Learner-reset'><code>mlr3::Learner$reset()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="train"><a href='../../mlr3/html/Learner.html#method-Learner-train'><code>mlr3::Learner$train()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LearnerAutoBranch-new"></a>}}
\if{latex}{\out{\hypertarget{method-LearnerAutoBranch-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerAutoBranch$new(id, task_type, param_set, graph, tuning_space)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
Identifier for the new instance.}

\item{\code{task_type}}{(\code{character(1)})\cr
Type of task, e.g. \code{"regr"} or \code{"classif"}.
Must be an element of \link[=mlr_reflections]{mlr_reflections$task_types$type}.}

\item{\code{param_set}}{(\link{ParamSet})\cr
Parameter set.}

\item{\code{graph}}{(\link[mlr3pipelines:Graph]{mlr3pipelines::Graph}).
Graph.}

\item{\code{tuning_space}}{(list of lists of \link[paradox:to_tune]{paradox::TuneToken})\cr
List of tuning spaces.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LearnerAutoBranch-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LearnerAutoBranch-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerAutoBranch$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
