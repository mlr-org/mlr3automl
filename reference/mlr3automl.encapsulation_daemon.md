# Encapsulation Daemon Callback

This
[mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
starts a persistent [mirai](https://CRAN.R-project.org/package=mirai)
daemon on each rush worker that is reused for the `"mirai"`
encapsulation of the tuned learners. Reusing a single daemon avoids the
overhead of starting a new daemon for every learner evaluation. The
daemon is started when the worker begins and its liveness is checked
before every evaluation. If the daemon has died, it is restarted.

## Parameters

- `n_daemons` :: `integer(1)`  
  Number of daemons to start on each worker. Defaults to `1`.

## Examples

``` r
clbk("mlr3automl.encapsulation_daemon", n_daemons = 1)
#> <CallbackAsyncTuning:mlr3automl.encapsulation_daemon>: Encapsulation Daemon Callback
#> * Active Stages: on_worker_end, on_optimizer_before_eval,
#>   on_worker_begin
```
