#' @section Parameters:
#' \describe{
#'   \item{learner_timeout}{(`integer(1)`)\cr
#'   Timeout for training and predicting with a single learner.}
#'
#'   \item{n_threads}{(`integer(1)`)\cr
#'   Number of threads used for training a single learner.}
#'
#'   \item{n_cpu}{(named `integer()`)\cr
#'   Number of CPUs a single training of a learner uses, named by learner id, e.g. `c(xgboost = 1)`.
#'   Overrides the default of the learner.
#'   Must be at least 1 because the worker always runs on the CPU.
#'   Currently informational only; the number of threads is controlled by `n_threads`.}
#'
#'   \item{n_gpu}{(named `integer()`)\cr
#'   Number of GPUs a single training of a learner uses, named by learner id, e.g. `c(xgboost = 1)`.
#'   Overrides the default of the learner.
#'   Can only be 0 or 1 for now.
#'   The torch learners, TabPFN, and fastai default to 1; all other learners default to 0.
#'   Only effective when `"cuda"` is part of `devices`; otherwise every learner is trained on the CPU.
#'   When the requirements are mixed, the search space is partitioned into a cpu and a gpu subspace and
#'   tuned with [mlr3mbo::TunerADBOSubspaces], see `subspace_profiles`.}
#'
#'   \item{subspace_profiles}{(named `character()`)\cr
#'   \CRANpkg{mirai} compute profile of the cpu and the gpu subspace, e.g. `c(cpu = "cpu", gpu = "gpu")`.
#'   Must have the names `"cpu"` and `"gpu"` and the two profiles must be different.
#'   Defaults to the profiles named `"cpu"` and `"gpu"`.
#'   When the learners have mixed `n_gpu` requirements and the daemons of both profiles are set up with
#'   [rush::rush_plan()], the search space is partitioned into a cpu and a gpu subspace which are tuned
#'   with [mlr3mbo::TunerADBOSubspaces].
#'   The workers of a profile only ever propose and evaluate points of the subspace of that profile,
#'   so the number of workers per subspace is the number of workers of its profile.
#'   Otherwise the cpu and gpu learners are tuned in a single search space with [mlr3mbo::TunerAsyncMbo].
#'
#'   ```
#'   mirai::daemons(7, .compute = "cpu")
#'   mirai::daemons(1, .compute = "gpu")
#'   rush::rush_plan(profiles = c(cpu = 7, gpu = 1))
#'   ```
#'   }
#'
#'   \item{memory_limit}{(`integer(1)`)\cr
#'   Memory limit for training a single learner in MB.
#'   The limit is shared across the parallel workers, i.e. divided by the number of workers.}
#'
#'   \item{devices}{(`character()`)\cr
#'   Devices to use for model training.
#'   Possible values are `"cpu"` and `"cuda"`.
#'   If `"cuda"`, learners with a `n_gpu` requirement of 1 are trained on a GPU,
#'   while the remaining learners stay on the CPU.}
#'
#'   \item{large_data_size}{(`integer(1)`)\cr
#'   Threshold for the data set size (number of rows times number of columns) above
#'   which large-data rules apply.
#'   Beyond this threshold the number of parallel workers is reduced and each remaining
#'   worker is given proportionally more threads and memory.
#'   When the workers are distributed over \CRANpkg{mirai} compute profiles, the number of workers of every
#'   profile is reduced, but every profile keeps at least one worker.}
#'
#'   \item{small_data_size}{(`integer(1)`)\cr
#'   Threshold value for the data set size from which special rules apply.}
#'
#'   \item{small_data_resampling}{([mlr3::Resampling])\cr
#'   Resampling strategy to use for model training on small data sets.}
#'
#'   \item{initial_design_default}{(`logical(1)`)\cr
#'   Whether to use the default design of the learner.}
#'
#'   \item{initial_design_set}{(`integer(1)`)\cr
#'   Number of points to use for the initial design set.}
#'
#'   \item{initial_design_size}{(`integer(1)`)\cr
#'   Size of the random, sobol or lhs initial design.}
#'
#'   \item{initial_design_type}{(`character(1)`)\cr
#'   Type of the initial design used for mbo.
#'   Possible values are `"lhs"`, `"sobol"`, `"random"`.
#'   `"lhs"` uses a Latin Hypercube Sampling design.
#'   `"sobol"` uses a Sobol sequence design.
#'   `"random"` uses a random design.}
#'
#'   \item{initial_design_fraction}{(`numeric(1)`)\cr
#'   Fraction of the budget to use for the initial design.
#'   When the search space is partitioned into a cpu and a gpu subspace, the remaining points of both
#'   designs are dropped, because every compute profile has its own queue.}
#'
#'   \item{resampling}{([mlr3::Resampling])\cr
#'   Resampling strategy used for tuning.}
#'
#'   \item{terminator}{([bbotk::Terminator])\cr
#'   Terminator criterion for tuning.}
#'
#'   \item{measure}{([mlr3::Measure])\cr
#'   Measure used for tuning.}
#'
#'   \item{callbacks}{([mlr3tuning::CallbackAsyncTuning])\cr
#'   Callbacks used for tuning.}
#'
#'   \item{store_benchmark_result}{(`logical(1)`)\cr
#'   Whether to store the benchmark result.}
#'
#'   \item{store_models}{(`logical(1)`)\cr
#'   Whether to store the models.}
#'
#'   \item{encapsulate_learner}{(`logical(1)`)\cr
#'   Whether to encapsulate the learner.
#'   Change to `FALSE` to debug.}
#'
#'   \item{encapsulate_mbo}{(`logical(1)`)\cr
#'   Whether to encapsulate the tuning.
#'   Change to `FALSE` to debug.}
#'
#'   \item{check_learners}{(`logical(1)`)\cr
#'   Whether to check if the learners are compatible with the task.
#'   Change to `FALSE` to debug.}
#'
#' }
