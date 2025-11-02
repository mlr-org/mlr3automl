#' @section Parameters:
#' \describe{
#'   \item{learner_timeout}{(`integer(1)`)\cr
#'   Timeout for training and predicting with a single learner.}
<<<<<<< HEAD
#'   \item{n_threads}{(`integer(1)`)\cr
#'   Number of threads to use for model training.}
#'   \item{memory_limit}{(`integer(1)`)\cr
#'   Memory limit for model training in MB.}
=======
#'
#'   \item{n_threads}{(`integer(1)`)\cr
#'   Number of threads to use for model training.}
#'
#'   \item{memory_limit}{(`integer(1)`)\cr
#'   Memory limit for model training in MB.}
#'
>>>>>>> main
#'   \item{devices}{(`character()`)\cr
#'   Devices to use for model training.
#'   Possible values are `"cpu"` and `"cuda"`.
#'   If `"cuda"`, the learner will be trained on a GPU.}
<<<<<<< HEAD
=======
#'
>>>>>>> main
#'   \item{large_data_size}{(`integer(1)`)\cr
#'   Threshold value for the data set size from which special rules apply.
#'   Only the learners specified in `large_data_learner_ids` will be considered.
#'   These learners can use up to `large_data_nthread` threads.}
<<<<<<< HEAD
#'   \item{small_data_size}{(`integer(1)`)\cr
#'   Threshold value for the data set size from which special rules apply.}
#'   \item{small_data_resampling}{([mlr3::Resampling])\cr
#'   Resampling strategy to use for model training on small data sets.}
#'   \item{initial_design_size}{(`integer(1)`)\cr
#'   Size of the initial design used for mbo.}
#'   \item{initial_design_type}{(`character(1)`)\cr
#'   Type of the initial design used for mbo.
#'   Possible values are `"default"`, `"lhs"`, `"set"`, `"random"`.
#'   `"default"` uses the default design of the auto.
#'   `"lhs"` uses a Latin Hypercube Sampling design.
#'   `"set"` uses a set of design points.
#'   `"random"` uses a random design.}
#'   \item{adaptive_design}{(`logical(1)`)\cr
#'   Whether to use adaptive design.
#'   If `TRUE`, the initial design will be adapted during the tuning process.}
#'   \item{adaptive_design_fraction}{(`numeric(1)`)\cr
#'   Fraction of the budget to use for adaptive design.
#'   If `adaptive_design` is `TRUE`, the initial design will be adapted during the tuning process.
#'   The fraction of the budget to use for adaptive design.}
=======
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
#'   Fraction of the budget to use for the initial design.}
#'
>>>>>>> main
#'   \item{resampling}{([mlr3::Resampling])\cr
#'   Resampling strategy used for tuning.}
#'
#'   \item{terminator}{([bbotk::Terminator])\cr
#'   Terminator criterion for tuning.}
#'
#'   \item{measure}{([mlr3::Measure])\cr
#'   Measure used for tuning.}
<<<<<<< HEAD
=======
#'
>>>>>>> main
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
<<<<<<< HEAD
#'   \item{check_learners}{(`logical(1)`)\cr
#'   Whether to check if the learners are compatible with the task.
#'   Change to `FALSE` to debug.}
=======
#'
#'   \item{check_learners}{(`logical(1)`)\cr
#'   Whether to check if the learners are compatible with the task.
#'   Change to `FALSE` to debug.}
#'
>>>>>>> main
#' }


