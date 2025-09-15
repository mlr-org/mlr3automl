#' @section Parameters:
#' \describe{
#'   \item{learner_timeout}{(`integer(1)`)\cr
#'   Timeout for training and predicting with a single learner.}
#'   \item{xgboost_eval_metric}{(`character(1)`)\cr
#'   Early stopping eval metric for XGBoost.
#'   If `NULL`, attempting to match the internal XGBoost eval metric with the mlr3  `measure`}.
#'   \item{catboost_eval_metric}{(`character(1)`)\cr
#'   Early stopping eval metric for CatBoost.
#'   If `NULL`, attempting to match the internal CatBoost eval metric with the mlr3 `measure`.}
#'   \item{lightgbm_eval_metric}{(`character(1)`)\cr
#'   Early stopping eval metric for LightGBM.
#'   If `NULL`, attempting to match the internal LightGBM eval metric with the mlr3 `measure`.}
#'   \item{max_nthread}{(`integer(1)`)\cr
#'   Maximum number of threads to use for model training.}
#'   \item{max_memory}{(`integer(1)`)\cr
#'   Maximum memory to use for model training.}
#'   \item{large_data_size}{(`integer(1)`)\cr
#'   Threshold value for the data set size from which special rules apply.
#'   Only the learners specified in `large_data_learner_ids` will be considered.
#'   These learners can use up to `large_data_nthread` threads.}
#'   \item{large_data_learner_ids}{(`character()`)\cr
#'   Learners to consider for model training on large data sets.}
#'   \item{large_data_nthread}{(`integer(1)`)\cr
#'   Maximum number of threads to use for model training on large data sets.}
#'   \item{small_data_size}{(`integer(1)`)\cr
#'   Threshold value for the data set size from which special rules apply.
#'   Only the learners specified in `small_data_learner_ids` will be considered.
#'   These learners can use up to `small_data_nthread` threads.}
#'   \item{small_data_resampling}{([mlr3::Resampling])\cr
#'   Resampling strategy to use for model training on small data sets.}
#'   \item{max_cardinality}{(`integer(1)`)\cr
#'   Maximum number of factor levels allowed.
#'   Collapses the rarest factors in the data set, until `max_cardinality` levels remain.}
#'   \item{extra_trees_max_cardinality}{(`integer(1)`)\cr
#'   Maximum number of factor levels allowed for extra trees.
#'   Collapses the rarest factors in the data set, until `extra_trees_max_cardinality` levels remain.}
#'   \item{resampling}{([mlr3::Resampling])\cr
#'   Resampling strategy used for tuning.}
#'   \item{terminator}{([bbotk::Terminator])\cr
#'   Terminator criterion for tuning.}
#'   \item{measure}{([mlr3::Measure])\cr
#'   Measure used for tuning.}
#'   \item{lhs_size}{(`integer(1)`)\cr
#'   Size of the initial design used for mbo.}
#'   \item{callbacks}{([mlr3tuning::CallbackAsyncTuning])\cr
#'   Callbacks used for tuning.}
#'   \item{store_benchmark_result}{(`logical(1)`)\cr
#'   Whether to store the benchmark result.}
#'   \item{store_models}{(`logical(1)`)\cr
#'   Whether to store the models.}
#'   \item{encapsulate_learner}{(`logical(1)`)\cr
#'   Whether to encapsulate the learner.
#'   Change to `FALSE` to debug.}
#'   \item{encapsulate_mbo}{(`logical(1)`)\cr
#'   Whether to encapsulate the tuning.
#'   Change to `FALSE` to debug.}
#' }


