# Critical review: mlr3automl (whole package, main @ 7129f46)

Scope: all of `R/` (~3,150 lines), test suite structure plus core test
files, `DESCRIPTION`, `.Rbuildignore`, pkgdown config, and roxygen
templates. Claims about catboost metric names, lightgbm defaults,
[`data.table::set()`](https://rdrr.io/pkg/data.table/man/assign.html) on
null tables, and `sprintf("%i", <double>)` were verified empirically
against the installed packages, not assumed. Not reviewed: generated
`man/` pages, CI workflows, the full runtime behavior of every learner
test (the suite was not executed).

## Summary

The architecture is sound and in places genuinely well thought out: the
`Auto` registry pattern is clean, the python/libtorch process isolation
(`isolated_model.R`, the `*Isolated` learners) solves a nasty real
problem and documents *why*, and the warm-start design with
task-dependent bounds filtering (`Auto$design_set`) is careful work. But
the package ships two silent-failure correctness bugs in the boosting
learners (catboost gets invalid metric names for the *default*
regression measure; lightgbm trains with 2% of its advertised budget), a
packaging bug that would put a 5.9 GB conda environment into the CRAN
tarball, and a default configuration in which a failed final model fit
silently returns a featureless model. The test suite trains a lot and
asserts almost nothing: exactly one
[`predict()`](https://rdrr.io/r/stats/predict.html) call in ~30 test
files, and the shared test helpers are dead code that every file
re-implements by copy-paste.

## Critical issues (Blocking)

1.  **`.Rbuildignore` last two patterns are broken regexes; the build
    would bundle a 5.9 GB conda env.** The final entries are
    `$oml_cache$` and `$.conda$` — a leading `$` anchors at
    end-of-string, so these patterns match *nothing* (verified:
    `grepl("$oml_cache$", "oml_cache")` is `FALSE`). `R CMD build` will
    therefore pack `oml_cache/` (19 MB) and `.conda/` (5.9 GB) into the
    tarball. `.Rprofile` (which monkey-patches the `languageserver`
    namespace via `unlockBinding` — a dev-machine hack that must never
    ship) is not ignored either. Fix: `^oml_cache$`, `^\.conda$`,
    `^\.Rprofile$`.

2.  **`AutoCatboost$internal_measure()` falls back to metric names that
    do not exist in catboost** (`R/AutoCatboost.R:103,118,128`). The
    `switch` defaults are `"rmse"`, `"error"`, and `"merror"` — xgboost
    names pasted into the catboost table. Verified against the installed
    catboost: `eval_metric = "rmse"` / `"error"` / `"merror"` all abort
    with `Key 'rmse' not found in enum ELossFunction`. This is not an
    exotic edge case: the *default* regression measure is `regr.mse`
    (`train_auto.R:4`), which is unmapped and hits the `"rmse"` fallback
    — so `lrn("regr.auto")$train(task)` with no measure set makes
    **every** catboost evaluation fail. With the default
    `encapsulate_learner = TRUE` those failures are silently converted
    to featureless-fallback scores; nobody sees an error, catboost just
    always “loses”. Same for any unmapped classif measure
    (`classif.fbeta`, multiclass `classif.bacc`, …). Fix: map
    `regr.mse`, and use valid catboost defaults (`"RMSE"`,
    `"Accuracy"`/`"ZeroOneLoss"`, `"MultiClass"`). While here: the
    tuning measure silently diverging from the early-stopping metric on
    fallback deserves at least an `lg$info()` in all three boosting
    autos.

3.  **`AutoLightgbm$graph()` never sets `num_iterations`; lightgbm
    trains with the upstream default of 100 iterations instead of the
    advertised 5,000** (`R/AutoLightgbm.R:44-53`). The search space
    declares
    `lightgbm.num_iterations = p_int(1L, 5000L, tags = "internal_tuning")`
    and line 44 reads the upper bound — but only feeds it into the
    patience calculation; the learner never receives it. Because the
    internal search space is hand-built (no `in_tune_fn`), the caller is
    responsible for configuring the budget on the learner — which is
    exactly why `AutoXgboost` sets `nrounds = nrounds`
    (`AutoXgboost.R:52`) and `AutoCatboost` sets
    `iterations = iterations` (`AutoCatboost.R:52`). Verified: a freshly
    constructed `classif.lightgbm` has `num_iterations = NULL` (upstream
    default 100). Consequences: (a) lightgbm is capped at 100 rounds, 2%
    of the intended budget; (b)
    `early_stopping_rounds(task, budget = 5000)` yields a patience of up
    to 200, which *exceeds* the real 100-round budget, so early stopping
    can never trigger on small tasks — the exact bug class commit \#69
    just fixed for the others; (c) the internal tuning aggregation
    averages best-iteration values from a 100-round run while the search
    space claims \[1, 5000\]. Fix: `num_iterations = num_iterations` in
    the `lrn()` call.

4.  **A failed final model fit silently yields a featureless model.**
    `train_auto.R:73-78` encapsulates the graph learner with a
    featureless fallback for the tuning phase, and the final fit at
    lines 164-173 reuses that same encapsulated learner (only the
    timeout is lifted). If the final `graph_learner$train(task)` fails —
    bad interaction of tuned params, OOM, python env drift on the main
    process — the user gets a “successfully trained” AutoML model that
    predicts the majority class / mean, with no error and no prominent
    warning. For the *tuning* phase a fallback is the right call; for
    the *final* fit it corrupts the deliverable. Disable the fallback
    (or at minimum check for and loudly report a fallback state) before
    the final fit.

## Required changes

5.  **`train_auto()` has no guard against an empty tuning result**
    (`R/train_auto.R:170`). If all evaluations fail (see issues 2 and 4
    — this is reachable), `instance$result_learner_param_vals` is `NULL`
    and `set_values(.values = NULL, .insert = FALSE)` either errors
    cryptically or wipes the parameter set. Check
    `is.null(self$instance$result)` and raise a clear `error_config()`.

6.  **Unvalidated `p_uty` parameters on the user-facing API**
    (`R/LearnerAuto.R:76,88,91-93,96`). `measure`, `resampling`,
    `small_data_resampling`, `terminator`, and `callbacks` accept
    literally anything; a typo like `measure = "classif.ce"` (string
    instead of `msr()`) surfaces as a cryptic failure inside
    `train_auto()`/`ti_async()`. The project’s own rules require
    checkmate validation on user-facing APIs, and `devices` on the very
    next line shows the `custom_check` pattern. Apply it to all five.

7.  **The learner’s `predict_type` is silently ignored**
    (`R/train_auto.R:71`). `LearnerClassifAuto` advertises
    `predict_types = c("response", "prob")`, but the trained graph’s
    predict type is hard-wired to `pv$measure$predict_type`. A user who
    sets `lrn("classif.auto", predict_type = "prob")` with the default
    `classif.ce` measure gets a model that cannot produce probabilities.
    Either honor `self$predict_type` when configuring the graph
    (`max(measure requirement, user request)`) or stop advertising it.

8.  **Stale-clock bug in the timeout callbacks** (`R/helper.R:1-42`).
    Both `cb_timeout_xgboost` and `cb_timeout_lightgbm` lazily
    initialize `start_time` in an environment created once at graph
    construction and never reset it. Cloning a learner does not
    duplicate closure environments, so every in-process training after
    the first measures elapsed time from the *first* training. The bug
    is masked in the default configuration only because mirai/callr
    encapsulation re-serializes the learner per call (fresh environment
    each time). With `encapsulate_learner = FALSE` — the configuration
    used by nearly every test, and on any rush worker that deserializes
    the objective once and clones per evaluation — once cumulative wall
    time passes `learner_timeout * 0.9`, every subsequent
    lightgbm/xgboost training “times out” after one boosting round and
    silently produces a garbage model. Reset the clock unconditionally
    at the start of each training (xgboost: `f_before_training`;
    lightgbm: reset when `env$iteration == 1L`).

9.  **`mlr3automl.initial_design_runtime` breaks when constructed
    without its argument** (`R/mlr_callbacks.R:24`).
    `callback$state$initial_design_fraction` is `NULL` unless supplied,
    so `runtime_limit * NULL` is `numeric(0)` and the `if` throws
    `argument is of length zero` at runtime — and the callback’s own
    `@examples` block constructs it exactly this way. Give the state a
    default (or assert it in an `on_optimization_begin` hook).

10. **Dead code and dead test infrastructure.**

    - `LearnerAuto$tuning_space` (`R/LearnerAuto.R:38-39`): a documented
      public field that nothing in the package reads or writes. Delete
      it.
    - `test_classif_learner()` / `test_regr_learner()`
      (`tests/testthat/helper.R:30-118`): 90 lines of shared helpers
      used by **zero** test files — all 25 learner test files copy-paste
      the same ~30-line rush/lrn/train block instead, and the copies
      have already drifted (some assert archive contents, most assert
      only the class of the return value). Either use the helpers
      everywhere or delete them; the current state is the worst of both.

11. **Test suite trains everything and verifies almost nothing.** There
    is exactly one [`predict()`](https://rdrr.io/r/stats/predict.html)
    call in the entire suite (`test_LearnerClassifAuto.R:243`, glmnet).
    Consequences: the isolated-model unpickle path in
    `LearnerClassif*Isolated$.predict` — the entire point of
    `isolated_model.R` — is never exercised; `predict_type = "prob"` is
    never exercised; predicting with a mirai-encapsulated final model is
    never exercised; and nothing would catch issue 4 (a featureless
    final model still passes `expect_class(learner$train(task), ...)`).
    At minimum, each learner test should predict on held-out rows and
    assert the prediction is non-degenerate, and one test should
    round-trip a tabpfn/fastai model through marshal/unmarshal +
    predict.

12. **`AutoTabpfn$check()` does the expensive check first and skips the
    cheap ones** (`R/AutoTabpfn.R:37-56`). It spawns a callr session and
    resolves a python environment (`check_python_packages`, potentially
    tens of seconds on first run) *before* the free checks in
    `super$check()` (R packages installed, task type, memory).
    `AutoFastai$check()` gets the order right. Also, when both python
    learners are selected, `check_python_packages()` runs two callr
    sessions per `$train()` call, both resolving `torch`, with no
    caching. Reorder, and memoize the probe result per session.

13. **Copy-paste `estimate_memory()` bodies.** The identical heuristic
    `task$nrow * task$ncol * 8 * 10 / 1e6` appears five times
    (`AutoTabpfn.R:97-101`, `AutoFastai.R:96-100`, `AutoMlp.R:68-72`,
    `AutoResNet.R:66-70`, `AutoFTTransformer.R:107-111`), differing only
    in the log prefix; ranger and extra_trees duplicate a second
    heuristic with drifted naming (`tree_size` vs `tree_size_bytes`,
    `AutoRanger.R:62` vs `AutoExtraTrees.R:74`). Hoist the shared
    formula into `Auto` (e.g. a `memory_factor` field) and log from one
    place.

14. **Documentation defects.**

    - `NEWS.md` does not exist. The project’s own rules require a bullet
      per user-facing change; the package has three authors and 60+
      commits of user-facing behavior with zero changelog.
    - `man-roxygen/section_debugging.R`: “run the tuning in the in the
      main session” (doubled words); `options(bbotk.debug)` is missing
      `= TRUE`.
    - `man-roxygen/section_parameters.R`: `small_data_size` is
      documented as “from which special rules apply” — say *what* rules
      (switches to `small_data_resampling`). `learner_timeout` does not
      mention it is only enforced via encapsulation/boosting callbacks,
      i.e. not at all for e.g. ranger with
      `encapsulate_learner = FALSE`.
    - `R/install_python_learners.R:60` recommends
      `options(reticulate.python = ...)`. Verify this option actually
      exists in reticulate; the documented mechanism is the
      `RETICULATE_PYTHON` environment variable. Also `conda_create()`
      errors unhelpfully if the env already exists — check and message
      first.

## Suggestions

15. `R/train_auto.R:153` calls
    [`mlr3mbo::default_surrogate()`](https://mlr3mbo.mlr-org.com/reference/default_surrogate.html)
    although mlr3mbo is imported wholesale — the project style rule says
    no `::` for imported packages; the rest of the file complies.

16. Empty `learner_ids` (`assert_subset` at `LearnerAuto.R:69` accepts
    `character(0)`) produces the misleading error “All learners have no
    hyperparameters to tune.” Use `assert_subset(..., empty.ok = FALSE)`
    or `assert_character(min.len = 1)`.

17. Feature/property claims vs. reality (verify): `feature_types`
    includes `"character"` (`LearnerAuto.R:111`) but no pipeline
    converts character to factor — `encode`/`fixfactors` operate on
    factor/ordered, so character columns reach xgboost/glmnet unencoded.
    Conversely `"ordered"` is handled by every `imputesample` selector
    but not declared. `"weights"` is claimed (`LearnerClassifAuto.R:33`)
    while inner learners like kknn and svm do not support weights. None
    of the bundled test tasks (penguins, spam, mtcars) contain character
    or ordered columns or weights, so tests cannot catch any of this.

18. Naming inconsistency across the same concepts: `AutoTabpfn` /
    `AutoMlp` / `AutoSvm` / `AutoLightgbm` vs `LearnerClassifAutoTabPFN`
    / `AutoMLP` / `AutoSVM` / `AutoLightGBM`, and test files
    (`test_LearnerClassifAutoTabpfn.R`, `test_LearnerClassifAutoMlp.R`)
    that match neither the R file names (`LearnerClassifAutoTabPFN.R`,
    `LearnerClassifAutoMLP.R`) — which breaks the project’s own
    `test_{name}.R` convention and `devtools::test_active_file()`.

19. `generate_initial_design()` (`R/helper.R:114-119`): the trailing
    comma in the `switch` silently returns `NULL` for an unknown method,
    which then fails downstream as “incorrect number of dimensions”. The
    value is currently constrained by `p_fct`, but a `stopf()` default
    costs one line. Also note the design is generated over the full
    space including internal-tuning params whose columns are then
    discarded — wasted sobol/lhs dimensions.

20. `AutoResNet`/`AutoFTTransformer` declare `devices = "cuda"` only, so
    `check()` excludes them for CPU users, yet `graph()` happily builds
    a CPU (`device = "auto"`) learner. Either they work on CPU (then
    allow it, perhaps gated on task size like tabpfn) or they don’t
    (then drop the `"auto"` branch). The current split means
    `check_learners = FALSE` behaves differently from what `check()`
    enforces.

21. [`sapply()`](https://rdrr.io/r/base/lapply.html) in
    `AutoFTTransformer.R:60` — project style is mlr3misc `map_*` (and
    `sapply`’s simplification is exactly the kind of latent bug the rule
    exists to prevent).

22. `deep_clone` (`LearnerAuto.R:130-142`) clones `self$instance`
    (field) and `state$model$instance` separately, so after a deep clone
    the field and the model hold two *different* instance objects that
    were identical before. Consider dropping the public `instance` field
    entirely — the model already carries it, and the field is redundant
    state that can drift.

## What is genuinely good

- `isolated_model.R` plus the `*Isolated` subclasses: a clean, minimal
  solution to the libtorch/python-torch conflict, with comments that
  explain the *why* at exactly the right level (`AutoFastai.R:151-161`
  is model documentation for tricky code).
- `Auto$design_set()` warm-start handling — measure filtering, task
  exclusion via option, and dropping out-of-bounds points for
  task-dependent spaces — is careful and has a dedicated regression test
  (`test_Auto.R:56-80`).
- The patience-vs-budget capping in `Auto$early_stopping_rounds()` (#69)
  is the right idea — it just needs to actually apply to lightgbm (issue
  3).
- `tests/testthat/helper.R:8-13` filtering out mlr3’s strict R6
  debugging helpers with an explanation of the torch incompatibility:
  exactly how a workaround should be recorded.

## Verdict

**Request Changes.** Issues 1–4 are shippable-corruption class: a 5.9 GB
tarball, catboost dead on arrival for default regression settings,
lightgbm at 2% of its advertised budget, and silent featureless final
models. None are hard to fix; all are hard to notice — which is
precisely why the missing prediction-quality assertions in the test
suite (issue 11) should be treated as part of the fix, not a
nice-to-have.

## Next steps

1.  Fix the three one-line blockers (1, 2, 3) and add regression tests
    that would have caught them (a catboost regr run with the default
    measure; an assertion that lightgbm’s trained model saw
    `num_iterations = 5000`).
2.  Decide the intended semantics for the final-fit fallback (4) — fail
    loudly vs. warn-and-return — and implement it.
3.  Work through the Required list; most items are mechanical (asserts,
    dedup, docs).
4.  Re-run `R CMD build` and inspect the tarball file list to confirm
    the packaging fix.
