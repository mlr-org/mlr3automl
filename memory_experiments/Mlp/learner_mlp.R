#!/usr/bin/env Rscript
# chmod +x learner_mlp.R
# Rscript learner_mlp.R <nrow> <nfeatures> <n_layers> <neurons> <p> <opt.lr> <opt.weight_decay> <epochs>

args = commandArgs(trailingOnly = TRUE)

if (length(args) != 8L) {
  stop("Usage: learner_mlp.R <nrow> <nfeatures> <n_layers> <neurons> <p> <opt.lr> <opt.weight_decay> <epochs>\n",
       call. = FALSE)
}

args = lapply(args, as.numeric)
names(args) = c("nrow", "nfeatures", "n_layers", "neurons", "p", "opt.lr", "opt.weight_decay", "epochs")


if (any(is.na(args))) {
  stop("Arguments missing\n", call. = FALSE)
}

library(mlr3verse)
library(mlr3torch)

# binary classification

task = tsk("spam")
task$filter(sample(task$row_ids, args$nrow, replace = TRUE))
task$select(sample(task$feature_names, args$nfeatures, replace = TRUE))

learner = lrn(sprintf("%s.mlp", task$task_type),
  id = "mlp",
  # measures_valid = measure,
  batch_size = 32L,
  # patience = self$early_stopping_rounds(task),  # no early stopping

  # parameters to test out memory usage
  n_layers = as.integer(args$n_layers),
  neurons = as.integer(args$neurons),
  p = args$p,
  opt.lr = args$opt.lr,
  opt.weight_decay = args$opt.weight_decay,
  epochs = args$epochs
)

# train and predict once
ids = partition(task)
learner$train(task, row_ids = ids$train)
preds = learner$predict(task, row_ids = ids$test)

