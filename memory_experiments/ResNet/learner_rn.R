#!/usr/bin/env Rscript
# chmod +x learner_rn.R
# Rscript learner_rn.R <nrow> <nfeatures> <n_blocks> <d_block> <d_hidden_multiplier> <dropout1> <dropout2> <opt.lr> <opt.weight_decay> <epochs>

args = commandArgs(trailingOnly = TRUE)

if (length(args) != 10L) {
  stop("Usage: learner_rn.R <nrow> <nfeatures> <n_blocks> <d_block> <d_hidden_multiplier> <dropout1> <dropout2> <opt.lr> <opt.weight_decay> <epochs>\n",
       call. = FALSE)
}

args = lapply(args, as.numeric)
names(args) = c("nrow", "nfeatures", "n_blocks", "d_block", "d_hidden_multiplier", "dropout1", "dropout2", "opt.lr", "opt.weight_decay", "epochs")


if (any(is.na(args))) {
  stop("Arguments missing\n", call. = FALSE)
}

library(mlr3verse)
library(mlr3torch)

# binary classification

task = tsk("spam")
task$filter(sample(task$row_ids, args$nrow, replace = TRUE))
task$select(sample(task$feature_names, args$nfeatures, replace = TRUE))

learner = lrn(sprintf("%s.tab_resnet", task$task_type),
  id = "resnet",
  # measures_valid = measure,
  batch_size = 32L,
  # patience = self$early_stopping_rounds(task),  # no early stopping
  device = "auto",
  
  # parameters to test out memory usage
  n_blocks = as.integer(args$n_blocks),
  d_block = as.integer(args$d_block),
  d_hidden_multiplier = args$d_hidden_multiplier,
  dropout1 = args$dropout1,
  dropout2 = args$dropout2,
  opt.lr = args$opt.lr,
  opt.weight_decay = args$opt.weight_decay,
  epochs = args$epochs
)

# train and predict once
ids = partition(task)
learner$train(task, row_ids = ids$train)
preds = learner$predict(task, row_ids = ids$test)

