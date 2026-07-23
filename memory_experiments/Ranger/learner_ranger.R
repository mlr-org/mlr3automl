#!/usr/bin/env Rscript
# chmod +x learner_ranger.R
# Rscript learner_ranger.R <nrow> <nfeatures> <num.trees> <mtry.ratio> <sample.fraction> <replace>

args = commandArgs(trailingOnly = TRUE)

if (length(args) != 6L) {
  stop("Usage: learner_ranger.R <nrow> <nfeatures> <num.trees> <mtry.ratio> <sample.fraction> <replace>\n",
       call. = FALSE)
}

args = lapply(args, as.numeric)
names(args) = c("nrow", "nfeatures", "num.trees", "mtry.ratio", "sample.fraction", "replace")

if (any(is.na(args))) {
  stop("Arguments missing\n", call. = FALSE)
}

library(mlr3verse)
library(data.table)

# binary classification

# synthetic data instead of tsk("spam"):
# sampling task features with replace = TRUE silently deduplicates,
# so nfeatures would be capped at the 57 unique spam features
n = as.integer(args$nrow)
p = as.integer(args$nfeatures)

data = as.data.table(matrix(rnorm(n * p), nrow = n))
# signal on the first (up to) 5 features so trees grow to realistic depths
data$y = factor(ifelse(rowSums(data[, seq_len(min(5L, p)), with = FALSE]) + rnorm(n) > 0, "A", "B"))
task = as_task_classif(data, target = "y", id = "synthetic")

learner = lrn("classif.ranger",
  id = "ranger",
  num.threads = 1L,  # matches the per-worker deployment in train_auto

  # parameters to test out memory usage
  num.trees = as.integer(args$num.trees),
  mtry.ratio = args$mtry.ratio,
  sample.fraction = args$sample.fraction,
  replace = args$replace == 1
)

# train and predict once
ids = partition(task)
learner$train(task, row_ids = ids$train)
preds = learner$predict(task, row_ids = ids$test)
