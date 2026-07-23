
# Build hyperparameter tables ####

# HP ranges from AutoRanger
# mtry.ratio      = p_dbl(0, 1)
# replace         = p_lgl()
# sample.fraction = p_dbl(1e-1, 1)
# num.trees       = p_int(500L, 2000L)

# default values from AutoRanger
# mtry.ratio = 0.5
# replace = TRUE (encoded as 1)
# sample.fraction = 0.632
# num.trees = 1000L

# test bash script with a small config
df_default = data.frame(
  nrow = 1000L,
  nfeatures = 10L,
  num_trees = 50L,
  mtry_ratio = 0.5,
  sample_fraction = 0.632,
  replace = 1L
)

write.csv(df_default, file = "parameters_ranger_test.csv")


# should not (or barely) have an influence ####

# replace
df_replace = data.frame(
  nrow = 10000L,
  nfeatures = 50L,
  num_trees = 1000L,
  mtry_ratio = 0.5,
  sample_fraction = 0.632,
  replace = c(0L, 1L)
)

# mtry.ratio (drives runtime, memory influence unclear)
df_mtry = data.frame(
  nrow = 10000L,
  nfeatures = 50L,
  num_trees = 1000L,
  mtry_ratio = c(0.1, 0.25, 0.5, 0.75, 1),
  sample_fraction = 0.632,
  replace = 1L
)

write.csv(rbind(df_replace, df_mtry), file = "parameters_ranger_no_influence.csv")


# grid with nrow, nfeatures, num_trees, sample_fraction ####
# forest memory should be roughly multiplicative: num_trees * nrow * sample_fraction
param_grid = expand.grid(
  nrow = c(100L, 1000L, 10000L, 100000L),
  nfeatures = c(10L, 100L, 1000L),
  num_trees = c(500L, 1000L, 2000L),
  mtry_ratio = 0.5,
  sample_fraction = c(0.1, 0.55, 1),
  replace = 1L
)

# keep the raw data below ~400 MB (drops 100000 x 1000)
param_grid = param_grid[param_grid$nrow * param_grid$nfeatures <= 5e7, ]

write.csv(param_grid, file = "parameters_ranger_grid.csv")


# large data sets at default hyperparameters ####
# ranger has the large_data_sets property, so the fit should be anchored beyond 1e5 rows;
# run this after the main grid, each config takes a while
param_large = expand.grid(
  nrow = c(200000L, 500000L, 1000000L),
  nfeatures = c(10L, 50L),
  num_trees = 1000L,
  mtry_ratio = 0.5,
  sample_fraction = 0.632,
  replace = 1L
)

write.csv(param_large, file = "parameters_ranger_large.csv")
