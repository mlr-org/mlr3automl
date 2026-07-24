library(tidyverse)
library(broom)

no_influence = read.csv("memory_experiments/Ranger/memory_result_ranger_no_influence.csv")
param_grid = read.csv("memory_experiments/Ranger/memory_result_ranger_grid.csv")
# optional, run after the main grid
# large = read.csv("memory_experiments/Ranger/memory_result_ranger_large.csv")
# param_grid = bind_rows(param_grid, large)

r_session = 455120 / 1024 # mib, only packages; re-measure with an empty Rscript if the library set changes

no_influence$real_mib = no_influence$rss_mib - r_session # replace, mtry_ratio
param_grid$real_mib = param_grid$rss_mib - r_session

all_res = bind_rows(list(no_influence = no_influence, param_grid = param_grid), .id = "scenario")


# look at results ####

res_fun = function(data, col) {
  data |>
    group_by(!!sym(col)) |>
    summarize(real_memory = mean(real_mib)) |>
    arrange(!!sym(col)) |>
    mutate(diff = real_memory - lag(real_memory))
}

res_fun(no_influence, "replace")
res_fun(no_influence, "mtry_ratio")

res_fun(param_grid, "nrow")
res_fun(param_grid, "nfeatures")
res_fun(param_grid, "num_trees")
res_fun(param_grid, "sample_fraction")


# descriptive plots (for model specification) ####

# target
ggplot(param_grid, aes(x = real_mib)) + geom_density()
ggplot(param_grid, aes(x = log(real_mib))) + geom_density()

# target vs. features
ggplot(param_grid, aes(y = real_mib, x = nrow)) + geom_point() + geom_smooth(method = "lm")
ggplot(param_grid, aes(y = real_mib, x = log(nrow, base = 10))) +
  geom_point(aes(color = factor(num_trees))) +
  geom_smooth(method = "lm") +
  scale_color_viridis_d(direction = -1)
ggplot(param_grid, aes(y = real_mib, x = log(nrow, base = 10))) +
  geom_point(aes(color = factor(sample_fraction))) +
  geom_smooth(method = "lm") +
  scale_color_viridis_d(direction = -1) +
  facet_wrap(~num_trees)

ggplot(param_grid, aes(y = real_mib, x = num_trees)) +
  geom_point(aes(color = nrow), alpha = 0.7) +
  geom_smooth(method = "lm") +
  scale_color_viridis_c(direction = -1)
ggplot(param_grid, aes(y = real_mib, x = sample_fraction)) +
  geom_point(aes(color = nrow), alpha = 0.7) +
  geom_smooth(method = "lm") +
  scale_color_viridis_c(direction = -1)
ggplot(param_grid, aes(y = real_mib, x = nfeatures)) +
  geom_point(aes(color = nrow), alpha = 0.7) +
  geom_smooth(method = "lm") +
  scale_color_viridis_c(direction = -1)


# fit models ####

# correlation matrix
corr_matrix = all_res |>
  select(nrow, nfeatures, num_trees, mtry_ratio, sample_fraction, replace, real_mib) |>
  GGally::ggpairs()
corr_matrix
ggsave(filename = "memory_experiments/Ranger/corr_matrix.png", plot = corr_matrix, width = 20, height = 10)

# forest memory is expected to be multiplicative (num_trees * nrow * sample_fraction),
# so also fit the gamma model with logged covariates (power law) and a lm with the
# theoretical interaction, and compare against the exp-linear form used for the torch learners
lm_interaction = lm(real_mib ~ nrow:nfeatures + nrow:num_trees:sample_fraction, data = param_grid)
glm_gamma = glm(
  rss_mib ~ nrow + nfeatures + num_trees + sample_fraction,
  data = param_grid,
  family = Gamma(link = "log")
)
glm_gamma_log = glm(
  rss_mib ~ log(nrow) + log(nfeatures) + log(num_trees) + log(sample_fraction),
  data = param_grid,
  family = Gamma(link = "log")
)

summary(lm_interaction)
summary(glm_gamma)
summary(glm_gamma_log)
AIC(lm_interaction, glm_gamma, glm_gamma_log)

# pick the model with the best AIC / prediction plot
model = glm_gamma_log

# plot predictions (on training data) as a sanity check
prds = predict(model, newdata = param_grid, type = "response")

df_plot = data.frame(
  real = param_grid$rss_mib,
  pred_raw = prds,
  pred_scaled = 1.3 * prds
) |>
  pivot_longer(
    cols = c(pred_raw, pred_scaled),
    names_to = "version",
    values_to = "pred"
  ) |>
  mutate(
    version = recode(version, pred_raw = "Model predictions", pred_scaled = "Predictions scaled by 130%")
  )

scale_prds = ggplot(data = df_plot, aes(x = real, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, color = "red") + # perfect prediction
  facet_wrap(~version) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "Perfect\nprediction",
    color = "red",
    hjust = 1.1,
    vjust = 2,
    size = 3
  ) +
  theme_bw() +
  labs(title = "Memory usage prediction for ranger", x = "real memory usage", y = "predicted memory usage")
scale_prds
ggsave(filename = "memory_experiments/Ranger/prds_vs_real.png", width = 9, height = 5)

# prediction function ####

# transcribe the coefficients with signif(coef(model), 3) and cross-check
# estimate_memory() against predict(model) before putting them into AutoRanger
signif(coef(model), 3)

estimate_memory = function(nrow, nfeatures, num_trees, sample_fraction) {
  coefs = coef(model)
  memory = exp(
    coefs[[1]] +
      coefs[[2]] * log(nrow) +
      coefs[[3]] * log(nfeatures) +
      coefs[[4]] * log(num_trees) +
      coefs[[5]] * log(sample_fraction)
  )
  ceiling(memory * 1.3) # scale by 30% to overestimate in most cases
}
