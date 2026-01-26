library(tidyverse)
library(broom)
library(readr)

no_influence = read_csv("memory_experiments/ResNet/memory_result_rn_no_influence.csv", col_types = cols())
blocks = read_csv("memory_experiments/ResNet/memory_result_rn_blocks.csv", col_types = cols())
data_size = read_csv("memory_experiments/ResNet/memory_result_rn_size.csv", col_types = cols())
token = read_csv("memory_experiments/ResNet/memory_result_rn_token.csv", col_types = cols())

param_grid = read.csv("memory_experiments/ResNet/memory_result_rn_grid.csv")

empty_session = 60.84  # mib, only packages
r_session = 500.11  # mib, load task


blocks$real_mib = blocks$rss_mib - r_session  # n_blocks
no_influence$real_mib = no_influence$rss_mib - r_session  # dropout1, dropout2, lr, weight_decay, epochs
data_size$real_mib = data_size$rss_mib - r_session  # nrow, nfeatures
token$real_mib = token$rss_mib - r_session  # d_block, d_hidden_multiplier

dfs = list(
  no_influence = no_influence,
  blocks = blocks,
  data_size = data_size,
  token = token
)

all_res = bind_rows(dfs, .id = "scenario")


# look at results ####

res_fun = function(data, col) {
  data %>%
    group_by(!!sym(col)) %>%
    summarize(real_memory = mean(real_mib)) %>% 
    arrange(!!sym(col)) %>% 
    mutate(diff = real_memory - lag(real_memory))
}

# no influence: dropout1
no_influence[ 1:6, ] %>% select(dropout1, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> no real influence (seems like random differences)

# no influence: dropout2
no_influence[ 6:12, ] %>% select(dropout2, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> no real influence (seems like random differences)

# no influence: lr
no_influence[ 13:22, ] %>% select(lr, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> no real influence (seems like random differences)

# no influence: weight_decay
no_influence[ 23:32, ] %>% select(weight_decay, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> no real influence (seems like random differences)

# no influence: epochs
no_influence[ 33:36, ] %>% select(epochs, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> the more epochs the less mib we need (but only by a little)


# blocks -> no influence
blocks %>% select(n_blocks, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))

# data_size -> positive influence
res_fun(data_size, "nrow")  #> positive influence
res_fun(data_size, "nfeatures")  #> not really an influence
data_size %>% select(nrow, nfeatures, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))

# token -> big influence
res_fun(token, "d_block")  #> big influence
res_fun(token, "d_hidden_multiplier")  #> influence
token %>% select(d_block, d_hidden_multiplier, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))

## see for parameter grid
print(res_fun(param_grid |> mutate(real_mib = rss_mib), "nrow"), n = 50)  # almost no influence
res_fun(param_grid |> mutate(real_mib = rss_mib), "nfeatures")  # almost no influence
res_fun(param_grid |> mutate(real_mib = rss_mib), "d_block")  # big influence
res_fun(param_grid |> mutate(real_mib = rss_mib), "d_hidden_multiplier")  # little influence

# descriptive plots (for model specification) ####

# correlation plot
corr_matrix = all_res %>% 
  select(nrow, nfeatures, n_blocks, d_block, d_hidden_multiplier, dropout1, dropout2, lr, weight_decay, epochs, rss_mib) %>%
  GGally::ggpairs()
corr_matrix
ggsave(filename = "memory_experiments/ResNet/corr_matrix.png", width = 20, height = 10)
# - correlation with mib: nrow, nfeatures, d_block, d_hidden_multiplier
# - high correlation between  d_block and d_hidden_multiplier

param_grid %>% 
  select(nrow, nfeatures, d_block, d_hidden_multiplier, rss_mib) %>%
  GGally::ggpairs()
# - all have correlation with mib
# - no correlation between the hyperparameters

# Fit gamma model (and log scale, grid is on log scale ...) ####

glm_gamma = glm(rss_mib ~ nrow + nfeatures + d_block + d_hidden_multiplier, data = param_grid, family = Gamma(link = "log"))
glm_gamma2 = glm(rss_mib ~ nrow + d_block + d_hidden_multiplier, data = param_grid, family = Gamma(link = "log"))

summary(glm_gamma)
summary(glm_gamma2)
# nfeatures has almost no influence and without it the AIC is slightly lower -> leave it out of the final model

plot(glm_gamma)
plot(glm_gamma2)

# plot predictions (on training data) as a sanity check
prds = predict(glm_gamma2, data = param_grid, type = "response")

df_plot = data.frame(
  real = param_grid$rss_mib,
  pred_raw = prds,
  pred_scaled = 1.2 * prds
) %>%
  pivot_longer(
    cols = c(pred_raw, pred_scaled),
    names_to = "version",
    values_to = "pred"
  ) %>%
  mutate(
    version = recode(version,
                     pred_raw = "Model predictions",
                     pred_scaled = "Predictions scaled by 120%")
  )

scale_prds = ggplot(data = df_plot, aes(x = real, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, color = "red") + # perfect prediction
  facet_wrap(~version) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "Perfect\nprediction",
    color = "red",
    hjust = 1.1, vjust = 2,
    size = 3
  ) +
  theme_bw() + 
  labs(title = "Memory usage prediction for ResNet", x = "real memory usage", y = "predicted memory usage")
scale_prds
ggsave(filename = "memory_experiments/ResNet/prds_vs_real.png", width = 9, height = 5)

# scaling by 1.2 overpredicts in most cases -> scale by 1.2 for safety


# prediction function ####
baseline = (glm_gamma2$coefficients[[1]])
b_nrow = (glm_gamma2$coefficients[[2]])
b_d_block = (glm_gamma2$coefficients[[3]])
b_d_hidden_multiplier = glm_gamma2$coefficients[[4]]


estimate_memory = function(nrow, d_block, d_hidden_multiplier ) {
  
  baseline = 6.56  # (glm_gamma2$coefficients[[1]])
  b_nrow = 8.35e-07  # (glm_gamma2$coefficients[[2]])
  b_d_block = 0.00046  # (glm_gamma2$coefficients[[3]])
  b_d_hidden_multiplier = 0.034  # glm_gamma2$coefficients[[4]])
  
  memory = exp(baseline +
                 b_nrow * nrow +
                 b_d_block * d_block +
                 b_d_hidden_multiplier * d_hidden_multiplier)
  # gamma model
  
  ceiling(memory * 1.2)  # scale by 20% to be save
} 


