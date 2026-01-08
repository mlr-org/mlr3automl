library(tidyverse)
library(broom)

no_influence = read.csv("memory_experiments/FTTransformer/memory_result_ftt_no_influence.csv")
blocks = read.csv("memory_experiments/FTTransformer/memory_result_ftt_blocks.csv")
data_size = read.csv("memory_experiments/FTTransformer/memory_result_ftt_size.csv")
token = read.csv("memory_experiments/FTTransformer/memory_result_ftt_token.csv")

param_grid = read.csv("memory_experiments/FTTransformer/memory_result_ftt_grid.csv")

empty_session = 61.12  # nothing
r_session = 487.44  # load task and packages


blocks$real_mib = blocks$rss_mib - r_session  # n_blocks
no_influence$real_mib = no_influence$rss_mib - r_session  # residual_dropout, attention_dropout, ffn_dropout, lr, weight_decay, epochs
data_size$real_mib = data_size$rss_mib - r_session  # nrow, nfeatures
token$real_mib = token$rss_mib - r_session  # d_token, ffn_d_hidden_multiplier

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


cols_to_diff = list(
  no_influence = "n_blocks",
  blocks = c("residual_dropout", "attention_dropout", "ffn_dropout", "lr", "weight_decay", "epochs"),
  data_size = c("nrow", "nfeatures"),
  token = c("d_token", "ffn_d_hidden_multiplier")
)

# no influence: reasidual dropout
no_influence[ 1:3, ] %>% select(residual_dropout, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> has a little effect, but not a lot

# no influence: attention_dropout
no_influence[ 4:9, ] %>% select(attention_dropout, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> smallest at 0.3, but not really a positive influence

# no influence: ffn_dropout
no_influence[ 10:15, ] %>% select(ffn_dropout, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> smallest at 0.3, but not really a positive influence

# no influence: lr
no_influence[ 16:20, ] %>% select(lr, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> smallest at 1e-4, but not really a positive influence

# no influence: weight_decay
no_influence[ 21:29, ] %>% select(weight_decay, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> no real influence (seems like random differences)

# no influence: epochs
no_influence[ 30:33, ] %>% select(epochs, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))
#> no real influence (seems like random differences)

# blocks -> no influence
blocks %>% select(n_blocks, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))

# data_size -> positive influence
res_fun(data_size, "nrow")
res_fun(data_size, "nfeatures")  # small influence
data_size %>% select(nrow, nfeatures, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))

# token -> big influence
res_fun(token, "d_token")
res_fun(token, "ffn_d_hidden_multiplier")
token %>% select(d_token, ffn_d_hidden_multiplier, rss_mib, real_mib) %>% mutate(diff = real_mib - lag(real_mib))


## see for parameter grid
print(res_fun(param_grid |> mutate(real_mib = rss_mib), "nrow"), n = 50)
res_fun(param_grid |> mutate(real_mib = rss_mib), "nfeatures")
res_fun(param_grid |> mutate(real_mib = rss_mib), "neurons")

# descriptive plots (for model specification) ####

# correlation plot
all_res %>% 
  select(nrow, nfeatures, n_blocks, d_token, ffn_d_hidden_multiplier, residual_dropout, attention_dropout, ffn_dropout, lr, weight_decay, epochs, rss_mib) %>%
  GGally::ggpairs()
# - correlation with mib: nrow, nfeatures, d_token, ffn_d_hidden_multiplier
# - high correlation between d_token and ffn_d_hidden_multiplier
# - some correlation between nrow and nfeatures

param_grid %>% 
  select(nrow, nfeatures, d_token, ffn_d_hidden_multiplier, rss_mib) %>%
  GGally::ggpairs()
# - all have correlation with mib
# - almost no correlation between the hyperparameters

# target
ggplot(param_grid, aes(x = rss_mib)) + geom_density()
ggplot(param_grid, aes(x = log(rss_mib))) + geom_density()
# no too skewed

# target vs. features
## nrow -> log 10  transformed
ggplot(param_grid, aes(y = rss_mib, x = nrow)) + geom_point() + geom_smooth(method = "lm")
ggplot(param_grid, aes(y = rss_mib, x = log(nrow, base = 10))) + geom_point() + geom_smooth(method = "lm")
ggplot(param_grid, aes(y = rss_mib, x = log(nrow, base = 10))) + geom_point(aes(color = factor(nfeatures))) + geom_smooth(method = "lm") + scale_color_viridis_d(direction = -1) 
ggplot(param_grid, aes(y = rss_mib, x = log(nrow, base = 10))) + geom_point(aes(color = factor(d_token))) + geom_smooth(method = "lm") + scale_color_viridis_d(direction = -1) 


# Fit gamma model (and log scale, grid is on log scale ...) ####

glm_gamma = glm(rss_mib ~ nrow + nfeatures + d_token + ffn_d_hidden_multiplier, data = param_grid, family = Gamma(link = "log"))

# plot predictions (on training data) as a sanity check
prds = predict(glm_gamma, data = param_grid, type = "response")

ggplot(data = data.frame(x = param_grid$rss_mib, y = prds), aes(x, y)) +
  geom_point() + geom_abline(slope = 1, color = "red")
ggplot(data = data.frame(x = param_grid$rss_mib, y = 1.3* prds), aes(x, y)) +
  geom_point() + geom_abline(slope = 1, color = "red")
# scaling by 1.3 overpredicts in most cases -> scale by 1.3 for safety


# prediction function ####
baseline = (glm_gamma$coefficients[[1]])
b_nrow = (glm_gamma$coefficients[[2]])
b_nfeatures = (glm_gamma$coefficients[[3]])
b_d_token = (glm_gamma$coefficients[[4]])
b_ffn_d_hidden_multiplier = glm_gamma$coefficients[[5]]


estimate_memory = function(nrow, nfeatures, d_token, ffn_d_hidden_multiplier ) {
  
  baseline = 6.54  # (glm_gamma$coefficients[[1]])
  b_nrow = 1.41e-06  # (glm_gamma$coefficients[[2]])
  b_nfeatures =  0.0004  # (glm_gamma$coefficients[[3]])
  b_d_token = 0.0013  # (glm_gamma$coefficients[[4]])
  b_ffn_d_hidden_multiplier = 0.11  # glm_gamma$coefficients[[5]])
  
  memory = exp(baseline +
    b_nrow * nrow +
    b_nfeatures * nfeatures +
    b_d_token * d_token +
    b_ffn_d_hidden_multiplier * ffn_d_hidden_multiplier)
  # gamma model
  
  ceiling(memory * 1.3)  # scale by 30% to be save
} 


