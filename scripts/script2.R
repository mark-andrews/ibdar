library(tidyverse)
library(brms)

source("https://raw.githubusercontent.com/mark-andrews/ibdar/refs/heads/ibdar25/data/dl_data.R")
source("https://raw.githubusercontent.com/mark-andrews/ibdar/refs/heads/ibdar25/data/sim_data.R")

data_df1
M_1 <- lm(y ~ x_1 + x_2, data = data_df1)
summary(M_1)
sigma(M_1)
confint(M_1)

# Bayesian version of this
M_2 <- brm(y ~ x_1 + x_2, data = data_df1)
summary(M_2) # M_2

plot(M_2)
mcmc_plot(M_2)
mcmc_plot(M_2, type = "intervals")
mcmc_plot(M_2, type = "hist")
mcmc_plot(M_2, type = "hist", bins = 50)
mcmc_plot(M_2, type = "hist", variable = "sigma")
mcmc_plot(M_2, type = "hist", variable = c("b_x_1", "b_x_2"))
mcmc_plot(M_2, type = "hist", variable = "b_x_[1-9]+", regex = T)
mcmc_plot(M_2, type = "areas")
mcmc_plot(M_2, type = "areas_ridges")
mcmc_plot(M_2, type = "dens")
mcmc_plot(M_2, type = "combo")
mcmc_plot(M_2, type = "dens_chains")
mcmc_plot(M_2, type = "dens_overlay")

as_draws_df(M_2) # samples

stancode(M_2)

prior_summary(M_2)

# change some default settings
M_3 <- brm(y ~ x_1 + x_2,
  chains = 4,
  cores = 4,
  iter = 10000,
  warmup = 2000,
  save_pars = save_pars(all = TRUE),
  data = data_df1
)

# Weight from height etc -----------------------------------------------------

M_4 <- lm(weight ~ height + gender, data = weight_df)
summary(M_4)
M_5 <- brm(weight ~ height + gender,
  save_pars = save_pars(all = TRUE),
  data = weight_df
)
M_5
prior_summary(M_5)
get_prior(weight ~ height, data = weight_df)

M_6 <- brm(weight ~ height + gender,
  prior = set_prior("normal(0, 10)"),
  save_pars = save_pars(all = TRUE),
  data = weight_df
)
prior_summary(M_6)

# compare posterior distributions over the non-intercept coefs
# for M_6 and M_5
mcmc_plot(M_5, type = "hist", variable = c("b_gendermale", "b_height"))
mcmc_plot(M_6, type = "hist", variable = c("b_gendermale", "b_height"))
fixef(M_5)
fixef(M_6)

new_priors <- c(
  set_prior("normal(0, 20)", class = "b", coef = "gendermale"),
  set_prior("normal(0, 20)", class = "b", coef = "height"),
  set_prior("normal(50, 25)", class = "Intercept"),
  set_prior("student_t(1, 0, 50)", class = "sigma")
)

M_7 <- brm(weight ~ height + gender,
  prior = new_priors,
  save_pars = save_pars(all = TRUE),
  data = weight_df
)
fixef(M_7)


# sample from a t distribution in R
x <- rt(1e6, df = 3) |> abs()
# df=1 => cauchy
rt(1e3, df = 1) |>
  abs() |>
  quantile() |>
  round(1)


M_8 <- lm(weight ~ height + gender + race, data = weight_df)
M_9 <- lm(weight ~ height, data = weight_df)
anova(M_9, M_8)
AIC(M_9, M_8)


M_10 <- brm(weight ~ height + gender + race,
  save_pars = save_pars(all = TRUE),
  data = weight_df
)
M_11 <- brm(weight ~ height,
  save_pars = save_pars(all = TRUE),
  data = weight_df
)

loo(M_10, M_11)
waic(M_10, M_11)
bayes_factor(M_10, M_11, log = TRUE)
bayes_factor(M_10, M_11)


# Beyond normal linear models ---------------------------------------------

M_12 <- brm(
  bf(
    weight ~ height + gender + race,
    sigma ~ height + gender + race
  ),
  family = student(),
  save_pars = save_pars(all = TRUE),
  data = weight_df
)

M_12
waic(M_12, M_10)
pp_check(M_10)
pp_check(M_12)

M_13 <- glm(I(cigs > 0) ~ educ + age,
  data = smoking_df,
  family = binomial()
)

M_14 <- brm(I(cigs > 0) ~ educ + age,
  data = smoking_df,
  cores = 4,
  family = bernoulli()
)
prior_summary(M_14)

library(lme4)
M_15 <- lmer(mathach ~ ses + (ses | school), data = mathach_df)
M_16 <- brm(mathach ~ ses + (ses | school),
  cores = 4,
  data = mathach_df
)
M_16
prior_summary(M_16)
