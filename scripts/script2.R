library(tidyvese)
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
