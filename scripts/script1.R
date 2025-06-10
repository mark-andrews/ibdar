#source("https://raw.githubusercontent.com/mark-andrews/ibdar/refs/heads/ibdar25/priorexposure/R/bernoulli_models.R")
library(priorexposure)
library(tidyverse)

m <- 139
n <- 250
bernoulli_likelihood(n, m)

m <- 14
n <- 25
bernoulli_likelihood(n, m)

# plot different beta priors
beta_plot(alpha = 3, beta = 5)
beta_plot(alpha = 5, beta = 3)
beta_plot(alpha = 9, beta = 15)
beta_plot(alpha = 1, beta = 1)

# alpha = 3, and beta = 5
# the posterior is this ....
n <- 250
m <- 139
alpha <- 3
beta <- 5
beta_plot(m + alpha, n - m + beta)
bernoulli_posterior_plot(n, m, alpha = alpha, beta = beta)
bernoulli_posterior_plot(n, m, alpha, beta)
bernoulli_posterior_plot(n, m, beta = beta, alpha =  alpha)
# change the prior to a uniform prior
bernoulli_posterior_plot(n, m, alpha = 1, beta = 1)

bernoulli_posterior_plot(n, m, alpha = 1, beta = 1) + xlim(0.4, 0.75)
bernoulli_posterior_plot(n, m, alpha = 3, beta = 5) + xlim(0.4, 0.75)




