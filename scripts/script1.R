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

# posterior summary with flat prior
bernoulli_posterior_summary(n, m, alpha =1, beta = 1)
# posterior summary with 3, 5
bernoulli_posterior_summary(n, m, alpha =3, beta = 5)

get_beta_hpd(m + 1, n - m + 1) # HPD for the uniform prior
# quantile distribution
qbeta(0.025, m + 1, n - m + 1)
qbeta(0.975, m + 1, n - m + 1)

# sample 1 million samples from a N(100, 15^2)
set.seed(41)
x <- rnorm(n = 1e6, mean = 100, sd = 15)
mean(x)
sd(x)
pnorm(130, mean = 100, sd = 15)
mean(x <= 130)
