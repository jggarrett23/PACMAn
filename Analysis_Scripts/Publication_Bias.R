library(tidyverse)
library(brms)
library(rstan)
library(MCMCvis)

rstan_options(auto_write=T)

parentDir <- '/work/garrett/PACMAn/NatComm_Update'
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir, 'Models')

setwd(parentDir)

effectSizes.df <- readRDS(file.path(dataDir,'study_effectSizes_hand_NatCommPsy_searchUpdate.rds'))

# Random effects model w/o publication bias
overall_model <- readRDS(file.path(modelDir, 'overall_random.rds'))

# Create standata from previous pooled-effects fit
data_ls <- standata(overall_model)

# Store information about p-value cutt-offs to test 
# Based on https://github.com/HaiyangJin/Bayesian-Meta-Bias-Stan
data_ls$alpha <- c(0.05, 0.1)
data_ls$N_alpha <- length(data_ls$alpha)
data_ls$side <- 1

# Use Rstan to fit hierarchical model accounting for publication bias
# Modified ma_bias_twosided.stan from HaiyanJin repository
overall_model_bias <- stan(
    file='ma_bias_twosided_Multilevel.stan',
    model_name = 'overall_model_bias',
    data=data_ls,
    iter=12000,
    warmup=2000,
    chains=4,
    cores=4,
    seed=23,
    verbose=F,
    open_progress=T,
    control=list(max_treedepth=15)
    )

# Random effects model w/ publication bias
saveRDS(overall_model_bias, file.path(modelDir, 'overall_model_bias.rds'), compress=T)