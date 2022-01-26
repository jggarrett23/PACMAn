"
-----------------------------------------------------------
PACMAn Meta-Analysis: Model Comparisons and Hypothesis Testing


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"
if(!require('logspline')) install.packages('logspline')
if(!require('see')) install.packages('see')
if(!require('bayestestR')) install.packages('bayestestR')
if(!require('emmeans')) install.packages('emmeans')

library(tidyverse)
library(brms)
library(gt)
library(bayestestR)
library(reshape2)
library(emmeans)
library(tidybayes)


# --- Setting Up Directories ---

parentDir <- getwd()
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')

setwd(modelDir)

# --- Load Models ---

overall_model <- readRDS('overall_random.rds')
exIntensity.model <- readRDS('subgroup_intensity.rds')
cogDomain_model <- readRDS('subgroup_cogDomain.rds')
exMode_model <- readRDS('subgroup_exMode.rds')
testTime_model <- readRDS('subgroup_testTime.rds')
outcome_model <- readRDS('subgroup_outcomeMeasure.rds')
task_model <- readRDS('subgroup_task.rds')
duration_model <- readRDS('subgroup_duration.rds')

# --- Compute LOOIC Scores ---

models.list <- list('Overall'= overall_model, 'Exercise Intensity'=exIntensity.model, 
                             'Cognitive Domain'=cogDomain_model, 'Exercise Type' = exMode_model, 
                             'Task Time' = testTime_model, 'Outcome Measure' = outcome_model,
                             'Exercise Duration' = duration_model, "Task Type" = task_model)


# ELPD = expected log predictive density, where higher values are better

# leave one out cross validation 
# WARNING: Computationally demanding. Save loo scores to prevent having to 
# run this line of code multiple times. Especially if have to set
# moment_match = T (i.e. pareto k > 0.7)
models.loo <- lapply(models.list, loo)

models.looic <- lapply(models.loo, 
                       function(x){
                         x$estimates[3]
                       })

models.looic <- melt(as.data.frame(models.looic))

names(models.looic) <- c('Model', 'LOOIC')

models_looic.gt_tbl <- gt(models.looic,rowname_col = 'Model') %>% 
  fmt_number(columns=2, decimals=2) %>% 
  tab_header(
    title = md('**Subgroup Analysis Model Comparison**'),
  ) %>% 
  cols_label(Model='Model', LOOIC = 'LOOIC')


gtsave(models_looic.gt_tbl, filename = 'Models_LOOIC.png', path=plotDir)

# ---- Hypothesis Testing ----

# use bayes factors to determine the amount of evidence for model parameters to be different from 0
# use WAIC to do model comparison, then test model parameters using bayes factors

# To get stable bayes factors, best to use 10000 iterations


# First use emmeans to ensure that the correct estimate is computed since subgroup models
# fit with an orthonormal contrast
overall.margins <- emmeans(overall_model,"1")
inten.margins <- emmeans(exIntensity.model, ~ Ex.ACSM.2)
domain.margins <- emmeans(cogDomain_model, ~ Domain.2)
type.margins <- emmeans(exMode_model, ~ Ex.Mode.2)
task.margins <- emmeans(task_model, ~ Task.2)
outcome.margins <- emmeans(outcome_model, ~ DV.2)
duration.margins <- emmeans(duration_model, ~ Duration.2)
time.margins <- emmeans(testTime_model, ~ EffectTime.2)


model.marginals <- list('Overall'= overall.margins, 'Exercise Intensity'=inten.margins, 
                        'Cognitive Domain'=domain.margins, 'Exercise Type' = type.margins, 
                        'Task Time' = time.margins, 'Outcome Measure' = outcome.margins,
                        'Exercise Duration' = duration.margins, "Task Type" = task.margins)


# Bayes factors approximated using the Savage-Dickey Ratio
model.bfs <- lapply(1:length(model.marginals), 
                     function(x){
                       bayesfactor_parameters(model.marginals[[x]], prior=models.list[[x]])
                     })

names(model.bfs) <- names(model.marginals)


# ---- Pairwise Comparisons ---

# have to define own mode function
arith.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

duration_posts <- as_draws_df(duration_model, variable=c('^b'), regex=T)

durations <- levels(duration_model$data$Duration.2)

duration_posts[2:length(durations)] <- apply(duration_posts[2:length(durations)], 2, 
                                             function(x){
                                               x + duration_posts$b_Intercept
                                             })

names(duration_posts)[1:length(durations)] <- durations

durations_modeHDI <- mode_hdi(duration_posts, .width=.89)

duration_pairs <- pairs(emmeans(duration_model, ~ Duration.2))

bayesfactor_parameters(duration_pairs, prior=duration_model)
