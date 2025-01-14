"
-----------------------------------------------------------
PACMAn Meta-Analysis: Bayesian Hiearchical Modeling

EF Interaction Models

Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"

if(!require('esc')) install.packages('esc')
if(!require('bayestestR')) install.packages('bayestestR')
if(!require('tidybayes')) install.packages('tidybayes')

library(tidyverse)
library(dplyr)
library(esc)
library(brms)
library(bayestestR)
library(tidybayes)

# --- Setting Up Directories ---

parentDir <- '/work/garrett/PACMAn/NatComm_Update'
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir,'Models/EF')
plotDir <- file.path(parentDir,'Figures')

setwd(dataDir)

# Load data
effectSizes.df <- readRDS('study_effects_trimmed_filled.rds') #readRDS('study_effectSizes_hand_NatCommPsy_searchUpdate.rds')


executive_data <- effectSizes.df %>%
                    filter(Domain.2 == 'Executive Function')


executive_data$Domain.3 <- sapply(executive_data$Cognitive.Task, function(x){
    if (grepl('Anti-Response|Symbol|Task-Switching Paradigm|Task Switching|Trail Making|AX-CPT|Angram|Dimension Switching', x, ignore.case=T)){
        z <- 'Cognitive Control'
    } else if (grepl('Anti-saccade|Antisaccade|Anti|Flanker|Eriksen|No-Go|Simon|Complex Visual Discrimination|Stroop|Stop Signal',x, ignore.case=T)){
        z <- 'Inhibition'
    } else if (grepl('Tower', x, ignore.case=T)){
        z <- 'Planning'
    } else if (grepl('Decision|Logical|Naval|Soccer|Speed|discrimination', x, ignore.case=T)){
        z <- 'Decision Making'
    } else if (grepl('Memory|-back|Change Detection|Ruff|Code|Trigram|Delayed|Span|COWAT|addition|Brown-Peterson|Sternberg|Sterberg|Plus-Minus|Subtraction|PASAT|DRM|Math|Letter Number|Self-Ordered|', 
              x, ignore.case=T)){
        z <- 'Working Memory'
    } 
      else {
        z <- x
    }
    })
## ---- Setting up Priors ----
# Intercept represents mu, while sd represents tau

overall_effect.priors <- c(prior(normal(0,1), class=Intercept),
                           prior(cauchy(0,0.5), class=sd))


betaWeight_prior <- c(prior(normal(0,1), class='b'))

priors <- c(overall_effect.priors,betaWeight_prior)

hdi_width = .89

overall_model <- brm(g|se(g_se) ~ 1 + (1|Article.ID/es.ids),
                     data=executive_data,
                     prior=overall_effect.priors,
                     iter = 12000, chains = 4, cores=4, warmup=2000, 
                     control=list(max_treedepth=12),
                     save_pars = save_pars(all=T), seed = 123,
                     file=paste(modelDir,'ef_overall',sep='/'),
                     file_refit = 'on_change')

executive_data$Ex.ACSM.2 <- as.factor(executive_data$Ex.ACSM.2)
executive_data$Ex.Mode.2 <- as.factor(executive_data$Ex.Mode.2)
executive_data$Duration.3 <- as.factor(executive_data$Duration.3)
executive_data$Domain.3 <- as.factor(executive_data$Domain.3)


contrasts(executive_data$Ex.ACSM.2) <- contr.equalprior
contrasts(executive_data$Ex.Mode.2) <- contr.equalprior
contrasts(executive_data$Duration.3) <- contr.equalprior
contrasts(executive_data$Domain.3) <- contr.equalprior

# ---- Additive Models ----
# Intensity + Type
intensity_type_model <- update(overall_model, formula. = ~ . + Ex.ACSM.2 + Ex.Mode.2,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_intensity_exMode_model', sep='/'),
                            file_refit = 'on_change')

# Intensity + Duration

intensity_duration_model <- update(overall_model, formula. = ~ . + Ex.ACSM.2 + Duration.3,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_intensity_duration_model', sep='/'),
                            file_refit = 'on_change')

# Type + Duration

type_duration_model <- update(overall_model, formula. = ~ . + Ex.Mode.2 + Duration.3,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_type_duration_model', sep='/'),
                            file_refit = 'on_change')

# Domain + Type

cogDomain_type_model <- update(overall_model, formula. = ~ . + Domain.3 + Ex.Mode.2,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4, 
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_domain_type_model', sep='/'),
                            file_refit = 'on_change')

#Domain + Intensity

cogDomain_intensity_model <- update(overall_model, formula. = ~ . + Domain.3 + Ex.ACSM.2,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123, 
                            cores = 4,
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_domain_intensity_model', sep='/'),
                            file_refit = 'on_change')

# Domain + Outcome

cogDomain_outcome_model <- update(overall_model, formula. = ~ . + Domain.3 + OutcomeVariable,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123, 
                            cores = 4, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_domain_outcome_model', sep='/'),
                            file_refit = 'on_change')

# Type + Outcome

type_outcome_model <- update(overall_model, formula. = ~ . + Ex.Mode.2 + OutcomeVariable,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123, 
                            cores = 4,
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_type_outcome_model', sep='/'),
                            file_refit = 'on_change')

# ---- Interactions ----


# Intensity x Type
intensity_X_type_model <- update(overall_model, formula. = ~ . + Ex.ACSM.2*Ex.Mode.2,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_intensity_X_exMode_model', sep='/'),
                            file_refit = 'on_change')

# Intensity x Duration

intensity_X_duration_model <- update(overall_model, formula. = ~ . + Ex.ACSM.2*Duration.3,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_intensity_X_duration_model', sep='/'),
                            file_refit = 'on_change')

# Type x Duration

type_X_duration_model <- update(overall_model, formula. = ~ . + Ex.Mode.2*Duration.3,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_type_X_duration_model', sep='/'),
                            file_refit = 'on_change')

# Domain x Type

cogDomain_X_type_model <- update(overall_model, formula. = ~ . + Domain.3 * Ex.Mode.2,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4, 
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_domain_X_type_model', sep='/'),
                            file_refit = 'on_change')

# Domain x Intensity

cogDomain_X_intensity_model <- update(overall_model, formula. = ~ . + Domain.3*Ex.ACSM.2,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123, 
                            cores = 4,
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_domain_X_intensity_model', sep='/'),
                            file_refit = 'on_change')

# Domain x Outcome

cogDomain_X_outcome_model <- update(overall_model, formula. = ~ . + Domain.3*OutcomeVariable,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123, 
                            cores = 4, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_domain_X_outcome_model', sep='/'),
                            file_refit = 'on_change')

# Type x Outcome

type_X_outcome_model <- update(overall_model, formula. = ~ . + Ex.Mode.2*OutcomeVariable,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123, 
                            cores = 4,
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_type_X_outcome_model', sep='/'),
                            file_refit = 'on_change')