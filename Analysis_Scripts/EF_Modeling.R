"
-----------------------------------------------------------
PACMAn Meta-Analysis: Bayesian Hiearchical Modeling of Executive Function Effect Sizes


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"

if(!require('tidyverse')) install.packages('tidyverse')
if(!require('dplyr')) install.packages('dplyr')
if(!require('esc')) install.packages('esc')
if(!require('brms')) install.packages('brms')
if(!require('bayestestR')) install.packages('bayestestR')

library(tidyverse)
library(dplyr)
library(esc)
library(brms)
library(bayestestR)

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
    
executive_data$Ex.ACSM.2 <- as.factor(executive_data$Ex.ACSM.2)
executive_data$Ex.Mode.2 <- as.factor(executive_data$Ex.Mode.2)
executive_data$Duration.3 <- as.factor(executive_data$Duration.3)
executive_data$EffectTime <- as.factor(executive_data$EffectTime)
executive_data$Domain.3 <- as.factor(executive_data$Domain.3)

## ---- Setting up Priors ----
# Intercept represents mu, while sd represents tau

overall_effect.priors <- c(prior(normal(0,1), class=Intercept),
                           prior(cauchy(0,0.5), class=sd))


betaWeight_prior <- c(prior(normal(0,1), class='b'))

priors <- c(overall_effect.priors,betaWeight_prior)

hdi_width = .89

## ---- Overall Model ----

overall_model <- brm(g|se(g_se) ~ 1 + (1|Article.ID/es.ids),
                     data=executive_data,
                     prior=overall_effect.priors,
                     iter = 12000, chains = 4, cores=4, warmup=2000, 
                     control=list(max_treedepth=12),
                     save_pars = save_pars(all=T), seed = 123,
                     file=paste(modelDir,'ef_overall',sep='/'),
                     file_refit = 'on_change')

## ---- Subgroup Analyses ----

## ---- Exercise Intensity ----

# Influence of exercise Intensity

contrasts(executive_data$Ex.ACSM.2) <- contr.equalprior

exIntensity.model <- update(overall_model, formula. = ~ . + Ex.ACSM.2,
                            newdata = executive_data,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123,
                            cores = 4,
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'ef_subgroup_intensity', sep='/'),
                            file_refit = 'on_change')

## ---- Influence of Cognitive Domain ----
contrasts(executive_data$Domain.3) <- contr.equalprior

cogDomain_model <- update(overall_model, formula. = ~ . + Domain.3,
                          newdata=executive_data,
                          prior= priors,
                          iter = 12000, chains = 4, warmup=2000,
                          cores = 4,
                          save_pars = save_pars(all=T), seed = 123,
                          control=list(max_treedepth=12),
                          file=paste(modelDir,'ef_subgroup_cogDomain',sep='/'),
                          file_refit = 'on_change')

## ---- Influence of Exercise Mode ----
contrasts(executive_data$Ex.Mode.2) <- contr.equalprior

exMode_model <- update(overall_model, formula. = ~ . + Ex.Mode.2,
                       newdata=executive_data,
                       prior= c(overall_effect.priors, betaWeight_prior),
                       iter = 12000, chains = 4, warmup=2000,
                       cores = 4,
                       control=list(max_treedepth=12),
                       save_pars = save_pars(all=T), seed = 123,
                       file=paste(modelDir,'ef_subgroup_exMode',sep='/'),
                       file_refit = 'on_change')

## ---- Influence of Test Time on Effect ----
contrasts(executive_data$EffectTime) <- contr.equalprior

testTime_model <- update(overall_model, formula. = ~ . + EffectTime,
                         newdata=executive_data,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 12000, chains = 4, warmup=2000,
                         cores = 4,
                         save_pars = save_pars(all=T), seed = 123,
                         control = list(max_treedepth=15),
                         file=paste(modelDir,'ef_subgroup_testTime',sep='/'),
                         file_refit = 'on_change')

## ---- RT vs Accuracy ----
outcome_model <- update(overall_model, formula. = ~ . + OutcomeVariable,
                        newdata=executive_data,
                        prior= c(overall_effect.priors, betaWeight_prior),
                        iter = 12000, chains = 4, warmup=2000,
                        cores = 4,
                        save_pars = save_pars(all=T), seed = 123,
                        file=paste(modelDir,'ef_subgroup_outcomeMeasure',sep='/'),
                        file_refit = 'on_change')


## ---- Exercise Duration ----

contrasts(executive_data$Duration.3) <- contr.equalprior

duration_model <- update(overall_model, formula. = ~ . + Duration.3,
                         newdata=executive_data,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 12000, chains = 4, warmup=2000, cores = 4,
                         save_pars = save_pars(all=T), seed = 123,
                         control = list(max_treedepth=11),
                         file=paste(modelDir,'ef_subgroup_duration',sep='/'),
                         file_refit = 'on_change')



