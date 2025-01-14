"
-----------------------------------------------------------
PACMAn Meta-Analysis: Bayesian Hiearchical Modeling


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

library(metafor)
library(tidyverse)
library(dplyr)
library(esc)
library(brms)
library(bayestestR)

# --- Setting Up Directories ---

parentDir <- '/work/garrett/PACMAn/NatComm_Update'
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')

setwd(dataDir)

# Load data
#effectSizes.df <- readRDS('study_effectSizes_hand_NatCommPsy_searchUpdate.rds') #readRDS('study_effectSizes_hand.rds')
effectSizes.df <- readRDS('study_effects_trimmed_filled.rds')

## ---- Setting up Priors ----
# Intercept represents mu, while sd represents tau

overall_effect.priors <- c(prior(normal(0,1), class=Intercept),
                           prior(cauchy(0,0.5), class=sd))


betaWeight_prior <- c(prior(normal(0,1), class='b'))

priors <- c(overall_effect.priors,betaWeight_prior)

hdi_width = .89

"
Break down of the random effects model formula in brms:
First the model is intercept only. Since we are using a 
random effects model, we include a random intercept term for
each study, indicating that effect sizes are nested within
studies. By using the '/', we are specifying that the 2nd level effect ID
is nested within the 3rd level of studies. 
Lastly, we cannot simple use the effect size of each
study as our outcome variable. Instead, we have to give studies
with higher precision greater weight. We can accomplish this 
by using y|se(se_y) as the outcome. 

Note: There is no need to calculate a summary effect for studies
that have contributed more than one effect size (e.g., multiple outcomes).
By using a hiearchical model, such dependent measures can be accounted for
through clustering. For instance, the 1st level of the model can be each
individual effect. The 2nd level could indicate each effect is clustered
within studies. Lastly, the 3rd level represents the pooling of effect sizes
across studies. More advantageous since we do not have to assume a correlation
between effect sizes. 

"
print('Overall\n')
overall_model <- brm(g|se(g_se) ~ 1 + (1|Article.ID/es.ids),
                     data=effectSizes.df,
                     prior=overall_effect.priors,
                     iter = 12000, chains = 4, cores=4, warmup=2000,
                     control=list(max_treedepth=12),
                     save_pars = save_pars(all=T), seed = 123,
                     file=paste(modelDir,'overall_random',sep='/'),
                     file_refit = 'on_change')


# Group-Level effects: sd(Intercept) represents between-study heterogeneity, or tau
# Population-Level effects: Intercept represents the overall pooled effect of the analysis

summary(overall_model)

## ---- Subgroup Analyses ----

## ---- Exercise Intensity ----

# Influence of exercise Intensity
print('Exercise Intensity\n')
contrasts(effectSizes.df$Ex.ACSM.2) <- contr.equalprior

exIntensity.model <- update(overall_model, formula. = ~ . + Ex.ACSM.2,
                            newdata = effectSizes.df,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'subgroup_intensity', sep='/'),
                            file_refit = 'on_change')

## ---- Influence of Cognitive Domain ----
print('Cognitive Domain\n')
contrasts(effectSizes.df$Domain.2) <- contr.equalprior

cogDomain_model <- update(overall_model, formula. = ~ . + Domain.2,
                          newdata=effectSizes.df,
                          prior= priors,
                          iter = 12000, chains = 4, warmup=2000,
                          save_pars = save_pars(all=T), seed = 123,
                          cores = 4,
                          control=list(max_treedepth=12),
                          file=paste(modelDir,'subgroup_cogDomain',sep='/'),
                          file_refit = 'on_change')

## ---- Influence of Exercise Mode ----
print('Exercise Type\n')
contrasts(effectSizes.df$Ex.Mode.2) <- contr.equalprior

exMode_model <- update(overall_model, formula. = ~ . + Ex.Mode.2,
                       newdata=effectSizes.df,
                       prior= c(overall_effect.priors, betaWeight_prior),
                       iter = 12000, chains = 4, warmup=2000,
                       cores = 4,
                       control=list(max_treedepth=12),
                       save_pars = save_pars(all=T), seed = 123,
                       file=paste(modelDir,'subgroup_exMode',sep='/'),
                       file_refit = 'on_change')

## ---- Influence of Test Time on Effect ----
print('Test Time\n')
contrasts(effectSizes.df$EffectTime) <- contr.equalprior

testTime_model <- update(overall_model, formula. = ~ . + EffectTime,
                         newdata=effectSizes.df,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 12000, chains = 4, warmup=2000,
                         save_pars = save_pars(all=T), seed = 123,
                         cores = 4,
                         control = list(max_treedepth=15),
                         file=paste(modelDir,'subgroup_testTime',sep='/'),
                         file_refit = 'on_change')

## ---- RT vs Accuracy ----
print('Outcome Measure\n')
outcome_model <- update(overall_model, formula. = ~ . + OutcomeVariable,
                        newdata=effectSizes.df,
                        prior= c(overall_effect.priors, betaWeight_prior),
                        iter = 12000, chains = 4, warmup=2000,
                        save_pars = save_pars(all=T), seed = 123,
                        file=paste(modelDir,'subgroup_outcomeMeasure',sep='/'),
                        file_refit = 'on_change')


## ---- Exercise Duration ----
print('Exercise Duration\n')
contrasts(effectSizes.df$Duration.3) <- contr.equalprior

duration_model <- update(overall_model, formula. = ~ . + Duration.3,
                         newdata=effectSizes.df,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 12000, chains = 4, warmup=2000, cores = 4,
                         save_pars = save_pars(all=T), seed = 123,
                         control = list(max_treedepth=15),
                         file=paste(modelDir,'subgroup_duration',sep='/'),
                         file_refit = 'on_change')