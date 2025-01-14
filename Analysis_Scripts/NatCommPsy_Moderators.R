"
-----------------------------------------------------------
PACMAn Meta-Analysis: Bayesian Hiearchical Modeling


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"
if(!require('esc')) install.packages('esc')
if(!require('forestplot')) install.packages('forestplot')
if(!require('tidybayes')) install.packages('tidybayes')
if(!require('cowplot')) install.packages('cowplot')
if(!require('glue')) install.packages('glue')
if(!require('ggridges')) install.packages('ggridges')
if(!require('logspline')) install.packages('logspline')
if(!require('see')) install.packages('see')
if(!require('bayestestR')) install.packages('bayestestR')

library(tidyverse)
library(dplyr)
library(esc)
library(brms)
library(forestplot)
library(gridExtra)
library(cowplot)
library(tidybayes)
library(glue)
library(ggridges)
library(reshape2)
library(bayestestR)


# --- Setting Up Directories ---

parentDir <- '/zwork/garrett/PACMAn/NatComm_Update/'
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')


setwd(dataDir)

# Load data
effectSizes.df <- readRDS('study_effects_trimmed_filled.rds') # readRDS('study_effectSizes_hand_NatCommPsy_searchUpdate.rds')


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
overall_model <- brm(g|se(g_se) ~ 1 + (1|Article.ID/es.ids),
                     data=effectSizes.df,
                     prior=overall_effect.priors,
                     iter = 12000, chains = 4, cores=4, warmup=2000,
                     save_pars = save_pars(all=T), seed = 123,
                     file=paste(modelDir,'overall_random',sep='/'),
                     file_refit = 'on_change')

# Convert numeric moderators where necessary

effectSizes.df$Mean.Age <- round(as.numeric(effectSizes.df$Mean.Age),2)

colnames(effectSizes.df)[which(names(effectSizes.df) == "Mean.BMI..kg.m.2.")] <- 'Mean.BMI'
colnames(effectSizes.df)[which(names(effectSizes.df) == "Mean.Height..cm.")] <- 'Mean.Height'
colnames(effectSizes.df)[which(names(effectSizes.df) == "Mean.Weight..kg.")] <- 'Mean.Weight'
colnames(effectSizes.df)[which(names(effectSizes.df) == "Mean.VO2.Max..mL.kg.min.")] <- 'Mean.VO2'


# Mean Center BMI, VO2, Height, Weight
effectSizes.df$Age.Centered <- effectSizes.df$Mean.Age - mean(effectSizes.df$Mean.Age, na.rm=T)
effectSizes.df$BMI.Centered <- effectSizes.df$Mean.BMI - mean(effectSizes.df$Mean.BMI, na.rm=T)
effectSizes.df$VO2.Centered <- effectSizes.df$Mean.VO2 - mean(effectSizes.df$Mean.VO2, na.rm=T)
effectSizes.df$Height.Centered <- effectSizes.df$Mean.Height - mean(effectSizes.df$Mean.Height, na.rm=T)
effectSizes.df$Weight.Centered <- effectSizes.df$Mean.Weight - mean(effectSizes.df$Mean.Weight, na.rm=T)


# --------- Subgroup/Moderator Analyses -----------

# Influence of Year
year.model <- update(overall_model, formula. = ~ . + Date,
                            newdata = effectSizes.df,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'moderator_year', sep='/'),
                            file_refit = 'on_change')


# Influence of between/within-subjects design
design.model <- update(overall_model, formula. = ~ . + Exercise.Design,
                            newdata = effectSizes.df,
                            prior = priors, iter = 12000, chains = 4, warmup=2000,
                            cores = 4,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'subgroup_design', sep='/'),
                            file_refit = 'on_change')

# Sample Age
age.model <- update(overall_model, formula. = ~ . + Age.Centered,
                       newdata = effectSizes.df,
                       prior = priors, iter = 12000, chains = 4, warmup=2000,
                       cores = 4,
                       save_pars=save_pars(all=T), seed=123, 
                       control=list(max_treedepth=15),
                       file=paste(modelDir,'moderator_age', sep='/'),
                       file_refit = 'on_change')

bmi.model <- update(overall_model, formula. = ~ . + BMI.Centered,
                    newdata = effectSizes.df,
                    prior = priors, iter = 12000, chains = 4, warmup=2000,
                    cores = 4,
                    save_pars=save_pars(all=T), seed=123, 
                    control=list(max_treedepth=15),
                    file=paste(modelDir,'moderator_bmi', sep='/'),
                    file_refit = 'on_change')

pcFem.model <- update(overall_model, formula. = ~ . + Percent.Female,
                    newdata = effectSizes.df,
                    prior = priors, iter = 12000, chains = 4, warmup=2000,
                    cores = 4,
                    save_pars=save_pars(all=T), seed=123, 
                    control=list(max_treedepth=15),
                    file=paste(modelDir,'moderator_pcFem', sep='/'),
                    file_refit = 'on_change')

VO2.model <- update(overall_model, formula. = ~ . + VO2.Centered,
                      newdata = effectSizes.df,
                      prior = priors, iter = 12000, chains = 4, warmup=2000,
                      cores = 4,
                      save_pars=save_pars(all=T), seed=123, 
                      control=list(max_treedepth=15),
                      file=paste(modelDir,'moderator_VO2', sep='/'),
                      file_refit = 'on_change')

height.model <- update(overall_model, formula. = ~ . + Height.Centered,
                    newdata = effectSizes.df,
                    prior = priors, iter = 12000, chains = 4, warmup=2000,
                    cores = 4,
                    save_pars=save_pars(all=T), seed=123, 
                    control=list(max_treedepth=15),
                    file=paste(modelDir,'moderator_height', sep='/'),
                    file_refit = 'on_change')

weight.model <- update(overall_model, formula. = ~ . + Weight.Centered,
                    newdata = effectSizes.df,
                    prior = priors, iter = 12000, chains = 4, warmup=2000,
                    cores = 4,
                    save_pars=save_pars(all=T), seed=123, 
                    control=list(max_treedepth=15),
                    file=paste(modelDir,'moderator_weight', sep='/'),
                    file_refit = 'on_change')

# subset data for testing difference between pre-post designs that had or did not have a control group
effectSizes.df$Pre_Post_Design <- apply(effectSizes.df, 1, 
     function(row){
         if(grepl('Pre, Post', row['Task.Execution.Time'], ignore.case=T)){
             if(grepl('Rest|Control|Relax|Baseline|Active|Passive', row['Compare.Level'], ignore.case=T)){
                 z <- 'With.Control'
             } else {
                 z <- 'Without.Control'
             }
         } else {
             z <- 'Not Applicable'
         }
     })

pre_post.df <- effectSizes.df %>% filter(Pre_Post_Design != 'Not Applicable')
pre_post.df$Pre_Post_Design <- factor(pre_post.df$Pre_Post_Design)
pre_post_model <- brm(g|se(g_se) ~ 1 + (1|Article.ID/es.ids) + Pre_Post_Design,
                     data=pre_post.df,
                     prior=priors,
                     iter = 12000, chains = 4, cores=4, warmup=2000,
                     save_pars = save_pars(all=T), seed = 123,
                     control=list(max_treedepth=15),
                     file=paste(modelDir,'pre_post_compare',sep='/'),
                     file_refit = 'on_change')

pre_post_null_model <- brm(g|se(g_se) ~ 1 + (1|Article.ID/es.ids),
                     data=pre_post.df,
                     prior=overall_effect.priors,
                     iter = 12000, chains = 4, cores=4, warmup=2000,
                     save_pars = save_pars(all=T), seed = 123,
                     file=paste(modelDir,'pre_post_compare_null',sep='/'),
                     file_refit = 'on_change')