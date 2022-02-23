"
-----------------------------------------------------------
PACMAn Meta-Analysis: Calcute Estimates For Tad & Grace


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-------------
"

library(tidyverse)
library(dplyr)
library(brms)
library(tidybayes)
library(reshape2)
library(bayestestR)
library(emmeans)


# --- Setting Up Directories ---

parentDir <- getwd()
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir,'Models')
setwd(dataDir)

# Load data
global.exInfluence.studies <- readRDS('global_exInfluence_effects.rds')

# Parse data into time bins that Tad is interested in 
"
< 2 mins
2-10 mins
11-30 mins
31-60 mins
> 60 mins
"
global.exInfluence.studies$Duration.3 <- sapply(tolower(global.exInfluence.studies$Duration),
                                                function(x){
                                                  if (grepl('exhaustion', x)){
                                                    t <- 'volitional exhaustion'
                                                  } else if (grepl('completion|game',x)){
                                                    t <- 'task completion'
                                                  } else if (grepl('total 7|3 x 1', x)){
                                                    t <- '2-10'
                                                  } else if (grepl('> 55', x)) {
                                                    t <- '31-60'
                                                  } else if (grepl('sets', x)){
                                                    t <- 'sets duration'
                                                  } else if (grepl('n/a|need to', x)){
                                                    t <- NA
                                                  } else {
                                                    num <- as.numeric(str_extract(x, '[0-9]{1,3}'))
                                                    
                                                    if (!any(is.na(num), is.null(num))){
                                                      
                                                      if (num < 2){
                                                        t <- '<2'
                                                      } else if (num >= 2 & num <= 10){
                                                        t <- '2-10'
                                                      } else if (num >= 11 & num <= 30){
                                                        t <- '11-30'
                                                      } else if (num >= 31 & num <= 60){
                                                        t <- '31-60'
                                                      } else if (num > 60){
                                                        t <- '>60'
                                                      }
                                                      
                                                    } else {
                                                      x
                                                    }
                                                  }
                                                })

global.exInfluence.studies$Duration.3 <- factor(global.exInfluence.studies$Duration.3)

contrasts(global.exInfluence.studies$Duration.3) <- contr.orthonorm

# Convert correlations to fishers z?
global.exInfluence.studies <- global.exInfluence.studies %>% 
  mutate(fisher_z = 0.5 * log((1+r)/(1-r)),
         fisher_z_se = sqrt(1/(Effect_N - 3)))


# Separate RT and ACC data
rt_data <- global.exInfluence.studies %>% 
  filter(DV.2 == 'RT')

acc_data <- global.exInfluence.studies %>% 
  filter(DV.2 == 'Accuracy')


# ---- Modeling ----

priors <- c(prior(normal(0,1), class=Intercept),
                           prior(normal(0,1), class=b),
                           prior(cauchy(0,0.5), class=sd))

hdi_width = .89


acc.model <- brm(fisher_z|se(fisher_z_se) ~ 1 + (1|Author/es.ids) + Duration.3,
                 data=acc_data,
                 prior= priors,
                 iter = 5000, chains = 4, warmup=1000,
                 save_pars = save_pars(all=T), seed = 123,
                 file=paste(modelDir,'tad_grace_acc',sep='/'),
                 file_refit = 'on_change')

rt.model <- brm(fisher_z|se(fisher_z_se) ~ 1 + (1|Author/es.ids) + Duration.3,
                 data=rt_data,
                 prior= priors,
                 iter = 5000, chains = 4, warmup=1000,
                 save_pars = save_pars(all=T), seed = 123,
                 file=paste(modelDir,'tad_grace_rt',sep='/'),
                 file_refit = 'on_change')

# compute marginals

durations <- levels(global.exInfluence.studies$Duration.3)

acc.marginals <- emmeans(acc.model, ~Duration.3)

rt.marginals <- emmeans(rt.model, ~Duration.3)

acc.posteriors <- as.data.frame(acc.marginals@post.beta)

colnames(acc.posteriors) <- unlist(acc.marginals@levels)

rt.posteriors <- as.data.frame(rt.marginals@post.beta)

colnames(rt.posteriors) <- unlist(rt.marginals@levels)

extract_corr.estimates <- function(post_dist, outcome_type){
  
  # transform back to pearsons r
  posteriors_r <- apply(post_dist, 2, 
                            function(x){
                              r <- (exp(2*x) - 1)/(exp(2*x) + 1)
                            })
  
  posteriors_r <- as.data.frame(posteriors_r)
  # compute mode and HDIs
  
  posteriors_mode <- mode_hdi(posteriors_r, .width=hdi_width)
  
  if (outcome_type == 'RT'){
    modes <- posteriors_mode[seq(1,12,by=3)]
    lw <- posteriors_mode[seq(2,12,by=3)]
    up <- posteriors_mode[seq(3,12,by=3)]
    
    posteriors_mode.mat <- as.matrix(rbind(as.numeric(modes),
                                           apply(post_dist,2,sd)[1:4],
                                           as.numeric(lw),
                                           as.numeric(up)),
                                     nrow=3, ncol=5)
    
  } else if (outcome_type == 'Acc'){
    modes <- posteriors_mode[seq(1,15,by=3)]
    lw <- posteriors_mode[seq(2,15,by=3)]
    up <- posteriors_mode[seq(3,15,by=3)]
    
    posteriors_mode.mat <- as.matrix(rbind(as.numeric(modes),
                                           apply(post_dist,2,sd)[1:5],
                                           as.numeric(lw),
                                           as.numeric(up)),
                                     nrow=3, ncol=5)
  }
  
  
  
  posteriors_mode.mat <- t(round(posteriors_mode.mat,2))
  
  colnames(posteriors_mode.mat) <- c('mode','sd','lw','upper')
  
  
  return(posteriors_mode.mat)
}

acc.estimates <- extract_corr.estimates(acc.posteriors,'Acc')
rt.estimates <- extract_corr.estimates(rt.posteriors,'RT')


# Break down of tasks for each measure

acc.task_counts <- acc_data %>% 
  group_by(Task.2) %>% 
  count()

rt.task_counts <- rt_data %>% 
  group_by(Task.2) %>% 
  count()





