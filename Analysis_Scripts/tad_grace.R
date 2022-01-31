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


# --- Setting Up Directories ---

parentDir <- getwd()
dataDir <- file.path(parentDir,'Data')
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





acc.model <- brm(g|se(g_se) ~ 1 + (1|Author/es.ids) + Duration.3,
                 data=global.exInfluence.studies,
                 prior=overall_effect.priors,
                 iter = 10000, chains = 4, warmup=2000,
                 save_pars = save_pars(all=T), seed = 123,
                 file=paste(modelDir,'overall_random',sep='/'),
                 file_refit = 'on_change')


