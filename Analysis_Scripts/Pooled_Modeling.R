"
-----------------------------------------------------------
PACMAn Meta-Analysis: Pooled Effect Size Modeling


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"
if(!require('esc')) install.packages('esc')
if(!require('meta')) install.packages('meta')
if(!require('forestplot')) install.packages('forestplot')

library(tidyverse)
library(dplyr)
library(esc)
library(meta)
library(forestplot)


# --- Helper Functions ---

# Based on Ludyga et al., 2020
comp_se <- function(stand_errors,r){
  variances <- stand_errors^2 # Bornstein et al., 2009?
  
  end_sumTerms <- c()
  for (iVariance in 1:length(variances)){
    end_sumTerms[iVariance] <- variances[iVariance]+sum(r*sqrt(variances[iVariance])*sqrt(variances[-iVariance]))
  }
  
  composite_variance <- (1/length(variances))^2 + sum(end_sumTerms)
  
  composite_se <- sqrt(composite_variance)
  
  return(composite_se)
}


split_forest <- function(model){
  
  data <- tibble('mean'= model$TE,
                 'lower'= model$lower,
                 'upper'= model$upper,
                 'study'= model$data$Author,
                 'g'=as.character(round(model$TE,2)),
                 '95% CI'= paste('[',
                                 as.character(round(model$lower,2)),
                                 ';',as.character(round(model$upper),2),
                                 ']',sep=''))
  
  data <- data[order(-data$mean),]
  
  data.1 <- data[data$mean > median(data$mean),]
  data.2 <- data[data$mean <= median(data$mean),]
  
  summary <- tibble('mean'=model$TE.random,
                    'lower'=model$lower.random,
                    'upper'=model$upper.random,
                    'study'='Summary',
                    'g' = as.character(round(model$TE.random,2)),
                    'summary'=T)
  
  header <- tibble('study'=c('','Study'),
                   'g'=c('',"Hedge's g"),
                   '95% CI' = c('','95% CI'),
                   'summary'=T)
  
  output_df_1 <- bind_rows(header,
                           tibble(mean=NA_real_),
                           data.1,
                           tibble(mean=NA_real_))
  
  output_df_2 <- bind_rows(tibble(mean=NA_real_,summary=T),
                           tibble(mean=NA_real_),
                           data.2,
                           tibble(mean=NA_real_),
                           summary)
  
  return(list(output_df_1,output_df_2))
}



parentDir <- 'D:/PACMAn'
dataDir <- file.path(parentDir,'Data')

setwd(dataDir)

# Load data
allStudies_effects.df <- readRDS('allStudies_effects.rds')


# Convert effects to hedges g
allStudies_effects.df$g <- hedges_g(allStudies_effects.df$SMD,allStudies_effects.df$Effect_N)
allStudies_effects.df$g_se <- sqrt((1-(3/(4*allStudies_effects.df$Effect_N-1))^2)*allStudies_effects.df$SMD_SE^2)



pre_post.effects <- allStudies_effects.df %>% 
  filter(grepl('pre|baseline',TaskTime) & grepl('post',TaskTime) & grepl('post', EffectTime))


during_wRest.effects <- allStudies_effects.df %>% 
  filter(grepl('during', EffectTime) & grepl('rest|baseline', Compare.Condition, ignore.case = T))


post_wRest.effects <- allStudies_effects.df %>% 
  filter(grepl('post', TaskTime) & 
           !grepl('pre|baseline', TaskTime) & 
           grepl('post', EffectTime) & 
           grepl('rest', Compare.Condition, ignore.case = T))


global.exInfluence.studies <- rbind(pre_post.effects,during_wRest.effects,post_wRest.effects)

# remove unnecessary effects
global.exInfluence.studies <- global.exInfluence.studies %>% 
  filter(!grepl('control|rest',Ex.Condition, ignore.case = T))



# Create Composite Score for Studies with multiple outcomes
global.exInfluence.sum_effects <- global.exInfluence.studies %>%
  group_by(ID,Author,Year,expNum) %>% 
  summarise(composite_effect = mean(g),
            composite_error = comp_se(SMD_SE,.6))

summarized_effects.outcome <- allStudies_effects.df %>%
  group_by(ID,Author,Year,DV,ExpNum) %>% 
  summarise(composite_effect = mean(g),
            composite_error = comp_se(SMD_SE,.6))


# ---- Modeling ----

# First try the frequentist approach

freq.pooled_model <- metagen(TE=composite_effect,
                             seTE=composite_error,
                             data=global.exInfluence.sum_effects,
                             sm='SMD',
                             comb.fixed=FALSE,
                             comb.random=TRUE,
                             method.tau="REML",
                             hakn=TRUE,
                             title='Global Influence of Exercise: Frequentist')



# forest plot
freq.pooled_data <- tibble('mean'=freq.pooled_model$TE,
                           'lower'=freq.pooled_model$lower,
                           'upper'=freq.pooled_model$upper,
                           'study'=freq.pooled_model$data$Author,
                           'g'=as.character(round(freq.pooled_model$TE,2)),
                           '95% CI'= paste('[',
                                           as.character(round(freq.pooled_model$lower,2)),
                                           ';',as.character(round(freq.pooled_model$upper),2),
                                           ']',sep=''))

freq.pooled_data <- freq.pooled_data[order(-freq.pooled_data$mean),]

freq.pooled_data.1 <- freq.pooled_data[freq.pooled_data$mean > median(freq.pooled_data$mean),]
freq.pooled_data.2 <- freq.pooled_data[freq.pooled_data$mean <= median(freq.pooled_data$mean),]

summary <- tibble('mean'=freq.pooled_model$TE.random,
                          'lower'=freq.pooled_model$lower.random,
                          'upper'=freq.pooled_model$upper.random,
                          'study'='Summary',
                          'g' = as.character(round(freq.pooled_model$TE.random,2)),
                          'summary'=T)

header <- tibble('study'=c('','Study'),
                 'g'=c('',"Hedge's g"),
                 '95% CI' = c('','95% CI'),
                 'summary'=T)

output_df_1 <- bind_rows(header,
                       tibble(mean=NA_real_),
                       freq.pooled_data.1,
                       tibble(mean=NA_real_))

output_df_2 <- bind_rows(tibble(mean=NA_real_,summary=T),
                         tibble(mean=NA_real_),
                         freq.pooled_data.2,
                         tibble(mean=NA_real_),
                         summary)

overall.plot.1 <- output_df_1 %>% 
  forestplot(labeltext = c('study','g','95% CI'), 
             is.summary = summary,
             xlab='Estimated Effect',
             hrzl_lines = list("3"=gpar(lty=2)),
             col = fpColors(box = "royalblue",
                            line = "grey",
                            summary = "green"),
             vertices=T)

overall.plot.2 <- output_df_2 %>% 
  forestplot(labeltext = c('study','g','95% CI'), 
             is.summary = summary,
             xlab='Estimated Effect',
             hrzl_lines = list("46"=gpar(lwd=1, columns=1:3, col='black')),
             col = fpColors(box = "royalblue",
                            line = "grey",
                            summary = "green"),
             vertices=T)


# Run Separate Models for RT and Accuracy
freq.pooled_model.RT <- metagen(TE=composite_effect,
                                seTE=composite_error,
                                data=summarized_effects.outcome[which(grepl('RT',summarized_effects.outcome$DV)==T),],
                                sm='SMD',
                                comb.fixed=FALSE,
                                comb.random=TRUE,
                                method.tau="REML",
                                hakn=TRUE,
                                title='Pre-Post Effect of Exercise on RT: Frequentist')

freq.pooled_model.Acc <- metagen(TE=composite_effect,
                                 seTE=composite_error,
                                 data=summarized_effects.outcome[which(grepl('RT',summarized_effects.outcome$DV)==F),],
                                 sm='SMD',
                                 comb.fixed=FALSE,
                                 comb.random=TRUE,
                                 method.tau="REML",
                                 hakn=TRUE,
                                 title='Pre-Post Effect of Exercise on RT: Frequentist')


rt_plot.dfs <- split_forest(freq.pooled_model.RT)

overall.plot.RT.1 <- rt_plot.dfs[[1]] %>% 
  forestplot(labeltext = c('study','g','95% CI'), 
             is.summary = summary,
             clip = c(-3,3),
             xlab='Estimated Effect',
             hrzl_lines = list("3"=gpar(lty=2)),
             col = fpColors(box = "royalblue",
                            line = "grey",
                            summary = "green"),
             vertices=T)


overall.plot.RT.2 <- rt_plot.dfs[[2]] %>% 
  forestplot(labeltext = c('study','g','95% CI'), 
             is.summary = summary,
             clip = c(-3,3),
             xlab='Estimated Effect',
             hrzl_lines = list("3"=gpar(lty=2)),
             col = fpColors(box = "royalblue",
                            line = "grey",
                            summary = "green"),
             vertices=T)


acc_plot.dfs <- split_forest(freq.pooled_model.Acc)

overall.plot.ACC.1 <- acc_plot.dfs[[1]] %>% 
  forestplot(labeltext = c('study','g','95% CI'), 
             is.summary = summary,
             clip = c(-3,3),
             xlab='Estimated Effect',
             hrzl_lines = list("3"=gpar(lty=2)),
             col = fpColors(box = "royalblue",
                            line = "grey",
                            summary = "green"),
             vertices=T)


overall.plot.ACC.2 <- acc_plot.dfs[[2]] %>% 
  forestplot(labeltext = c('study','g','95% CI'), 
             is.summary = summary,
             clip = c(-3,3),
             xlab='Estimated Effect',
             hrzl_lines = list("34"=gpar(lwd=1, columns=1:3, col='black')),
             col = fpColors(box = "royalblue",
                            line = "grey",
                            summary = "green"),
             vertices=T)



