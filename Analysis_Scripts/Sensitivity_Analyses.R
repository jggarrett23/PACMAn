"
-----------------------------------------------------------
PACMAn Meta-Analysis: Sensitivity Analyses


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

parentDir <- '/work/garrett/PACMAn'
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')

setwd(modelDir)

# --- Load Models ---

overall_model <- readRDS('overall_random.rds')
#exIntensity.model <- readRDS('subgroup_intensity.rds')
#cogDomain_model <- readRDS('subgroup_cogDomain.rds')
#exMode_model <- readRDS('subgroup_exMode.rds')
#testTime_model <- readRDS('subgroup_testTime.rds')
#outcome_model <- readRDS('subgroup_outcomeMeasure.rds')
#task_model <- readRDS('subgroup_task.rds')
#duration_model <- readRDS('subgroup_duration.rds')


# --- t-Distribution likelihood ---
t_priors <- c(prior(normal(0,1), class=Intercept),
              prior(cauchy(0,0.5), class=sd),
              prior(exponential(one_over_twentynine), class=nu))


overall_model.t <- update(overall_model, family=student,
                          prior=t_priors, iter = 10000, chains = 4, warmup=2000, cores=4,
                          stanvars = stanvar(1/29, name='one_over_twentynine'),
                          save_pars=save_pars(all=T), seed=123,
                          file=paste(modelDir,'overall_t', sep='/'),
                          file_refit = 'on_change')


# --- No effect exercise prior ---

ne_priors <- c(prior(normal(0,0.5), class=Intercept),
               prior(cauchy(0,0.5), class=sd))

overall_model.ne_prior <- update(overall_model, prior=ne_priors, 
                                 iter = 10000, chains = 4, warmup=2000, cores=4,
                                 save_pars=save_pars(all=T), seed=123,
                                 file=paste(modelDir,'overall_ne', sep='/'),
                                 file_refit = 'on_change')


# --- Positive effect prior ---
pe_priors <- c(prior(normal(0.24,0.57), class=Intercept),
                            prior(cauchy(0,0.5), class=sd))

overall_model.pe_prior <- update(overall_model, prior=pe_priors, 
                                 iter = 10000, chains = 4, warmup=2000, cores=4,
                                 save_pars=save_pars(all=T), seed=123,
                                 file=paste(modelDir,'overall_pe', sep='/'),
                                 file_refit = 'on_change')

# --- Extract Posteriors ---

overall_posts <- as_draws_df(overall_model, variable=c('^b','^sd'), regex=T)
overall_posts.t <- as_draws_df(overall_model.t, variable=c('^b','^sd'), regex=T)
overall_posts.ne <- as_draws_df(overall_model.ne_prior, variable=c('^b','^sd'), regex=T)
overall_posts.pe <- as_draws_df(overall_model.pe_prior, variable=c('^b','^sd'), regex=T)


names(overall_posts)[c(1:3)] <- names(overall_posts.t)[c(1:3)] <- 
  names(overall_posts.ne)[c(1:3)] <- names(overall_posts.pe)[c(1:3)]  <- c('g','tau1','tau2')


overall_posts$Model <- 1
overall_posts.t$Model <- 2
overall_posts.ne$Model <- 3
overall_posts.pe$Model <- 4

all_overall.posts <- rbind(overall_posts,overall_posts.t,overall_posts.ne, overall_posts.pe)

all_overall.posts <- all_overall.posts %>% 
  mutate(Model = factor(Model, labels=c('Initial','t-likelihood','no_effect', 'positive_effect')))


# ---- Plot Posteriors ----
compare_mu.plot <- ggplot(all_overall.posts, aes(x=g, fill=Model)) + 
  geom_density(alpha=0.3, size=.8, color='black') + 
  stat_pointinterval(point_interval = mode_hdi, .width = 0.89, 
                     size=5, point_size=2, shape=21, 
                     mapping=aes(fill=Model, interval_colour=Model, interval_alpha=0.3),
                     show.legend=F) +
  labs(x = expression(paste("Hedge's ", italic(g))),
       y = 'Density',
       title='Pooled Effect') +
  scale_fill_discrete(name='Prior/Likelihood:', labels=c('Weakly\nInformed','TL','NE','PE')) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black'),
    legend.text = element_text(size=6),
    legend.title = element_text(size=6.5),
    legend.position = 'none'
  )


ggsave('Sensitivity_compare_pooledEffect.jpg', plot=compare_mu.plot,
       path=plotDir, units='in', width = 2.9, height=1.98)

compare_tau1.plot <- ggplot(all_overall.posts, aes(x=tau1, fill=Model)) + 
  geom_density(alpha=0.3, size=.8, color='black') + 
  stat_pointinterval(point_interval = mode_hdi, .width = 0.89, 
                     size=5, point_size=2, shape=21, 
                     mapping=aes(fill=Model, interval_colour=Model, interval_alpha=0.3),
                     show.legend=F) +
  labs(x = expression({tau[Between]}),
       y = element_blank(),
       title='Between Study Heterogeneity') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black'),
    legend.position = 'none'
  )

ggsave('Sensitivity_compare_tau1.jpg', plot=compare_tau1.plot,
  path=plotDir, units='in', width = 2.9, height=1.98)

compare_tau2.plot <- ggplot(all_overall.posts, aes(x=tau2, fill=Model)) + 
  geom_density(alpha=0.3, size=1, color='black') + 
  stat_pointinterval(point_interval = mode_hdi, .width = 0.89, 
                     size=5, point_size=2, shape=21, 
                     mapping=aes(fill=Model, interval_colour=Model, interval_alpha=0.3),
                     show.legend=F) +
  labs(x = expression({tau[Within]}),
       y = element_blank(),
       title='Within Study Heterogeneity') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black'),
    legend.position = 'none'
  )

ggsave('Sensitivity_compare_tau2.jpg', plot=compare_tau2.plot,
       path=plotDir, units='in', width = 2.9, height=1.98)
