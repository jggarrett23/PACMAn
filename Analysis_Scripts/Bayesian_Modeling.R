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
library(gt)
library(bayestestR)

# --- Setting Up Directories ---

parentDir <- getwd()
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')


setwd(dataDir)

# Load data
allStudies_effects.df <- readRDS('allStudies_effects.rds')


# Convert effects to hedges g
allStudies_effects.df$g <- hedges_g(allStudies_effects.df$SMD,allStudies_effects.df$Effect_N)
allStudies_effects.df$g_se <- sqrt((1-(3/(4*allStudies_effects.df$Effect_N-1))^2)*allStudies_effects.df$SMD_SE^2)

allStudies_effects.df$es.ids <- factor(1:nrow(allStudies_effects.df))


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
  filter(!grepl('control|rest|relax',Ex.Condition, ignore.case = T))


## ---- Preprocessing ----

# Make Each Study a factor
global.exInfluence.studies$Author <- factor(global.exInfluence.studies$Author)


# Exercise Intensities
# first, use study intensity categorization if ACSM categorization not available
global.exInfluence.studies$Ex.ACSM.2 <- apply(global.exInfluence.studies,1,
                                              function(x){
                                                if(is.na(x['Ex.ACSM'])){
                                                  x['Ex.ACSM'] <- x['Ex.Condition']
                                                }
                                                else {
                                                  x['Ex.ACSM']
                                                }
                                              })

# reduce number of intensity categorizations
all_Intensities <- unique(global.exInfluence.studies$Ex.ACSM.2)

global.exInfluence.studies$Ex.ACSM.2 <- sapply(global.exInfluence.studies$Ex.ACSM.2,
                                               function(x){
                                                 if (grepl('moderate ', x, ignore.case = T)){
                                                   x <- 'Moderate'
                                                 } else if (grepl('low', x, ignore.case = T)){
                                                   x <- 'Light'
                                                 } else if (grepl('high', x,ignore.case = T)){
                                                   x <- 'Vigorous'
                                                 } else if (grepl('Low/Moderate', x,ignore.case = T)){
                                                   x <- 'Light-Moderate'
                                                 } else if (grepl('Moderate-High', x,ignore.case = T)){
                                                   x <- 'Moderate-Vigorous'
                                                 }
                                                 else {
                                                   x
                                                 }
                                                 
                                               })


global.exInfluence.studies$Ex.ACSM.2 <- factor(global.exInfluence.studies$Ex.ACSM.2, 
                                               levels = c('Very Light', 'Light', 'Light-Moderate',
                                                          'Moderate','Moderate-Vigorous', 'Vigorous', 'Maximal'))
# Cognitive Domains
global.exInfluence.studies$Domain.2 <- sapply(global.exInfluence.studies$Domain, 
                                              function(x){
                                                if (grepl('perc',x, ignore.case = T)){
                                                  x <- 'Perception'
                                                } else if (grepl('decision', x, ignore.case = T)){
                                                  x <- 'Decision_Making'
                                                } else if (grepl('working memory|WM', x, ignore.case = T)){
                                                  x <- 'Working Memory'
                                                } else if (grepl('Information', x, ignore.case = T)){
                                                  x <- 'Information Processing'
                                                } else if (grepl('Ex', x, ignore.case = T)){
                                                  x <- 'Executive Function'
                                                } else if (grepl('Ep|Long Term|Learning &', x, ignore.case = T)){
                                                  x <- 'Memory'
                                                } else {
                                                  x
                                                }
                                              })


global.exInfluence.studies$Domain.2 <- factor(global.exInfluence.studies$Domain.2)


# Exercise Mode
global.exInfluence.studies$Ex.Mode.2 <- sapply(global.exInfluence.studies$Ex.Mode,
                                               function(x){
                                                 if (grepl('cycling ', x, ignore.case = T)){
                                                   x <- 'Cycling'
                                                 } else if (grepl('(treadmill)|walking|Treadmill Walking|Stair',x,ignore.case = F)){
                                                   x <- 'Walking'
                                                 } else if (grepl('jumping|yoga|climb|voll', x, ignore.case = T)){
                                                   x <- 'Sport Activity'
                                                 } else if (grepl('HITT',x,ignore.case = T)){
                                                   x <- 'HITT'
                                                 } else {
                                                   x
                                                 }
                                               })

global.exInfluence.studies$Ex.Mode.2 <- factor(global.exInfluence.studies$Ex.Mode.2)


# Effect Time
# use 15, 30, and 180 minutes post as groupings
global.exInfluence.studies$EffectTime.2 <- sapply(global.exInfluence.studies$EffectTime, 
                                                  function(x){
                                                    if (grepl('post', x)){
                                                      time_point <- as.numeric(str_extract(x, '[0-9]{2}'))
                                                      
                                                      # if digit found
                                                      if (!is.na(time_point)){
                                                        if (time_point <= 5){
                                                          x <- 'post'
                                                        } else if (5 < time_point & time_point <= 15){
                                                          x <- 'post (15 min)'
                                                        } else if (15 < time_point & time_point != 18) {
                                                          x <- 'post (30-75 min)'
                                                        } else {
                                                          x <- 'post (180 min)'
                                                        }
                                                      } else {
                                                        x <- 'post'
                                                      }
                                                      
                                                    } else if (grepl('during', x)){
                                                      x <- 'during'
                                                    } else {
                                                      x
                                                    }
                                                  })

global.exInfluence.studies$EffectTime.2 <- factor(global.exInfluence.studies$EffectTime.2,
                                                  levels=c('during','post', 'post (15 min)',
                                                           'post (30-75 min)', 'post (180 min)'))
# Dependent Measures
global.exInfluence.studies$DV.2 <- sapply(global.exInfluence.studies$DV, 
                                          function(x){
                                            if (grepl('RT|Interference|Error', x, ignore.case = T)){
                                              x <- 'RT'
                                            } else {
                                              x <- 'Accuracy'
                                            }
                                          })

global.exInfluence.studies$DV.2 <- factor(global.exInfluence.studies$DV.2)


# Task 
global.exInfluence.studies$Task.2 <- sapply(tolower(global.exInfluence.studies$Task), 
                                          function(x){
                                            if (grepl('stroop', x)){
                                              x <- 'stroop'
                                            } else if (grepl('sternberg|sterberg', x)) {
                                              x <- 'sternberg'
                                            } else if (grepl('flanker|flaker', x)){
                                              x <- 'flanker'
                                            } else if (grepl('vigilance|vigiliance', x)){
                                              x <- 'vigilance'
                                            } else if (grepl('back', x)){
                                              x <- 'n-back'
                                            } else if (grepl('choice', x)){
                                              x <-  'choice rt'
                                            } else if (grepl('task switching', x)){
                                              x <- 'task switching'
                                            } else if (grepl('rey', x)){
                                              x <- 'ravlt'
                                            } else if (grepl('continuous visual', x)){
                                              x <- 'cavt'
                                            } else if (grepl('monitoring', x)){
                                              x <-  'monitoring'
                                            } else if (grepl('cpt', x)){
                                              x <- 'cpt'
                                            } else if (grepl('multitasking', x)){
                                              x <- 'multitasking'
                                            } else if (grepl('switch', x)){
                                              x <- 'task switching'
                                            } else if (grepl('trail', x)){
                                              x <- 'trail making'
                                            } else if (grepl('global', x)){
                                              x <- 'global vs local detection'
                                            } else if (grepl('go/no', x)){
                                              x <- 'go/no-go'
                                            } else if (grepl('spatial', x)){
                                              x <- 'spatial WM'
                                            } else if (grepl('assocation', x)){
                                              x <- 'word association'
                                            } else {
                                              x 
                                            }
                                          })

global.exInfluence.studies$Task.2 <- factor(global.exInfluence.studies$Task.2)

# Duration
global.exInfluence.studies$Duration.2 <- sapply(tolower(global.exInfluence.studies$Duration), 
                                          function(x){
                                            if (grepl('exhaustion', x)){
                                              t <- 'volitional exhaustion'
                                            } else if (grepl('completion|game',x)){
                                              t <- 'task completion'
                                            } else if (grepl('total 7|3 x 1', x)){
                                              t <- '<=15'
                                            } else if (grepl('> 55', x)) {
                                              t <- '40-45'
                                            } else if (grepl('sets', x)){
                                              t <- 'sets duration'
                                            } else if (grepl('n/a|need to', x)){
                                              t <- NA
                                            } else {
                                              num <- as.numeric(str_extract(x, '[0-9]{1,3}'))
                                              
                                              if (!any(is.na(num), is.null(num))){
                                                
                                                if (num <= 15){
                                                  t <- '<=15'
                                                } else if (num >= 20 & num < 30){
                                                  t <- '20-27'
                                                } else if (num >= 30 & num < 40){
                                                  t <- '30-35'
                                                } else if (num >= 40 & num <= 45){
                                                  t <- '40-45'
                                                } else if (num >= 60){
                                                  t <- '>60'
                                                }
                                                
                                              } else {
                                                x
                                              }
                                            }
                                          })

global.exInfluence.studies$Duration.2 <- factor(global.exInfluence.studies$Duration.2, 
                                                levels = c('<=15', '20-27', '30-35', '40-45', '>60',
                                                           'task completion', 'volitional exhaustion', 'sets duration'))

save(global.exInfluence.studies, file=paste(dataDir, 'global_exInfluence_effects.rds', sep='/'))


## ---- Plot Effects ----
g.effects_plot <- ggplot(data=global.exInfluence.studies, aes(x=g, y=factor(ID), colour=DV.2)) +
  geom_point(shape=1, stroke=1) + 
  labs(x='Study ID',
       y=expression("Hedge's "*italic(g)),
       title='Effect Sizes',
       colour='Outcome\nMeasure') + 
  scale_colour_manual(values = c('green','black')) + 
  scale_alpha_manual(values=c(0.6, 0.4)) + 
  theme_classic() + 
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.text.x = element_text(size= 7, angle=90, vjust=0.5, hjust=1),
    axis.title = element_text(size=12,color='black')
  )

ggsave('Effects_Scatter.jpg', plot = g.effects_plot, path=plotDir,
       units='in', width = 3, height = 8)


## ---- Setting up Priors ----
# Intercept represents mu, while sd represents tau

overall_effect.priors <- c(prior(normal(0,1), class=Intercept),
                          prior(cauchy(0,0.5), class=sd))


betaWeight_prior <- c(prior(normal(0,1), class='b'))

priors <- c(overall_effect.priors,betaWeight_prior)

hdi_width = .89

## ---- Overall Model ----

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
overall_model <- brm(g|se(g_se) ~ 1 + (1|Author/es.ids),
                     data=global.exInfluence.studies,
                     prior=overall_effect.priors,
                     iter = 10000, chains = 4, warmup=2000,
                     save_pars = save_pars(all=T), seed = 123,
                     file=paste(modelDir,'overall_random',sep='/'),
                     file_refit = 'on_change')


# Group-Level effects: sd(Intercept) represents between-study heterogeneity, or tau
# Population-Level effects: Intercept represents the overall pooled effect of the analysis

summary(overall_model)


## ---- Posteriors ----

overall_model.post_samps <- as_draws_df(overall_model, variable=c('^b','^sd'), regex = T)

names(overall_model.post_samps)[c(1:3)] <- c('g','tau1','tau2')


overall_model.post_hdi <- mode_hdi(overall_model.post_samps, .width=hdi_width)

# Plot posterior distribution

overall.mu_plot <- ggplot(data = overall_model.post_samps, aes(x = g)) +
  geom_density(fill = "lightblue",                # set the color
               color = "black", alpha = 0.5, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=1, point_size=2) +
  labs(x = expression(mu),
       y = 'Density',
       title='Pooled Effect') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black')
  ) + 
  annotate(geom='text', x=mean(overall_model.post_samps$g), y=.8,
           label=paste(hdi_width*100,'% HDI',sep=''), size=2.5) +
  
  annotate(geom='text', x=overall_model.post_hdi$g,
           y=0.35, label=sprintf('%.2f',overall_model.post_hdi$g), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$g.lower,
           y=0.35, label=sprintf('%.2f',overall_model.post_hdi$g.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$g.upper,
         y=0.35, label=sprintf('%.2f',overall_model.post_hdi$g.upper), size=2.2)


# between study heterogeneity
overall.tau1_plot <- ggplot(aes(x = tau1), data = overall_model.post_samps) +
  geom_density(fill = "darkgreen",               # set the color
               color = "black", alpha = 0.25, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = .89, 
                     size=1, point_size=2) +        
  labs(x = expression({tau[Between]}),
       y = element_blank(),
       title='Between Study Heterogeneity') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black')
  ) + 
  annotate(geom='text', x=mean(overall_model.post_samps$tau1), y=1.35,
           label=paste(hdi_width*100,'% HDI',sep=''), size=2.5) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1,
           y=.5, label=sprintf('%.2f',overall_model.post_hdi$tau1), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1.lower,
           y=.5, label=sprintf('%.2f',overall_model.post_hdi$tau1.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1.upper,
           y=.5, label=sprintf('%.2f',overall_model.post_hdi$tau1.upper), size=2.2)


# within study heterogenity
overall.tau2_plot <- ggplot(aes(x = tau2), data = overall_model.post_samps) +
  geom_density(fill = "lightgreen",               # set the color
               color = "black", alpha = 0.7, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = .89, 
                     size=1, point_size=2) +        
  labs(x = expression({tau[Within]}),
       y = element_blank(),
       title='Within Study Heterogeneity') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black')
  ) + 
  annotate(geom='text', x=mean(overall_model.post_samps$tau2), y=1.5,
           label=paste(hdi_width*100,'% HDI',sep=''), size=2.5) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2,
           y=.6, label=sprintf('%.2f',overall_model.post_hdi$tau2), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2.lower,
           y=.6, label=sprintf('%.2f',overall_model.post_hdi$tau2.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2.upper,
           y=.6, label=sprintf('%.2f',overall_model.post_hdi$tau2.upper), size=2.2)


overall_plot <- grid.arrange(overall.mu_plot,overall.tau1_plot, overall.tau2_plot, ncol=3,
             top=textGrob('Overall Effect of Exercise on Cognition',
                          gp=gpar(fontsize=12,fontface=2)))


ggsave('Overall_Model_Posteriors.jpg', plot=overall_plot, path=plotDir,
       units='in', width=5, height=4)

## ---- Quantify Heterogeneity ----
# Use Higgins and Thompson (2002) to estimate within study variance. Then use estimate to compute I^2

estimate_v.q <- function(effect_variances){
  nume <- (length(effect_variances) - 1) * sum(1/effect_variances)
  denom <- (sum(1/effect_variances)^2) - sum(1/effect_variances^2)
  
  return(nume/denom)
}

v.q <- estimate_v.q(global.exInfluence.studies$g_se)

# Compute posteriors of I^2 for each level
I.2.lvl_2 <- overall_model.post_samps$tau2^2/(overall_model.post_samps$tau1^2 + 
                                              overall_model.post_samps$tau2^2 + v.q)

I.2.lvl_3 <- overall_model.post_samps$tau1^2/(overall_model.post_samps$tau1^2 + 
                                                overall_model.post_samps$tau2^2 + v.q)

# Degree of heterogeneity within studies
mode_hdi(I.2.lvl_2*100,.width=hdi_width)

# degree of heterogeneity between studies
mode_hdi(I.2.lvl_3*100,.width=hdi_width)

## ---- Publication Bias ----
# Funnel Plot

se.seq <- seq(0,max(global.exInfluence.studies$g_se),length.out=nrow(global.exInfluence.studies))

# psuedo confidence interval
lwr <- overall_model.post_hdi$g - (1.96*se.seq)
uppr <- overall_model.post_hdi$g + (1.96*se.seq)

funnel <- ggplot(global.exInfluence.studies, aes(x=g, y=g_se)) + 
  geom_point(shape=1) + 
  geom_segment(aes(x=overall_model.post_hdi$g, y=0, xend=overall_model.post_hdi$g, yend=max(g_se)),
               color='blue') + 
  geom_line(aes(x=lwr, y=se.seq), linetype='dashed') + 
  geom_line(aes(x=uppr, y=se.seq), linetype='dashed') + 
  xlim(c(-13,13)) + 
  labs(x = expression("Hedge's "*italic(g)),
         y = 'Standard Error') +
  scale_y_reverse() + 
  theme_bw() +
  theme(panel.grid = element_blank())

# Egger's Regression Test
eggs.test <- global.exInfluence.studies %>% 
  mutate(y = g/g_se, x=1/g_se) %>% 
  brm(y ~ 0 + Intercept + x, data=., prior=c(prior(normal(0,1), class=b)),
      iter=5000, warmup=1000, save_pars=save_pars(all=T))
  
eggs.test_hdi <- as_draws_df(eggs.test, variable='b_Intercept') %>% 
  mode_hdi(.width=hdi_width)

## ---- Posterior Predictive Check ----

overall_model.ppc_plot <- pp_check(overall_model, ndraws=100) + 
  labs(x = expression("Hedge's "*italic(g)),
       y = 'Density',
       title='Overall Model: Posterior Predictive Check') +
  xlim(c(-4,4)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size=13,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=11,color='black')
  )  


ggsave('Overall_Effect_PPC.jpg', plot=overall_model.ppc_plot,
       path=plotDir, units='in', width=6, height=3)

## ---- Empirical cumulative density function (i.e., probability the effect size is <= a value) ----

overall_effect.ecdf_plot <- ggplot(overall_model.post_samps, aes(x=g)) + 
  stat_ecdf(size = 1.5) + 
  geom_vline(xintercept=mean(overall_model.post_samps$g), 
             color='red', size=1.2, alpha=0.5, linetype='dashed') +
  labs(y='Cumulative Probability',
       x=expression(mu),
       title='ECDF: Posterior Distribution of the Pooled Effect Size') + 
  theme_light() +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line = element_line(colour="darkgrey",size=.4),
    panel.border = element_blank(),
    axis.text = element_text(size=10,color='black'),
    axis.title = element_text(size=13,color='black')
  )
  

ggsave('Overall_Model_Posteriors_ECDF.jpg', plot=overall_effect.ecdf_plot, path=plotDir,
       units = 'in', width=8, height=3)


## ---- Forest Plot ----

# calculate actual effect of each study by adding pooled effect size to estimate deviation

study.draws <- spread_draws(overall_model, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = b_Intercept + r_Author)

overall_pooled_effect.draws <- spread_draws(overall_model, b_Intercept) %>% 
  mutate(Author='Pooled Effect')


overall.forest_data <- bind_rows(study.draws, overall_pooled_effect.draws) %>% 
  ungroup() %>% 
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept)) 

overall.forest_data$group <- factor(ifelse(overall.forest_data$Author=='Pooled Effect',1,0))


overall.forest_data.summary <- group_by(overall.forest_data, Author) %>% 
  mean_hdi(b_Intercept, .width=hdi_width)  
  
  
first_half.studies <- overall.forest_data.summary %>% 
  filter(Author != 'Pooled Effect') %>% 
  filter(b_Intercept > median(b_Intercept))

first_half.studies <- first_half.studies[order(first_half.studies$b_Intercept, decreasing=T),]

second_half.studies <- overall.forest_data.summary %>% 
  filter(Author != 'Pooled Effect') %>% 
  filter(b_Intercept <= median(b_Intercept))

second_half.studies <- second_half.studies[order(second_half.studies$b_Intercept, decreasing=T),]


second_half.studies_bool <- sapply(second_half.studies$Author,
                              function(x){
                                !any(grepl(x,first_half.studies$Author))
                              })
second_half.studies <- second_half.studies[second_half.studies_bool, ]

second_half.studies <- rbind(second_half.studies, 
                             overall.forest_data.summary[which(overall.forest_data.summary$Author == 'Pooled Effect'),])


first_half.studies$group <- factor(0)
second_half.studies$group <- factor(c(rep(0,nrow(second_half.studies)-1),1))
  
first_half.forest_plot <- ggplot(data = overall.forest_data %>% 
                                   filter(Author %in% first_half.studies$Author),
       aes(x=b_Intercept, y=relevel(Author,'Pooled Effect',after=Inf),  fill=group)) +
  
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(overall_model)[1, 1], 
             color = "red", size = 1.2, alpha=0.5) +
  geom_vline(xintercept = fixef(overall_model)[1, 3:4], 
             color = "grey", linetype = 2, size=1) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  
  # Add densities
  geom_density_ridges(rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha=0.5, size=1.5) +
  
  scale_fill_manual(values = c("blue", "lightblue"), guide='none') +
  scale_colour_manual(values=c('black','black'))+
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=1.7, point_size=1.6) +
  
  lims(x = c(-1.8,1.8)) + 
  
  # Add text and labels
  geom_text(data = mutate_if(first_half.studies, 
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = expression("Hedge's "*italic(g)), # summary measure
       y = element_blank()) +
  theme_minimal()


second_half.forest_plot <- ggplot(data = overall.forest_data %>% 
                                   filter(Author %in% second_half.studies$Author | Author == 'Pooled Effect'), 
                                 aes(x=b_Intercept, y=relevel(Author,'Pooled Effect', after=Inf), fill=group)) +
  
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(overall_model)[1, 1], 
             color = "red", size = 1.2, alpha=0.5) +
  geom_vline(xintercept = fixef(overall_model)[1, 3:4], 
             color = "grey", linetype = 2, size=1) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  
  # Add densities
  geom_density_ridges(rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.6) +
  
  scale_fill_manual(values = c("blue", "lightblue"), guide='none') +
  scale_colour_manual(values=c('black','black')) +
  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=1.7, point_size=1.6) +
  
  lims(x = c(-2,1.8)) + 
  
  # Add text and labels
  geom_text(data = mutate_if(second_half.studies, 
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = expression("Hedge's "*italic(g)), # summary measure
       y = element_blank()) +
  theme_minimal()


overall_forest.plot <- grid.arrange(first_half.forest_plot, second_half.forest_plot, ncol=2,
                                    top=textGrob(label='Study Contribution to Pooled Effect',
                                                 gp=gpar(fontsize=20,fontface=2)))


ggsave('Overall_Forest_Plot.jpg', plot=overall_forest.plot, path=plotDir,
       units='in', width=13, height=7)


## ---- Subgroup Analyses ----

## ---- Exercise Intensity ----

# Influence of exercise Intensity

contrasts(global.exInfluence.studies$Ex.ACSM.2) <- contr.orthonorm

exIntensity.model <- update(overall_model, formula. = ~ . + Ex.ACSM.2,
                            newdata = global.exInfluence.studies,
                            prior = priors, iter = 10000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123,
                            file=paste(modelDir,'subgroup_intensity', sep='/'),
                            file_refit = 'on_change')

summary(exIntensity.model)


intensity_model.post_samps <- posterior_samples(exIntensity.model, c('^b','^sd'))

names(intensity_model.post_samps) <- c(levels(global.exInfluence.studies$Ex.ACSM.2),'tau1','tau2')

# transform posterior samples so they are not deflections from intercept
intensity_model.post_samps <- intensity_model.post_samps %>% 
  mutate(Light = `Very Light` + Light, 
         `Light-Moderate` = `Very Light` + `Light-Moderate`, 
         Moderate = `Very Light` + Moderate, 
         `Moderate-Vigorous` = `Very Light` + `Moderate-Vigorous`, 
         Vigorous = `Very Light`+Vigorous, 
         Maximal = Maximal + `Very Light`)


intensity_model.post_hdi <- mean_hdi(intensity_model.post_samps, .width=hdi_width)

intensity_model.betas <- intensity_model.post_samps %>% 
  select(`Very Light`:Maximal) %>% 
  melt()

names(intensity_model.betas) <- c('Intensity', 'beta')

# Plot posteriors
intensity_model.posterior_plot <- ggplot(data = intensity_model.betas, aes(x = beta)) +
  facet_wrap(~ Intensity, ncol = 1, scales = 'fixed') + 
  geom_density(fill = 'light blue',
               color = "black", alpha = 0.5, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=6, point_size=3.5) +
  labs(x = expression(beta),
       y = 'Density',
       title='Influence of Exercise Intensity on Cognition') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.title = element_text(size=12,color='black')
)

ggsave('Subgroup_exIntensity_Posteriors.jpg', plot=intensity_model.posterior_plot, path=plotDir,
       units='in', width=5, height=7)

## ---- Influence of Cognitive Domain ----
contrasts(global.exInfluence.studies$Domain.2) <- contr.orthonorm

cogDomain_model <- update(overall_model, formula. = ~ . + Domain.2,
                       newdata=global.exInfluence.studies,
                       prior= priors,
                       iter = 10000, chains = 4, warmup=2000,
                       save_pars = save_pars(all=T), seed = 123,
                       file=paste(modelDir,'subgroup_cogDomain',sep='/'),
                       file_refit = 'on_change')


summary(cogDomain_model)

cogDomain_model.post_samps <- posterior_samples(cogDomain_model, c('^b','^sd'))

# transform posterior samples so they are not deflections from intercept
cogDomain_model.betas <- cogDomain_model.post_samps %>% 
  select(b_Intercept:b_Domain.2WorkingMemory)


names(cogDomain_model.betas) <- levels(global.exInfluence.studies$Domain.2)

cogDomain_model.betas[,-1] <- apply(cogDomain_model.betas[,-1],2,
                                    function(x){
                                      x + cogDomain_model.betas$Attention
                                    })
  
cogDomain_model.betas <- melt(cogDomain_model.betas)

names(cogDomain_model.betas) <- c('Domain','beta')

# Plot Posteriors
cogDomain_model.posterior_plot <- ggplot(data = cogDomain_model.betas, aes(x = beta)) +
  facet_wrap(~ Domain, ncol = 3, scales = 'fixed') + 
  geom_density(fill = 'light blue',
               color = "black", alpha = 0.5, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=1, point_size=2) +
  labs(x = expression(beta),
       y = 'Density',
       title='Influence of Exercise on Cognitive Domain') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.title = element_text(size=12,color='black')
  )
  
ggsave('Subgroup_cogDomain_Posteriors.jpg', plot=cogDomain_model.posterior_plot, path=plotDir,
       units='in', width=5, height=4)


## ---- Influence of Exercise Mode ----
contrasts(global.exInfluence.studies$Ex.Mode.2) <- contr.orthonorm

exMode_model <- update(overall_model, formula. = ~ . + Ex.Mode.2,
                    newdata=global.exInfluence.studies,
                    prior= c(overall_effect.priors, betaWeight_prior),
                    iter = 10000, chains = 4, warmup=2000,
                    save_pars = save_pars(all=T), seed = 123,
                    file=paste(modelDir,'subgroup_exMode',sep='/'),
                    file_refit = 'on_change')


summary(exMode_model)

# plot posteriors
exMode_model.post_samps <- as_draws_df(exMode_model, variable=c('^b','^sd'), regex=T)

exMode_model.betas <- exMode_model.post_samps %>% 
  select(b_Intercept:b_Ex.Mode.210)

names(exMode_model.betas) <- levels(global.exInfluence.studies$Ex.Mode.2)


exMode_model.betas[,-1] <- apply(exMode_model.betas[,-1], 2, 
                                 function(x){
                                   x + exMode_model.betas$Circuit
                                 })


exMode_model.betas <- melt(exMode_model.betas)

names(exMode_model.betas) <- c('Ex.Mode','beta')


exMode_model.posterior_plot <- ggplot(data = exMode_model.betas, aes(x = beta)) +
  facet_wrap(~ Ex.Mode, ncol = 4, scales = 'fixed') + 
  geom_density(fill = 'light blue',
               color = "black", alpha = 0.5, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=2, point_size=1) +
  labs(x = expression(beta),
       y = 'Density',
       title='Influence of Exercise Mode on Cognition') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=10,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=6,color='black'),
    axis.title = element_text(size=7,color='black'),
    strip.text = element_text(size=8,color='black')
  )


ggsave('Subgroup_exMode_Posteriors.jpg', plot=exMode_model.posterior_plot, path=plotDir,
       units='in', width=5, height=4)

## ---- Influence of Test Time on Effect ----
contrasts(global.exInfluence.studies$EffectTime.2) <- contr.orthonorm

testTime_model <- update(overall_model, formula. = ~ . + EffectTime.2,
                         newdata=global.exInfluence.studies,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 10000, chains = 4, warmup=2000,
                         save_pars = save_pars(all=T), seed = 123,
                         file=paste(modelDir,'subgroup_testTime',sep='/'),
                         file_refit = 'on_change')

summary(testTime_model)

testTime_model.post_samps <- posterior_samples(testTime_model, c('^b','^sd'))

testTime_model.betas <- testTime_model.post_samps %>% 
  select(b_Intercept:b_EffectTime.2post180min)


names(testTime_model.betas) <- levels(global.exInfluence.studies$EffectTime.2)


testTime_model.betas[,-1] <- apply(testTime_model.betas[,-1], 2,
                                   function(x){
                                     testTime_model.betas[,1] + x
                                   })

testTime_model.betas <- melt(testTime_model.betas)

names(testTime_model.betas) <- c('Task_Time', 'beta')


testTime_model.posterior_plot <- ggplot(data = testTime_model.betas, aes(x = beta)) +
  facet_wrap(~ Task_Time, ncol = 3, scales = 'fixed') + 
  geom_density(fill = 'light blue',
               color = "black", alpha = 0.5, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=6, point_size=3.5) +
  labs(x = expression(beta),
       y = 'Density',
       title='Influence of Task Time') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.title = element_text(size=12,color='black')
  )


ggsave('Subgroup_taskTime_Posteriors.jpg', plot=testTime_model.posterior_plot, path=plotDir,
       units='in', width=13, height=7)


## ---- RT vs Accuracy ----
contrasts(global.exInfluence.studies$DV.2) <- contr.orthonorm

outcome_model <- update(overall_model, formula. = ~ . + DV.2,
                        newdata=global.exInfluence.studies,
                        prior= c(overall_effect.priors, betaWeight_prior),
                        iter = 10000, chains = 4, warmup=2000,
                        save_pars = save_pars(all=T), seed = 123,
                        file=paste(modelDir,'subgroup_outcomeMeasure',sep='/'),
                        file_refit = 'on_change')

summary(outcome_model)


outcome_model.post_samps <- posterior_samples(outcome_model, c('^b','^sd'))

outcome_model.betas <- outcome_model.post_samps[,c(1,2)]

names(outcome_model.betas) <- levels(global.exInfluence.studies$DV.2)

outcome_model.betas$RT <- outcome_model.betas$Accuracy + outcome_model.betas$RT

outcome_model.betas <-  melt(outcome_model.betas)

names(outcome_model.betas) <- c('Dependent_Variable','beta')


outcome_model.posterior_plot <- ggplot(data = outcome_model.betas, aes(x = beta)) +
  facet_wrap(~ Dependent_Variable, ncol = 3, scales = 'fixed') + 
  geom_density(fill = 'light blue',
               color = "black", alpha = 0.5, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=6, point_size=3.5) +
  labs(x = expression(beta),
       y = 'Density',
       title='Influence of Exercise on Dependent Variable') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.title = element_text(size=12,color='black')
  )


ggsave('Subgroup_outcomeMeasure_Posteriors.jpg', plot=outcome_model.posterior_plot, path=plotDir,
       units='in', width=5, height=4)

## ---- Task ----
# Note: few effects for each task. May be too fine grained of an analysis
contrasts(global.exInfluence.studies$Task.2) <- contr.orthonorm

task_model <- update(overall_model, formula. = ~ . + Task.2,
                         newdata=global.exInfluence.studies,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 10000, chains = 4, warmup=2000,
                         save_pars = save_pars(all=T), seed = 123,
                         file=paste(modelDir,'subgroup_task',sep='/'),
                         file_refit = 'on_change')

task_model.post_samps <- posterior_samples(task_model, c('^b','^sd'))

task_model.betas <- task_model.post_samps %>% 
  select(b_Intercept:b_Task.2wordrecall)


names(task_model.betas) <- levels(global.exInfluence.studies$Task.2)


task_model.betas[,-1] <- apply(task_model.betas[,-1], 2,
                                   function(x){
                                     task_model.betas[,1] + x
                                   })

task_model.betas <-  melt(task_model.betas)


names(task_model.betas) <- c('Task','beta')


task_beta.table <- task_model.betas %>% group_by(Task) %>% 
  mean_hdi(beta, .width=hdi_width)

task_beta.table <- as.data.frame(task_beta.table[,-c(5,6,7)])

task_beta.table$N_effects <- table(global.exInfluence.studies$Task.2)


task_bool <- apply(task_beta.table, 1, 
                   function(x){
                     l <- as.numeric(x['.lower'])
                     u <- as.numeric(x['.upper'])
                     if ((l < 0) & (u > 0)){
                       z <- 0
                     } else {
                       z <- 1
                     }
                   })

task_betas.gt_tbl <- gt(task_beta.table[which(task_bool==1),],rowname_col = 'rowname') %>% 
  fmt_number(columns=c(2,3), decimals=2) %>% 
  fmt_scientific(columns=4, decimals=2) %>% 
  tab_header(
    title = md('**Influence of Exercise on Task Performance**'),
  ) %>% 
  cols_label(Task='Task', beta = html('&beta;'), .lower = '89% HDI\nLower', .upper='89% HDI\nUpper', 
             N_effects='Number of Effects')


gtsave(task_betas.gt_tbl,filename = 'Task_Beta_Weights.png', path=plotDir)



## ---- Exercise Duration ----
contrasts(global.exInfluence.studies$Duration.2) <- contr.orthonorm

duration_model <- update(overall_model, formula. = ~ . + Duration.2,
                         newdata=global.exInfluence.studies,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 10000, chains = 4, warmup=2000,
                         save_pars = save_pars(all=T), seed = 123,
                         file=paste(modelDir,'subgroup_duration',sep='/'),
                         file_refit = 'on_change')

summary(duration_model)

duration_model.post_samps <- posterior_samples(duration_model, c('^b','^sd'))

duration_model.betas <- duration_model.post_samps %>% 
  select(b_Intercept:b_Duration.2setsduration)

names(duration_model.betas) <- levels(global.exInfluence.studies$Duration.2)


duration_model.betas[,-1] <- apply(duration_model.betas[,-1], 2,
                               function(x){
                                 duration_model.betas[,1] + x
                               })
duration_model.betas <- melt(duration_model.betas)

names(duration_model.betas) <- c('Duration','beta')

dur.labels <- c('\U2264 15','20-27','30-35', '40-45','\U003E 60','task completion','volitional exhaustion', 
                'sets duration')

names(dur.labels) <- c('<=15','20-27','30-35', '40-45','>60','task completion','volitional exhaustion', 
                       'sets duration')

duration_model.posterior_plot <- ggplot(data = duration_model.betas, aes(x = beta)) +
  facet_wrap(~ Duration, ncol = 3, scales = 'fixed', labeller = labeller(Duration=dur.labels)) + 
  geom_density(fill = 'light blue',
               color = "black", alpha = 0.5, size=0.73) +  
  stat_pointinterval(point_interval = mean_hdi, .width = hdi_width, 
                     size=1.5, point_size=1) +
  labs(x = expression(beta),
       y = 'Density',
       title='Influence of Exercise Duration (Minutes) on Cognition') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=10,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=6,color='black'),
    axis.title = element_text(size=7,color='black'),
    strip.text = element_text(size=8,color='black')
  )


ggsave('Subgroup_Duration_Posteriors.jpg', plot=duration_model.posterior_plot, path=plotDir,
       units='in', width=5, height=4)





## ---- Full Subgroup Model ----

all_subgroups.model <- update(overall_model, formula. = ~ . + Ex.ACSM.2 + Domain.2 + Ex.Mode.2 + EffectTime.2,
                        newdata=global.exInfluence.studies,
                        prior= c(overall_effect.priors, betaWeight_prior),
                        iter = 10000, chains = 4, warmup=2000,
                        save_pars = save_pars(all=T), seed = 123,
                        file=paste(modelDir,'all_subgroups',sep='/'),
                        file_refit = 'on_change')

summary(all_subgroups.model)



# ---- Interaction of Intensity and Domain ----
intensity_cogDomain.model <- brm(g|se(g_se) ~ 1 + (1|Author/es.ids) + Ex.ACSM.2*Domain.2,
                                 data=global.exInfluence.studies,
                                 prior= c(overall_effect.priors, betaWeight_prior),
                                 iter = 10000, chains = 4, warmup=2000,
                                 save_pars = save_pars(all=T), seed = 123,
                                 file=paste(modelDir,'subgroup_intensity_x_cogDomain',sep='/'),
                                 file_refit = 'on_change')




