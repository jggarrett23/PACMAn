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

parentDir <- getwd()
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')


setwd(dataDir)

# Load data
effectSizes.df <- readRDS('study_effectSizes_hand.rds')


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
                     iter = 10000, chains = 4, cores=4, warmup=2000,
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
               color = "black", alpha = 0.5, size=1) +  
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=2) +
  labs(x = expression(paste("Hedge's ", italic(g))),
       y = 'Density',
       title='Overall Effect') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black')
  ) + 
  annotate(geom='text', x=mean(overall_model.post_samps$g), y=2,
           label=paste(hdi_width*100,'% HDI',sep=''), size=2.5) +
  
  annotate(geom='text', x=overall_model.post_hdi$g,
           y=0.7, label=sprintf('%.2f',overall_model.post_hdi$g), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$g.lower,
           y=0.7, label=sprintf('%.2f',overall_model.post_hdi$g.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$g.upper,
           y=0.7, label=sprintf('%.2f',overall_model.post_hdi$g.upper), size=2.2)


# between study heterogeneity
overall.tau1_plot <- ggplot(aes(x = tau1), data = overall_model.post_samps) +
  geom_density(fill = "darkgreen",               # set the color
               color = "black", alpha = 0.25, size=1) +  
  stat_pointinterval(point_interval = mean_hdi, .width = .89, 
                     size=1, point_size=2) +        
  labs(x = expression({tau[Between]}),
       y = element_blank(),
       title='Between Study Heterogeneity') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=9,color='black')
  ) + 
  annotate(geom='text', x=mean(overall_model.post_samps$tau1), y=1.5,
           label=paste(hdi_width*100,'% HDI',sep=''), size=2.5) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1,
           y=.7, label=sprintf('%.2f',overall_model.post_hdi$tau1), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1.lower,
           y=.7, label=sprintf('%.2f',overall_model.post_hdi$tau1.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1.upper,
           y=.7, label=sprintf('%.2f',overall_model.post_hdi$tau1.upper), size=2.2)


# within study heterogenity
overall.tau2_plot <- ggplot(aes(x = tau2), data = overall_model.post_samps) +
  geom_density(fill = "lightgreen",               # set the color
               color = "black", alpha = 0.7, size=1) +  
  stat_pointinterval(point_interval = mean_hdi, .width = .89, 
                     size=1, point_size=2) +        
  labs(x = expression({tau[Within]}),
       y = element_blank(),
       title='Within Study Heterogeneity') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=9,color='black')
  ) + 
  annotate(geom='text', x=mean(overall_model.post_samps$tau2), y=3,
           label=paste(hdi_width*100,'% HDI',sep=''), size=2.5) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2,
           y=1.2, label=sprintf('%.2f',overall_model.post_hdi$tau2), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2.lower,
           y=1.2, label=sprintf('%.2f',overall_model.post_hdi$tau2.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2.upper,
           y=1.2, label=sprintf('%.2f',overall_model.post_hdi$tau2.upper), size=2.2)


overall_plot <- grid.arrange(overall.mu_plot,overall.tau1_plot, overall.tau2_plot, ncol=3,
                             top=textGrob('Overall Effect of Exercise on Cognition',
                                          gp=gpar(fontsize=12,fontface=2)))

ggsave('Overall_Model_mu.jpg', plot=overall.mu_plot, path=plotDir,
       units='in', width=2.9, height=1.98)

ggsave('Overall_Model_tau1.jpg', plot=overall.tau1_plot, path=plotDir,
       units='in', width=2.9, height=1.98)

ggsave('Overall_Model_tau2.jpg', plot=overall.tau2_plot, path=plotDir,
       units='in', width=2.9, height=1.98)

ggsave('Overall_Model_Posteriors.jpg', plot=overall_plot, path=plotDir,
       units='in', width=5, height=4)

## ---- Quantify Heterogeneity ----
# Use Higgins and Thompson (2002) to estimate within study variance. Then use estimate to compute I^2

estimate_v.q <- function(effect_variances){
  nume <- (length(effect_variances) - 1) * sum(1/effect_variances)
  denom <- (sum(1/effect_variances)^2) - sum(1/effect_variances^2)
  
  return(nume/denom)
}

v.q <- estimate_v.q(effectSizes.df$g_se)

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

se.seq <- seq(0,max(effectSizes.df$g_se),length.out=nrow(effectSizes.df))

# psuedo confidence interval
lwr <- overall_model.post_hdi$g - (1.96*se.seq)
uppr <- overall_model.post_hdi$g + (1.96*se.seq)

funnel <- ggplot(effectSizes.df, aes(x=g, y=g_se)) + 
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
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))

ggsave('Funnel_Plot.jpg', plot = funnel, path=plotDir,
       width=3.6, height=2.48)


# Egger's Regression Test
# if intercept significantly differs from zero, then evidence for publication bias
eggs.test <- effectSizes.df %>% 
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

## ---- Subgroup Analyses ----

## ---- Exercise Intensity ----

# Influence of exercise Intensity

contrasts(effectSizes.df$Ex.ACSM) <- contr.orthonorm

exIntensity.model <- update(overall_model, formula. = ~ . + Ex.ACSM,
                            newdata = effectSizes.df,
                            prior = priors, iter = 10000, chains = 4, warmup=2000,
                            save_pars=save_pars(all=T), seed=123, 
                            control=list(max_treedepth=15),
                            file=paste(modelDir,'subgroup_intensity', sep='/'),
                            file_refit = 'on_change')

## ---- Influence of Cognitive Domain ----
contrasts(effectSizes.df$Domain.2) <- contr.orthonorm

cogDomain_model <- update(overall_model, formula. = ~ . + Domain.2,
                          newdata=effectSizes.df,
                          prior= priors,
                          iter = 10000, chains = 4, warmup=2000,
                          save_pars = save_pars(all=T), seed = 123,
                          file=paste(modelDir,'subgroup_cogDomain',sep='/'),
                          file_refit = 'on_change')

## ---- Influence of Exercise Mode ----
contrasts(effectSizes.df$Ex.Mode.2) <- contr.orthonorm

exMode_model <- update(overall_model, formula. = ~ . + Ex.Mode.2,
                       newdata=effectSizes.df,
                       prior= c(overall_effect.priors, betaWeight_prior),
                       iter = 10000, chains = 4, warmup=2000,
                       save_pars = save_pars(all=T), seed = 123,
                       file=paste(modelDir,'subgroup_exMode',sep='/'),
                       file_refit = 'on_change')

## ---- Influence of Test Time on Effect ----
contrasts(effectSizes.df$EffectTime) <- contr.orthonorm

testTime_model <- update(overall_model, formula. = ~ . + EffectTime,
                         newdata=effectSizes.df,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 10000, chains = 4, warmup=2000,
                         save_pars = save_pars(all=T), seed = 123,
                         control = list(max_treedepth=15),
                         file=paste(modelDir,'subgroup_testTime',sep='/'),
                         file_refit = 'on_change')

## ---- RT vs Accuracy ----
outcome_model <- update(overall_model, formula. = ~ . + OutcomeVariable,
                        newdata=effectSizes.df,
                        prior= c(overall_effect.priors, betaWeight_prior),
                        iter = 10000, chains = 4, warmup=2000,
                        save_pars = save_pars(all=T), seed = 123,
                        file=paste(modelDir,'subgroup_outcomeMeasure',sep='/'),
                        file_refit = 'on_change')


## ---- Exercise Duration ----

contrasts(effectSizes.df$Duration.2) <- contr.orthonorm

duration_model <- update(overall_model, formula. = ~ . + Duration.2,
                         newdata=effectSizes.df,
                         prior= c(overall_effect.priors, betaWeight_prior),
                         iter = 10000, chains = 4, warmup=2000, cores = 4,
                         save_pars = save_pars(all=T), seed = 123,
                         control = list(max_treedepth=11),
                         file=paste(modelDir,'subgroup_duration',sep='/'),
                         file_refit = 'on_change')
