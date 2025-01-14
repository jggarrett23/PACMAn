"
-----------------------------------------------------------
PACMAn Meta-Analysis: Plotting Model Posterior Distributions


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"

library(tidyverse)
library(brms)
library(bayestestR)
library(reshape2)
library(emmeans)
library(tidybayes)
library(ggplot2)
library(ggridges)


# --- Setting Up Directories ---

parentDir <- '/work/garrett/PACMAn/NatComm_Update'
modelDir <- file.path(parentDir,'Models/EF')
plotDir <- file.path(parentDir,'Figures')

setwd(modelDir)

# --- Load Models ---
compute_margins = F

hdi_width=0.89
overall_model <- readRDS('ef_overall.rds')

if (compute_margins){
  
  exIntensity.model <- readRDS('ef_subgroup_intensity.rds')
  cogDomain_model <- readRDS('ef_subgroup_cogDomain.rds')
  exMode_model <- readRDS('ef_subgroup_exMode.rds')
  testTime_model <- readRDS('ef_subgroup_testTime.rds')
  outcome_model <- readRDS('ef_subgroup_outcomeMeasure.rds')
  duration_model <- readRDS('ef_subgroup_duration.rds')
  
  
  # --- Extract Marginal Distributions ---
  hdi_width = 0.89
  
  overall.margins <- emmeans(overall_model,"1", level=hdi_width)
  inten.margins <- emmeans(exIntensity.model, ~ Ex.ACSM.2, level=hdi_width)
  domain.margins <- emmeans(cogDomain_model, ~ Domain.3, level=hdi_width)
  type.margins <- emmeans(exMode_model, ~ Ex.Mode.2, level=hdi_width)
  outcome.margins <- emmeans(outcome_model, ~ OutcomeVariable, level=hdi_width)
  duration.margins <- emmeans(duration_model, ~ Duration.3, level=hdi_width)
  time.margins <- emmeans(testTime_model, ~ EffectTime, level=hdi_width)
} else {
  
  # Load previously computed marginals after running "Model_Comparison.R"
  model.marginals <- readRDS('ef_model_marginals.rds')
  
  overall.margins <- model.marginals$Overall
  inten.margins <- model.marginals$`Exercise Intensity`
  domain.margins <- model.marginals$`Cognitive Domain`
  type.margins <- model.marginals$`Exercise Type`
  outcome.margins <- model.marginals$`Outcome Measure`
  duration.margins <- model.marginals$`Exercise Duration`
  time.margins <- model.marginals$`Task Time`
}



# Convert posteriors to data frames

overall.posteriors <- as.data.frame(as.mcmc(overall.margins, sep.chains=F))
names(overall.posteriors) <- 'g'

inten.posteriors <- as.data.frame(as.mcmc(inten.margins, sep.chains=F))
colnames(inten.posteriors) <- inten.margins@levels$Ex.ACSM
inten.posteriors <- melt(inten.posteriors)
names(inten.posteriors) <- c('Intensity', 'beta')

domain.posteriors <- as.data.frame(as.mcmc(domain.margins, sep.chains=F))
colnames(domain.posteriors) <- domain.margins@levels$Domain.3
domain.posteriors <- melt(domain.posteriors)
names(domain.posteriors) <- c('Domain', 'beta')

type.posteriors <- as.data.frame(as.mcmc(type.margins, sep.chains=F))
colnames(type.posteriors) <- type.margins@levels$Ex.Mode.2
type.posteriors <- melt(type.posteriors)
names(type.posteriors) <- c('ExType', 'beta')

outcome.posteriors <- as.data.frame(as.mcmc(outcome.margins, sep.chains=F))
colnames(outcome.posteriors) <- outcome.margins@levels$OutcomeVariable
outcome.posteriors <- melt(outcome.posteriors)
names(outcome.posteriors) <- c('DV', 'beta')

duration.posteriors <- as.data.frame(as.mcmc(duration.margins, sep.chains=F))
colnames(duration.posteriors) <- duration.margins@levels$Duration.3
duration.posteriors <- melt(duration.posteriors)
names(duration.posteriors) <- c('Duration', 'beta')

time.posteriors <- as.data.frame(as.mcmc(time.margins, sep.chains=F))
colnames(time.posteriors) <- time.margins@levels$EffectTime
time.posteriors <- melt(time.posteriors)
names(time.posteriors) <- c('EffectTime', 'beta')

# ---- Plotting ----

# Overall ----
variance_posts <- as_draws_df(overall_model, variable=c('^sd'), regex=T)
variance_posts <- variance_posts[c(1:2)]
names(variance_posts) <- c('tau1', 'tau2')

overall.posteriors <- cbind(overall.posteriors, variance_posts)

overall_model.post_hdi <- mode_hdi(overall.posteriors, .width=hdi_width)


overall.mu_plot <- ggplot(data = overall.posteriors, aes(x = g)) +
  geom_density(fill = "mediumblue",                # set the color
               color = "mediumblue", alpha = 0.15, size=1) +  
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=2) +
  geom_vline(xintercept=0, lty='dashed', color='slategray', size=0.8, alpha=0.5) + 
  labs(x = expression(paste("Hedge's ", italic(g))),
       y = 'Density',
       title='Executive Function Overall Effect') +
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black')
  ) + 
  
  annotate(geom='text', x=overall_model.post_hdi$g,
           y=0.7, label=sprintf('%.2f',overall_model.post_hdi$g), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$g.lower,
           y=0.7, label=sprintf('%.2f',overall_model.post_hdi$g.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$g.upper,
           y=0.7, label=sprintf('%.2f',overall_model.post_hdi$g.upper), size=2.2)

# between study heterogeneity
overall.tau1_plot <- ggplot(aes(x = tau1), data = overall.posteriors) +
  geom_density(fill = "forestgreen",               # set the color
               color = "forestgreen", alpha = 0.25, size=1) +  
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
  
  annotate(geom='text', x=overall_model.post_hdi$tau1,
           y=.7, label=sprintf('%.2f',overall_model.post_hdi$tau1), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1.lower,
           y=.7, label=sprintf('%.2f',overall_model.post_hdi$tau1.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau1.upper,
           y=.7, label=sprintf('%.2f',overall_model.post_hdi$tau1.upper), size=2.2)

# within study heterogeneity
overall.tau2_plot <- ggplot(aes(x = tau2), data = overall.posteriors) +
  geom_density(fill = "seagreen3",               # set the color
               color = "seagreen3", alpha = 0.25, size=1) +  
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
  
  annotate(geom='text', x=overall_model.post_hdi$tau2,
           y=1.3, label=sprintf('%.2f',overall_model.post_hdi$tau2), size=2.3, fontface=2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2.lower,
           y=1.2, label=sprintf('%.2f',overall_model.post_hdi$tau2.lower), size=2.2) +
  
  annotate(geom='text', x=overall_model.post_hdi$tau2.upper,
           y=1.2, label=sprintf('%.2f',overall_model.post_hdi$tau2.upper), size=2.2)

ggsave('EF_Model_mu.jpg', plot=overall.mu_plot, path=plotDir,
       units='in', width=2.9, height=1.98, bg='white')

ggsave('EF_Model_tau1.jpg', plot=overall.tau1_plot, path=plotDir,
       units='in', width=2.9, height=1.98, bg='white')

ggsave('EF_Model_tau2.jpg', plot=overall.tau2_plot, path=plotDir,
       units='in', width=2.9, height=1.98, bg='white')

# Overall ECDF ----

overall_effect.ecdf_plot <- ggplot(overall.posteriors, aes(x=g)) + 
  stat_ecdf(size = 1, color='mediumblue') + 
  geom_vline(xintercept=mean(overall.posteriors$g), 
             color='black', size=0.8, linetype='dashed') +
  labs(y='Cumulative Probability',
       x=expression(paste("Hedge's ", italic(g)))) + 
  theme_light() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line = element_line(colour="black",size=.4),
    panel.border = element_blank(),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=9,color='black')
  )

ggsave('EF_Model_mu_ecdf.jpg', plot=overall_effect.ecdf_plot, path=plotDir,
       units='in', width=2.65, height=1.98, bg='white')

# Savage Dickey ----

overall.prior <- density(distribution_normal(n=nrow(overall.posteriors)))
overall.mu <- density(overall.posteriors$g)

overall.savage_dickey <- data.frame('x'=c(overall.prior$x, overall.mu$x), 
                                    'y'=c(overall.prior$y, overall.mu$y),
                                    'distribution'=c(rep('prior',length(overall.prior$x)),
                                                     rep('posterior', length(overall.prior$x))))

null_likelihoods <- overall.savage_dickey %>% 
  group_by(distribution) %>%  
  filter(abs(x-0) == min(abs(x-0)))


p <- ggplot(data = overall.savage_dickey, aes(x=x, y=y, color = distribution, 
                                              fill=distribution)) +
  geom_line(size=.6, show.legend=F) +  
  geom_area(alpha=0.15, position='identity', show.legend=F) +
  geom_vline(xintercept=0, lty='dashed', 
             color='slategray', size=0.8, alpha=0.5) + 
  labs(x = expression(paste("Hedge's ", italic(g))),
       y = 'Density',
       fill = 'Distribution',
       color = 'Distribution') +
  scale_fill_manual(values=c('mediumblue','red1'),
                    labels=c('Posterior','Prior')) + 
  scale_color_manual(values=c('mediumblue','red1'),
                     labels=c('Posterior','Prior')) + 
  xlim(c(-1.5,1.5)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size=9,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size=7,color='black'),
    axis.title = element_text(size=8,color='black'),
    legend.text = element_text(size=6),
    legend.title = element_text(size=6.5),
    legend.position='top'
  )

savageD_plot <- p + geom_point(data=null_likelihoods, 
                pch=21, size=1.5, 
                color='white',
               show.legend = F) 

ggsave('EF_Model_savageD.jpg', plot=savageD_plot, path=plotDir,
       units='in', width=3, height=1.98, bg='white')


# Subgroups ----

inten_plot <- ggplot(inten.posteriors, aes(x=beta, y=Intensity)) + 
  geom_density_ridges(fill='mediumblue', 
                      color='mediumblue',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.15) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1.5, point_size=1.5) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.6) + 
  labs(x = "Hedge's g", 
       title = 'Intensity',
       y=NULL) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), limits=rev) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=10,color='black')
  )


domain_plot <- ggplot(domain.posteriors, aes(x=beta, y=Domain)) + 
  geom_density_ridges(fill='mediumblue', 
                      color='mediumblue',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.15) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.5) + 
  labs(x = "Hedge's g", 
       title = 'Executive Function\nSub-Domain',
       y=NULL) + 
  scale_x_continuous(expand=c(0,0), limits=c(-0.8,1.25)) + 
  scale_y_discrete(expand=c(0,0), 
                   labels=c('Cognitive Control', 'Decision Making',
                           'Inhibition', 'Planning', 'Working Memory')) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11, hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=10,color='black')
  )


exType_plot <- ggplot(type.posteriors, aes(x=beta, y=ExType)) + 
  geom_density_ridges(fill='mediumblue', 
                      color='mediumblue',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.15) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.5) + 
  labs(x = "Hedge's g", 
       title = 'Type',
       y = NULL) + 
  scale_x_continuous(expand=c(0,0), limits=c(-0.6, 1.7)) + 
  scale_y_discrete(expand=c(0,0)) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=10,color='black')
  )

outcome_plot <- ggplot(outcome.posteriors, aes(x=beta, y=DV)) + 
  geom_density_ridges(fill='mediumblue', 
                      color='mediumblue',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.15) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=2, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.7) + 
  labs(x = "Hedge's g", 
       title = 'Task Performance Measure',
       y = NULL) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=10,color='black')
  )

dur.labels <- c('\U2264 15','20-27','30-35', '40-45','\U003E 60', 'undefined')

duration_plot <- ggplot(duration.posteriors, aes(x=beta, y=Duration)) + 
  geom_density_ridges(fill='mediumblue', 
                      color='mediumblue',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.15) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.5) + 
  labs(x = "Hedge's g", 
       title = 'Duration',
       y = NULL) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), limits=rev, labels=rev(dur.labels)) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=10,color='black')
  )

time_plot <- ggplot(time.posteriors, aes(x=beta, y=EffectTime)) + 
  geom_density_ridges(fill='mediumblue', 
                      color='mediumblue',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.15) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=2, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.7) + 
  labs(x = "Hedge's g", 
       title = 'Task Completion Time',
       y = NULL) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), limits=rev,
                   labels=c('Post\n(\U003E 180 min)', 'Post\n(20-75 min)', 'Post', 'During')) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=10,color='black')
  )


ggsave('EF_Subgroup_exIntensity_Posteriors.jpg',plot=inten_plot, path=plotDir,
       units='in', width=2.85, height=2.499, bg='white')

ggsave('EF_Subgroup_cogDomain_Posteriors.jpg', plot=domain_plot, path=plotDir,
       units='in', width=2.85, height=2.499, bg='white')

ggsave('EF_Subgroup_exMode_Posteriors.jpg', plot=exType_plot, path=plotDir,
       units='in', width=2.85, height=2.499, bg='white')

ggsave('EF_Subgroup_taskTime_Posteriors.jpg', plot=time_plot, path=plotDir,
       units='in', width=2.85, height=2.499, bg='white')

ggsave('EF_Subgroup_outcomeMeasure_Posteriors.jpg', plot=outcome_plot, path=plotDir,
       units='in',width=2.85, height=2.499, bg='white')

ggsave('EF_Subgroup_Duration_Posteriors.jpg', plot=duration_plot, path=plotDir,
       units='in', width=2.85, height=2.499, bg='white')