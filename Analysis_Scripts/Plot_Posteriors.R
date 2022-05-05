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

parentDir <- getwd()
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')

setwd(modelDir)

# --- Load Models ---

overall_model <- readRDS('overall_random.rds')
exIntensity.model <- readRDS('subgroup_intensity.rds')
cogDomain_model <- readRDS('subgroup_cogDomain.rds')
exMode_model <- readRDS('subgroup_exMode.rds')
testTime_model <- readRDS('subgroup_testTime.rds')
outcome_model <- readRDS('subgroup_outcomeMeasure.rds')
duration_model <- readRDS('subgroup_duration.rds')


# --- Extract Marginal Distributions ---
hdi_width = 0.89

overall.margins <- emmeans(overall_model,"1", level=hdi_width)
inten.margins <- emmeans(exIntensity.model, ~ Ex.ACSM, level=hdi_width)
domain.margins <- emmeans(cogDomain_model, ~ Domain.2, level=hdi_width)
type.margins <- emmeans(exMode_model, ~ Ex.Mode.2, level=hdi_width)
outcome.margins <- emmeans(outcome_model, ~ OutcomeVariable, level=hdi_width)
duration.margins <- emmeans(duration_model, ~ Duration.2, level=hdi_width)
time.margins <- emmeans(testTime_model, ~ EffectTime, level=hdi_width)

# Convert posteriors to data frames

overall.posteriors <- as.data.frame(as.mcmc(overall.margins, sep.chains=F))
names(overall.posteriors) <- 'g'

inten.posteriors <- as.data.frame(as.mcmc(inten.margins, sep.chains=F))
colnames(inten.posteriors) <- inten.margins@levels$Ex.ACSM
inten.posteriors <- melt(inten.posteriors)
names(inten.posteriors) <- c('Intensity', 'beta')

domain.posteriors <- as.data.frame(as.mcmc(domain.margins, sep.chains=F))
colnames(domain.posteriors) <- domain.margins@levels$Domain.2
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
colnames(duration.posteriors) <- duration.margins@levels$Duration.2
duration.posteriors <- melt(duration.posteriors)
names(duration.posteriors) <- c('Duration', 'beta')

time.posteriors <- as.data.frame(as.mcmc(time.margins, sep.chains=F))
colnames(time.posteriors) <- time.margins@levels$EffectTime
time.posteriors <- melt(time.posteriors)
names(time.posteriors) <- c('EffectTime', 'beta')


# ---- Plotting ----
inten_plot <- ggplot(inten.posteriors, aes(x=beta, y=Intensity)) + 
  geom_density_ridges(fill='light blue', 
                      color='black',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.5) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1.5, point_size=1.5) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.6) + 
  labs(x = "Hedge's g", 
       y = 'Exercise Intensity') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), limits=rev) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=12,color='black')
  )


domain_plot <- ggplot(domain.posteriors, aes(x=beta, y=Domain)) + 
  geom_density_ridges(fill='light blue', 
                      color='black',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.5) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.5) + 
  labs(x = "Hedge's g", 
       y = 'Cognitive Domain') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=12,color='black')
  )


exType_plot <- ggplot(type.posteriors, aes(x=beta, y=ExType)) + 
  geom_density_ridges(fill='light blue', 
                      color='black',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.5) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.5) + 
  labs(x = "Hedge's g", 
       y = 'Exercise') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=12,color='black')
  )

outcome_plot <- ggplot(outcome.posteriors, aes(x=beta, y=DV)) + 
  geom_density_ridges(fill='light blue', 
                      color='black',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.5) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=2, point_size=2.5) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.7) + 
  labs(x = "Hedge's g", 
       y = 'Task Dependent Variable') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=12,color='black')
  )

dur.labels <- c('\U2264 15','20-27','30-35', '40-45','\U003E 60','Task Completion','Volitional exhaustion', 
                'Sets Duration')

duration_plot <- ggplot(duration.posteriors, aes(x=beta, y=Duration)) + 
  geom_density_ridges(fill='light blue', 
                      color='black',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.5) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=1, point_size=1) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.5) + 
  labs(x = "Hedge's g", 
       y = 'Exercise Duration') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), limits=rev, labels=rev(dur.labels)) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=12,color='black')
  )

time_plot <- ggplot(time.posteriors, aes(x=beta, y=EffectTime)) + 
  geom_density_ridges(fill='light blue', 
                      color='black',
                      size=0.75,
                      scale=0.9, 
                      rel_min_height=0.01,
                      alpha=0.5) + 
  stat_pointinterval(point_interval = mode_hdi, .width = hdi_width, 
                     size=2, point_size=2.5) + 
  geom_vline(xintercept=0, colour='slate grey', linetype='dashed', size=0.7) + 
  labs(x = "Hedge's g", 
       y = 'Cognitive Task Completion Time') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), limits=rev,
                   labels=c('Post (\U003E 180 min)', 'Post (20-75 min)', 'Post', 'During')) +
  coord_cartesian(clip='off') +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=16,hjust = 0.5),
    axis.line.x = element_line(colour="black",size=.4),
    axis.text = element_text(size=9,color='black'),
    axis.text.y = element_text(vjust=0),
    axis.title = element_text(size=12,color='black')
  )


ggsave('Subgroup_exIntensity_Posteriors.jpg', plot=inten_plot, path=plotDir,
       units='in', width=5, height=7)

ggsave('Subgroup_cogDomain_Posteriors.jpg', plot=domain_plot, path=plotDir,
       units='in', width=5, height=4)

ggsave('Subgroup_exMode_Posteriors.jpg', plot=exType_plot, path=plotDir,
       units='in', width=5, height=4)

ggsave('Subgroup_taskTime_Posteriors.jpg', plot=time_plot, path=plotDir,
       units='in', width=13, height=7)

ggsave('Subgroup_outcomeMeasure_Posteriors.jpg', plot=outcome_plot, path=plotDir,
       units='in', width=5, height=4)

ggsave('Subgroup_Duration_Posteriors.jpg', plot=duration_plot, path=plotDir,
       units='in', width=5, height=4)

