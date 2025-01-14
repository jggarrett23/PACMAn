"
-----------------------------------------------------------
PACMAn Meta-Analysis: Plotting typel Posterior Distributions


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


parentDir <- '/work/garrett/PACMAn/NatComm_Update'
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')

setwd(modelDir)

# ---- Load Interaction typels
interaction_margins <- readRDS('interaction_model_marginals.rds')
domain_type.margins <- interaction_margins$`Domain x Type`
domain_outcome.margins <- interaction_margins$`Domain x Outcome`
type_outcome.margins <- interaction_margins$'Type.Outcome'

hdi_width=0.89

domain_outcome.posteriors <- as.data.frame(as.mcmc(domain_outcome.margins, sep.chains=F))
domain_outcome.posteriors <- melt(domain_outcome.posteriors)
domain_outcome.posteriors <- separate(domain_outcome.posteriors,
                                   col=variable,
                                   into=c('Domain','Measure'), sep=" (?=[^ ]+$)")

domain_outcome.posteriors <- domain_outcome.posteriors %>% 
  mutate(g=value) %>% 
  select(Domain,Measure,g)

domain_type.posteriors <- as.data.frame(as.mcmc(domain_type.margins, sep.chains=F))
domain_type.posteriors <- melt(domain_type.posteriors)
domain_type.posteriors <- separate(domain_type.posteriors,
                                   col=variable,
                                   into=c('Domain','Type'), sep=" (?=[^ ]+$)")

domain_type.posteriors <- domain_type.posteriors %>% 
  mutate(g=value) %>% 
  select(Domain,Type,g)


type_outcome.posteriors <- as.data.frame(as.mcmc(type_outcome.margins, sep.chains=F))
type_outcome.posteriors <- melt(type_outcome.posteriors)
type_outcome.posteriors <- separate(type_outcome.posteriors,
                                    col=variable,
                                    into=c('Type', 'Measure'), sep=" (?=[^ ]+$)")

type_outcome.posteriors <- type_outcome.posteriors %>% 
                              mutate(g=value) %>% 
                              select(Type,Measure,g)
                              
  
# summarize ----

domain_outcome.post_summary <- domain_outcome.posteriors %>% 
  group_by(Domain,Measure) %>% 
  mode_hdi(.width=hdi_width)

domain_type.post_summary <- domain_type.posteriors %>% 
  group_by(Domain,Type) %>% 
  mode_hdi(.width=hdi_width)

type_outcome.post_summary <- type_outcome.posteriors %>% 
  group_by(Type,Measure) %>% 
  mode_hdi(.width=hdi_width)

domain_outcome.post_summary$Measure <- factor(domain_outcome.post_summary$Measure, 
                                              levels=c('Accuracy', 'RT'))

domain_outcome.post_summary$Domain <- lapply(domain_outcome.post_summary$Domain, 
                                          function(x){
                                              labs <- c('Attention', 'Executive Function', 'Information Processing',
                                                        'Learning', 'Memory', 'Motor Skills', 'Perception')
                                              for (i in 1:length(labs)){
                                                  if (grepl(labs[i], x, ignore.case=T)){
                                                      domain_idx <- i
                                                  }
                                              }
                                              z <- labs[domain_idx]
                                          })
domain_outcome.post_summary$Domain <- factor(domain_outcome.post_summary$Domain, 
                                          levels = c('Attention', 'Executive Function', 'Information Processing',
                                                        'Learning', 'Memory', 'Motor Skills', 'Perception')
                                          )

domain_outcome.plot <- ggplot(domain_outcome.post_summary, aes(x=Measure, y=g, color=Domain)) + 
  geom_pointrange(aes(ymin=.lower, ymax=.upper),
                  size=0.6, fatten=1,
                  position=position_dodge(.3),
                  show.legend=T) + 
  geom_hline(yintercept=0, lty='dashed', color='slate grey') + 
  scale_x_discrete(labels=c('Accuracy', 'RT')) + 
  scale_color_discrete(labels=c('Attention', 'Executive\nFunction', 'Information\nProcessing',
                                'Learning', 'Memory', 'Motor Skills', 'Perception'),
                       name='Cognitive Domain') +
  labs(y = "Hedge's g",
       x = 'Task Performance Measure') + 
  coord_flip() + 
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",linewidth=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0, size=8),
    axis.title = element_text(size=10,color='black'),
    legend.text = element_text(size=7, color='black'),
    legend.title = element_text(size=10, color='black'),
    legend.position = 'bottom'
  )

domainX.legend <- cowplot::get_legend(domain_outcome.plot)
domainX.legend.plot <- cowplot::ggdraw(domainX.legend)

domain_outcome.plot <- domain_outcome.plot + theme(legend.position='none')
  
domain_type.post_summary$Type <- factor(domain_type.post_summary$Type, 
                                          levels=c('Circuit', 'Cycling', 'HITT', 'Resistance',
                                                   'Running', 'Sport Activity', 'Walking'))

domain_type.post_summary$Domain <- lapply(domain_type.post_summary$Domain, 
                                          function(x){
                                              labs <- c('Attention', 'Executive Function', 'Information Processing',
                                                        'Learning', 'Memory', 'Motor Skills', 'Perception')
                                              for (i in 1:length(labs)){
                                                  if (grepl(labs[i], x, ignore.case=T)){
                                                      domain_idx <- i
                                                  }
                                              }
                                              z <- labs[domain_idx]
                                          })
domain_type.post_summary$Domain <- factor(domain_type.post_summary$Domain, 
                                          levels = c('Attention', 'Executive Function', 'Information Processing',
                                                        'Learning', 'Memory', 'Motor Skills', 'Perception')
                                          )

domain_type.plot <- ggplot(domain_type.post_summary, aes(x=Type, y=g, color=Domain)) + 
  geom_pointrange(aes(ymin=.lower, ymax=.upper),
                  size=0.6, fatten=1,
                  position=position_dodge(.3),
                  show.legend=T) + 
  geom_hline(yintercept=0, lty='dashed', color='slate grey') + 
  scale_x_discrete(labels=c('Circuit', 'Cycling', 'HIIT', 'Resistance',
                            'Running', 'Sport Activity', 'Walking')) + 
  scale_color_discrete(labels=c('Attention', 'Executive\nFunction', 'Information\nProcessing',
                                'Learning', 'Memory', 'Motor Skills', 'Perception'),
                       name='Cognitive Domain') +
  labs(y = "Hedge's g",
       x = 'Exercise Type') + 
  coord_flip() + 
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",linewidth=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0, size=8),
    axis.title = element_text(size=10,color='black'),
    legend.text = element_text(size=7, color='black'),
    legend.title = element_text(size=10, color='black'),
    legend.position = 'none'
  )

type_outcome.post_summary$Type <- factor(type_outcome.post_summary$Type, 
                                          levels=c('Circuit', 'Cycling', 'HIIT', 'Resistance',
                                                   'Running', 'Sport Activity', 'Walking'))

type_outcome.plot <- ggplot(type_outcome.post_summary, aes(x=Type, y=g, color=Measure)) + 
  geom_pointrange(aes(ymin=.lower, ymax=.upper),
                  size=0.7, fatten=1.2,
                  position=position_dodge(.3),
                  show.legend=T) + 
  geom_hline(yintercept=0, lty='dashed', color='slate grey') + 
  scale_x_discrete(labels=c('Circuit', 'Cycling', 'HITT', 'Resistance',
                            'Running', 'Sport Activity', 'Walking')) + 
  scale_color_discrete(type=c('firebrick1','navyblue'),
                       labels=c('Accuracy','RT'),
                       name='Exercise Type') +
  labs(y = "Hedge's g",
       x = 'Exercise Type') + 
  coord_flip() + 
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(size=11,hjust = 0.5),
    axis.line.x = element_line(colour="black",linewidth=.4),
    axis.text = element_text(size=8.5,color='black'),
    axis.text.y = element_text(vjust=0, size=8),
    axis.title = element_text(size=10,color='black'),
    legend.text = element_text(size=7, color='black'),
    legend.title = element_text(size=10, color='black'),
    legend.position = 'bottom'
  )

typeXoutcome.legend <- cowplot::get_legend(type_outcome.plot)
typeXoutcome.legend.plot <- cowplot::ggdraw(typeXoutcome.legend)

type_outcome.plot <- type_outcome.plot + theme(legend.position='none')

  
ggsave('domainXoutcome_Posteriors.jpg', plot=domain_outcome.plot, path=plotDir,
       units='in', width=4.25, height=3.7645, bg='white')

ggsave('domainXtype_Posteriors.jpg', plot=domain_type.plot, path=plotDir,
       units='in', width=4.25, height=3.7645, bg='white')

ggsave('domainXlegend.jpg', plot=domainX.legend.plot, path=plotDir,
       units='in', width=5, height=1.3, bg='white')

ggsave('type_outcome_Posteriors.jpg', plot=type_outcome.plot, path=plotDir,
       units='in', width=4.25, height=3.7645, bg='white')

ggsave('type_legend.jpg', plot=typeXoutcome.legend.plot, path=plotDir,
       units='in', width=4.1, height=1, bg='white')
