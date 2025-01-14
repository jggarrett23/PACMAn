library(ggplot2)
library(brms)
library(tidybayes)

parentDir <- '/work/garrett/PACMAn/NatComm_Update'
dataDir <- file.path(parentDir, 'Data')
modelDir <- file.path(parentDir, 'Models')
plotDir <- file.path(parentDir, 'Figures')

trim_filled_data <- readRDS(file.path(parentDir, 'Data/study_effects_trimmed_filled.rds'))
overall_model <- readRDS(file.path(parentDir, 'Models/overall_random.rds'))

overall_model.post_samps <- as_draws_df(overall_model, variable=c('^b','^sd'), regex = T)

names(overall_model.post_samps)[c(1:3)] <- c('g','tau1','tau2')

overall_model.post_hdi <- mode_hdi(overall_model.post_samps, .width=0.89)

se.seq <- seq(0,max(trim_filled_data$g_se),length.out=nrow(trim_filled_data))

# psuedo confidence interval
lwr <- overall_model.post_hdi$g - (1.96*se.seq)
uppr <- overall_model.post_hdi$g + (1.96*se.seq)

funnel <- ggplot(trim_filled_data, aes(x=g, y=g_se, group=interpolated)) + 
  geom_point(aes(shape=interpolated)) + 
  geom_segment(aes(x=overall_model.post_hdi$g, y=0, xend=overall_model.post_hdi$g, yend=max(g_se)),
               color='blue') + 
  geom_line(aes(x=lwr, y=se.seq), linetype='dashed') + 
  geom_line(aes(x=uppr, y=se.seq), linetype='dashed') + 
  xlim(c(-7,7)) + 
  labs(x = expression("Hedge's "*italic(g)),
       y = 'Standard Error') +
  scale_shape_manual(values=c(19, 1)) + 
  scale_y_reverse() + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=10),
        legend.position='none',
        axis.title = element_text(size=12))

ggsave('Funnel_Plot.jpg', plot = funnel, path=plotDir,
       width=3.6, height=2.48)