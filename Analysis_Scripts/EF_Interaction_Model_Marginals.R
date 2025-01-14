"
-----------------------------------------------------------
PACMAn Meta-Analysis: Model Comparisons and Hypothesis Testing


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
library(loo)


# --- Setting Up Directories ---

parentDir <- '/work/garrett/PACMAn/NatComm_Update/'
modelDir <- file.path(parentDir,'Models/EF')
plotDir <- file.path(parentDir,'Figures')

setwd(modelDir)

# --- Load Models ---

domain_X_outcome_model <- readRDS('ef_subgroup_domain_X_outcome_model.rds')
type_outcome_model <- readRDS('ef_subgroup_type_outcome_model.rds')
domain_X_type_model <- readRDS('ef_subgroup_domain_X_type_model.rds')


models.list <- list('Domain x Outcome'= domain_X_outcome_model,
                    'Type.Outcome' = type_outcome_model,
                    'Domain x Type' = domain_X_type_model
                    )

# --- Compute R^2 ---

models.R_2 <- lapply(models.list, 
                     function(x){
                       bayes_R2(x, probs=c(0.055,0.945))
                     })

# --- Compute LOOIC Scores (DO ON CLUSTER!!) ---

# ELPD = expected log predictive density, where higher values are better

# leave one out cross validation 
# WARNING: Computationally demanding. Save loo scores to prevent having to 
# run this line of code multiple times. Especially if have to set
# moment_match = T (i.e. pareto k > 0.7)
#models.loo <- lapply(models.list, loo)

#models.looic <- lapply(models.loo, 
#                       function(x){
#                         x$estimates[3]
#                       })

#models.looic <- melt(as.data.frame(models.looic))

#names(models.looic) <- c('Model', 'LOOIC')

#saveRDS(models.looic, 'models_looic.rds')

#models_looic.gt_tbl <- gt(models.looic,rowname_col = 'Model') %>% 
#  fmt_number(columns=2, decimals=2) %>% 
#  tab_header(
#    title = md('**Subgroup Analysis Model Comparison**'),
#  ) %>% 
#  cols_label(Model='Model', LOOIC = 'LOOIC')


#gtsave(models_looic.gt_tbl, filename = 'Models_LOOIC.png', path=plotDir)

# ---- Hypothesis Testing ----

# use bayes factors to determine the amount of evidence for model parameters to be different from 0
# use WAIC to do model comparison, then test model parameters using bayes factors

# To get stable bayes factors, best to use 10000 iterations


# First use emmeans to ensure that the correct estimate is computed since subgroup models
# fit with an orthonormal contrast
hdi_width = .89

domain_X_outcome.margins <- emmeans(domain_X_outcome_model, ~ Domain.3*OutcomeVariable, level=hdi_width)
type_outcome.margins <- emmeans(type_outcome_model, ~ Ex.Mode.2 + OutcomeVariable, level=hdi_width)
domain_X_type.margins <- emmeans(domain_X_type_model, ~ Domain.3*Ex.Mode.2, level=hdi_width)

domain_X_outcome.margins.prior <- emmeans(unupdate(domain_X_outcome_model),
                                          ~ Domain.3*OutcomeVariable, level=hdi_width)

type_outcome.margins.prior <- emmeans(unupdate(type_outcome_model),
                                      ~ Ex.Mode.2 + OutcomeVariable, level=hdi_width)

domain_X_type.margins.prior <- emmeans(unupdate(domain_X_type_model),
                                       ~ Domain.3*Ex.Mode.2, level=hdi_width)

model.marginals <- list('Domain x Outcome'= domain_X_outcome.margins,
                    'Type.Outcome' = type_outcome.margins,
                    'Domain x Type' = domain_X_type.margins
                    )

saveRDS(model.marginals, 'ef_interaction_model_marginals.rds')

model.marginals.priors <- list('Domain x Outcome'= domain_X_outcome.margins.prior,
                    'Type.Outcome' = type_outcome.margins.prior,
                    'Domain x Type' = domain_X_type.margins.prior
                    )

saveRDS(model.marginals.priors, 'ef_interaction_model_priors_marginals.rds')

# Bayes factors approximated using the Savage-Dickey Ratio
model.bfs <- lapply(1:length(model.marginals), 
                     function(x){
                       bayesfactor_parameters(model.marginals[[x]], prior=model.marginals.priors[[x]])
                     })

names(model.bfs) <- names(model.marginals)

saveRDS(model.bfs, 'ef_interaction_model_parameter_bfs.rds')

# have to define own mode function
arith.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# ---- Plot Posteriors vs Priors ----

#overall.bf_plot <- plot(model.bfs$Overall) + 
#  scale_fill_manual(values = c('lightblue','red')) + 
#  scale_color_manual(values = c('lightblue','red')) + 
#  theme_minimal() + 
#  labs(title='Overall Pooled Effect Posterior vs Prior', x=expression(mu)) + 
#  theme(plot.title = element_text(size=13,hjust = 0.5),
#        axis.line.x = element_line(colour="black",size=.4))


#ggplot(foo1, aes(x=g, y=Domain)) + 
#  geom_density_ridges(fill='lightblue', alpha=0.5, 
#                      size=1, rel_min_height=0.01, scale=0.95) + 
#  stat_pointinterval(point_interval=mode_hdi, .width=.89, 
#                     size=1, point_size=2) + 
#  scale_y_discrete(labels=c('Attention', 'Cognitive Control',
#                            'Decision Making', 'Executive Function', 
#                            'Information Processing', 'Inhibition',
#                            'Learning', 'Memory', 'Motor Skills',
#                            'Perception', 'Planning', 'Working Memory')) + 
#  geom_vline(xintercept=0, linetype='dotted') + 
#  labs(y='Cognitive Domain', x=expression(paste("Hedge's ", italic(g)))) + 
#  theme_minimal() + 
#  theme(
#    panel.grid.minor = element_blank(),
#    axis.text = element_text(size=12,color='black'),
#    axis.title = element_text(size=14,color='black')
#  ) 

# ---- Bayesian Model Averaging and Stacking ----

#lpd_point <- as.data.frame(lapply(models.loo, 
#                    function(x){
#                      x$pointwise[,'elpd_loo']
#                    }))

#lpd_point <- as.matrix(lpd_point)

# pseudo-BMA w/o Bayesian bootstrap
#pbma_wts <- pseudobma_weights(lpd_point, BB=F)

# pseudo-BMA w/ Bayesian bootstrap
#pbma_BB_wts <- pseudobma_weights(lpd_point)

# Bayesian Stacking
#stacking_wts <- stacking_weights(lpd_point)

#round(cbind(pbma_wts, pbma_BB_wts, stacking_wts), 2)
