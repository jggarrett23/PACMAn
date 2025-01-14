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
#task_model <- readRDS('subgroup_task.rds')
duration_model <- readRDS('subgroup_duration.rds')
year_model <- readRDS('moderator_year.rds')
design_model <- readRDS('subgroup_design.rds')
age_model <- readRDS('moderator_age.rds')
bmi_model <- readRDS('moderator_bmi.rds')
pcFem_model <- readRDS('moderator_pcFem.rds')
VO2_model <- readRDS('moderator_VO2.rds')
height_model <- readRDS('moderator_height.rds')
weight_model <- readRDS('moderator_weight.rds')



models.list <- list('Overall'= overall_model, 'Exercise Intensity'=exIntensity.model, 
                    'Cognitive Domain'=cogDomain_model, 'Exercise Type' = exMode_model, 
                    'Task Time' = testTime_model, 'Outcome Measure' = outcome_model,
                    'Exercise Duration' = duration_model, 'Year' = year_model,
                    'Design' = design_model, 'Age' = age_model, 'BMI' = bmi_model,
                    'pcFem' = pcFem_model, 'VO2' = VO2_model, 'Height' = height_model, 
                    'Weight' = weight_model
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

overall.margins <- emmeans(overall_model,"1", level=hdi_width)
inten.margins <- emmeans(exIntensity.model, ~ Ex.ACSM.2, level=hdi_width)
domain.margins <- emmeans(cogDomain_model, ~ Domain.2, level=hdi_width)
type.margins <- emmeans(exMode_model, ~ Ex.Mode.2, level=hdi_width)
#task.margins <- emmeans(task_model, ~ Task.2, level=hdi_width)
outcome.margins <- emmeans(outcome_model, ~ OutcomeVariable, level=hdi_width)
duration.margins <- emmeans(duration_model, ~ Duration.3, level=hdi_width)
time.margins <- emmeans(testTime_model, ~ EffectTime, level=hdi_width)
year.margins <- emmeans(year_model, ~ Date, level=hdi_width)
design.margins <- emmeans(design_model, ~ Exercise.Design, level=hdi_width)
age.margins <- emmeans(age_model, ~ Age.Centered, level=hdi_width)
bmi.margins <- emmeans(bmi_model, ~ BMI.Centered, level=hdi_width)
pcFem.margins <- emmeans(pcFem_model, ~ Percent.Female, level=hdi_width)
vo2.margins <- emmeans(VO2_model, ~ VO2.Centered, level=hdi_width)
height.margins <- emmeans(height_model, ~ Height.Centered, level=hdi_width)
weight.margins <- emmeans(weight_model, ~ Weight.Centered, level=hdi_width)


overall.margins.prior <- emmeans(unupdate(overall_model),"1", level=hdi_width)
inten.margins.prior <- emmeans(unupdate(exIntensity.model), ~ Ex.ACSM.2, level=hdi_width)
domain.margins.prior <- emmeans(unupdate(cogDomain_model), ~ Domain.2, level=hdi_width)
type.margins.prior <- emmeans(unupdate(exMode_model), ~ Ex.Mode.2, level=hdi_width)
#task.margins <- emmeans(task_model, ~ Task.2, level=hdi_width)
outcome.margins.prior <- emmeans(unupdate(outcome_model), ~ OutcomeVariable, level=hdi_width)
duration.margins.prior <- emmeans(unupdate(duration_model), ~ Duration.3, level=hdi_width)
time.margins.prior <- emmeans(unupdate(testTime_model), ~ EffectTime, level=hdi_width)
year.margins.prior <- emmeans(unupdate(year_model), ~ Date, level=hdi_width)
design.margins.prior <- emmeans(unupdate(design_model), ~ Exercise.Design, level=hdi_width)
age.margins.prior <- emmeans(unupdate(age_model), ~ Age.Centered, level=hdi_width)
bmi.margins.prior <- emmeans(unupdate(bmi_model), ~ BMI.Centered, level=hdi_width)
pcFem.margins.prior <- emmeans(unupdate(pcFem_model), ~ Percent.Female, level=hdi_width)
vo2.margins.prior <- emmeans(unupdate(VO2_model), ~ VO2.Centered, level=hdi_width)
height.margins.prior <- emmeans(unupdate(height_model), ~ Height.Centered, level=hdi_width)
weight.margins.prior <- emmeans(unupdate(weight_model), ~ Weight.Centered, level=hdi_width)


model.marginals <- list('Overall'= overall.margins, 'Exercise Intensity'=inten.margins, 
                        'Cognitive Domain'=domain.margins, 'Exercise Type' = type.margins, 
                        'Task Time' = time.margins, 'Outcome Measure' = outcome.margins,
                        'Exercise Duration' = duration.margins, 'Year' = year.margins,
                        'Design' = design.margins, 'Age' = age.margins, 'BMI' = bmi.margins,
                        'pcFem' = pcFem.margins, 'VO2' = vo2.margins, 'Height' = height.margins,
                        'Weight' = weight.margins
                        )

saveRDS(model.marginals, 'all_model_marginals.rds')

model.marginals.priors <- list('Overall'= overall.margins.prior, 'Exercise Intensity'=inten.margins.prior, 
                               'Cognitive Domain'=domain.margins.prior, 'Exercise Type' = type.margins.prior, 
                               'Task Time' = time.margins.prior, 'Outcome Measure' = outcome.margins.prior,
                               'Exercise Duration' = duration.margins.prior, 'Year' = year.margins.prior,
                               'Design' = design.margins.prior, 'Age' = age.margins.prior, 'BMI' = bmi.margins.prior,
                               'pcFem' = pcFem.margins.prior, 'VO2' = vo2.margins.prior, 'Height' = height.margins.prior,
                               'Weight' = weight.margins.prior)

saveRDS(model.marginals.priors, 'all_model_priors_marginals.rds')

# Bayes factors approximated using the Savage-Dickey Ratio
model.bfs <- lapply(1:length(model.marginals), 
                     function(x){
                       bayesfactor_parameters(model.marginals[[x]], prior=model.marginals.priors[[x]])
                     })

names(model.bfs) <- names(model.marginals)

saveRDS(model.bfs, 'all_model_parameter_bfs.rds')

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
