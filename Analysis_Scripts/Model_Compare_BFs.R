if(!require('bayestestR')) install.packages('bayestestR')
library(bayestestR)

parentDir <- '/work/garrett/PACMAn/NatComm_Update'
dataDir <- file.path(parentDir,'Data')
modelDir <- file.path(parentDir,'Models')
plotDir <- file.path(parentDir,'Figures')

setwd(modelDir)

# ---- Load models that have all been trained on same data points

overall_model <- readRDS('overall_random.rds')
inten_model <- readRDS('subgroup_intensity.rds')
domain_model <- readRDS('subgroup_cogDomain.rds')
type_model <- readRDS('subgroup_exMode.rds')
testTime_model <- readRDS('subgroup_testTime.rds')
outcome_model <- readRDS('subgroup_outcomeMeasure.rds')
duration_model <- readRDS('subgroup_duration.rds')
year_model <- readRDS('moderator_year.rds')
design_model <- readRDS('subgroup_design.rds')

# ---- Additive Models ----
inten_type_model <- readRDS('subgroup_intensity_exMode_model.rds')
inten_duration_model <- readRDS('subgroup_intensity_duration_model.rds')
domain_type_model <- readRDS('subgroup_domain_type_model.rds')
domain_duration_model <- readRDS('subgroup_domain_duration_model.rds')
domain_inten_model <- readRDS('subgroup_domain_intensity_model.rds')
domain_outcome_model <- readRDS('subgroup_domain_outcome_model.rds')
type_duration_model <- readRDS('subgroup_type_duration_model.rds')
type_outcome_model <- readRDS('subgroup_type_outcome_model.rds')

# ----- Interaction Models ----

inten_X_type_model <- readRDS('subgroup_intensity_X_exMode_model.rds')
inten_X_duration_model <- readRDS('subgroup_intensity_X_duration_model.rds')
domain_X_type_model <- readRDS('subgroup_domain_X_type_model.rds')
domain_X_duration_model <- readRDS('subgroup_domain_X_duration_model.rds')
domain_X_inten_model <- readRDS('subgroup_domain_X_intensity_model.rds')
domain_X_outcome_model <- readRDS('subgroup_domain_X_outcome_model.rds')
type_X_duration_model <- readRDS('subgroup_type_X_duration_model.rds')
type_X_outcome_model <- readRDS('subgroup_type_X_outcome_model.rds')

print('Models Loaded.')

#models.list <- list('Overall'= overall_model, 'Exercise Intensity' = inten_model, 
#                    'Cognitive Domain' = domain_model, 'Exercise Type' = type_model, 
#                    'Task Time' = testTime_model, 'Outcome Measure' = outcome_model,
#                    'Exercise Duration' = duration_model, 'Year' = year_model,
#                    'Design' = design_model, 'Inten.Type' = inten_type_model, 'Inten.Duration' = inten_duration_model,
#                    'Domain.Duration' = domain_duration_model, 'Domain.Outcome' = domain_outcome_model, 
#                    'Type.Duration' = type_duration_model, 'Inten x Type' = inten_X_type_model,
#                    'Inten x Duration' = inten_X_duration_model, 'Domain x Duration' = domain_X_duration_model, 
#                    'Domain x Outcome' = domain_X_outcome_model, 'Type x Duration' = type_X_duration_model,
#                    'Type.Outcome' = type_outcome_model, 'Type X Outcome' = type_X_outcome_model
#                        )

print('Comparing Models...')

model_compare_bfs <- bayesfactor_models(inten_model, domain_model, type_model, testTime_model,
                                        outcome_model, duration_model, year_model, design_model, 
                                        inten_type_model, inten_duration_model, domain_duration_model, domain_type_model,
                                        domain_outcome_model, type_duration_model, type_outcome_model,
                                        inten_X_type_model, inten_X_duration_model, domain_X_duration_model, domain_X_type_model,
                                        domain_X_outcome_model, type_X_duration_model, type_X_outcome_model,
                                        denominator=overall_model)

inclusion_bfs <- bayesfactor_inclusion(model_compare_bfs, 
                                       match=T)

model_compare_bfs$BF <- exp(model_compare_bfs$log_BF)
inclusion_bfs$BF <- exp(inclusion_bfs$log_BF)

saveRDS(model_compare_bfs, 'all_model_comparison_bfs.rds')
saveRDS(inclusion_bfs, 'all_model_inclusionBFs.rds')