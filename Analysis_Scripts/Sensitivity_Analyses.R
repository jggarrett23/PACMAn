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

parentDir <- getwd()
modelDir <- file.path(parentDir,'Models')


setwd(modelDir)

# --- Load Models ---

overall_model <- readRDS('overall_random.rds')
exIntensity.model <- readRDS('subgroup_intensity.rds')
cogDomain_model <- readRDS('subgroup_cogDomain.rds')
exMode_model <- readRDS('subgroup_exMode.rds')
testTime_model <- readRDS('subgroup_testTime.rds')
outcome_model <- readRDS('subgroup_outcomeMeasure.rds')
task_model <- readRDS('subgroup_task.rds')
duration_model <- readRDS('subgroup_duration.rds')