# TODO: Fix storage of designs with multiple post tests. 
# Fix studies with NA for effect size measures
# Compute effect sizes for other task times.


"
-----------------------------------------------------------
PACMAn Meta-Analysis: Compute Effect Sizes


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"


if(!require('readxl')) install.packages('readxl')
if (!require('googlesheets4')) install.packages('googlesheets4')
if(!require('googledrive')) install.packages('googledrive')
#if(!require('gt')) install.packages('gt')
if(!require('webshot')) install.packages('webshot')

library(googledrive)
library(googlesheets4)
library(readxl)
library(tidyverse)
library(dplyr)

## ---- Helper Functions ----

extract_studyCharacteristics <- function(data_row, allExLevel.cols){
  
  # determine study design
  design <- data_row$`Exercise Design`
  
  
  # determine task execution time
  task_time <- data_row$`Task Execution Time`
  
  # number of condition levels
  n_condLevels <- data_row$`N Ex Levels`
  
  study_condLevels.idx <-  allExLevel.cols[which((!is.na(data_row[allExLevel.cols])) & 
                                                   (sapply(data_row[allExLevel.cols],function(x){
                                                     !is.null(x)
                                                   })
                                                   ))]
  
  study_condLevels <- unlist(data_row[study_condLevels.idx])
  
  # sample sizes for each condition 
  study_condLevels.n <- data_row[(study_condLevels.idx+3)]
  
  # number of measures per condition
  n_condLevel_measures <- data_row$`N Task Measures Per Ex Level`
  
  study_characteristics <- list('design'=design,
                                'task_time'=task_time,
                                'num_condLevels'=n_condLevels,
                                'condLevels'=study_condLevels,
                                'condLevels.idx' = study_condLevels.idx,
                                'condLevels.n'=study_condLevels.n,
                                'condLevels.n_measures'=n_condLevel_measures)
  
  return(study_characteristics)
}


# weighted pooled standard deviation
wt.pooled_sd <- function(sds,sample_sizes){
  
  numerator <- sum((sds^2)*(sample_sizes-1))
  denominator <- sum(sample_sizes)-length(sds)
  
  return (sqrt(numerator/denominator))
  
}


# effect size variance
es.var <- function(design,cond1.n,cond2.n,smd){
  
  assumed.cor <- .6
  
  # between subjects formula 
  if (design == 'between'){
    first_term <- (sum(cond1.n,cond2.n)/(cond1.n*cond2.n))
    second_term <- (smd^2)/(2*sum(cond1.n,cond2.n))
    
    var_d <- sum(first_term,second_term)
  } else { # within subjects formula (requires assumed correlation)
    first_term <- (1/cond1.n) + (smd^2)/(2*cond1.n)
    second_term <- 2*(1-assumed.cor)
    
    var_d <- first_term*second_term
  }
  
  return(var_d)
}



## ---- Load Data ----

parentDir <- getwd()
dataDir <- file.path(parentDir,'Data')

setwd(dataDir)

#gs4_auth(email = 'jordangarrett@ucsb.edu')
#sheet_link <- 'https://docs.google.com/spreadsheets/d/1I7TtqSe9-p8RNyXuS_DJw3f0KmSOWysx9_lxFKaNSLM/edit#gid=0'
#allStudy_data <- read_sheet(sheet_link,sheet='Sheet1')
#allStudy.acsmIntensities <- read_sheet(sheet_link,sheet='ACSM Intensities')

data_file = 'PACMAN Effect Sizes.xlsx'

allStudy_data <- read_excel(data_file, sheet='Data')
allStudy.acsmIntensities <- read_excel(data_file, sheet='ACSM Intensities')


## ---- Preprocessing ----
"Means and standard deviations separated by a ';' indicate multiple measures for that condition."

# Drop unnecessary columns
allStudy_data <- allStudy_data[,c(-5,-193:-197)]

# Remove columns we don't have data for
allTask_data.cols <- as.vector(sapply(which(grepl('Cognitive Task',names(allStudy_data)) == T), 
                                      function(x){x+c(2:11)}))

allTask_mean.cols <- allTask_data.cols[seq(1,length(allTask_data.cols), by=2)]
n_tasks.means <- length(allTask_mean.cols)

noData_drop_matrix <- matrix(nrow=nrow(allStudy_data), ncol=n_tasks.means)
colnames(noData_drop_matrix) <- names(allStudy_data)[allTask_mean.cols]
for (iCol in seq(allTask_mean.cols)){
  col_num <- allTask_mean.cols[iCol]
  noData_drop_matrix[, iCol] <- sapply(allStudy_data[[names(allStudy_data)[col_num]]],
                                  function(x){
                                    any(c(is.na(x),is.null(x)))
                                  })
}

study_drop1.bool <- apply(noData_drop_matrix, 1,function(x) any(x == F))

allStudy_data.filt1 <- allStudy_data[study_drop1.bool,]

allStudy.acsmIntensities.filt1 <- allStudy.acsmIntensities[study_drop1.bool,]

# Check that the number of means match with task execution time
nMeans_drop_matrix <- matrix(nrow=nrow(allStudy_data.filt1), ncol=n_tasks.means)
for (iCol in seq(allTask_mean.cols)){
  col_num <- allTask_mean.cols[iCol]
  
  study_nMeasures <- sapply(allStudy_data.filt1[[names(allStudy_data.filt1)[col_num]]], 
                            function(x){
                              length(unlist(str_split(x,';')))
                            })
  
  nMeans_drop_matrix[,iCol] <- allStudy_data.filt1$`N Task Measures Per Ex Level` == study_nMeasures
  
}

study_drop2.bool <- apply(nMeans_drop_matrix, 1, function(x) any(x==T))

check_studies.1 <- allStudy_data.filt1[!study_drop2.bool,]

# study 24, 13, 51, & 111 are special cases 


# Standardize columns
allStudy_data.filt1$`Exercise Design` <- tolower(allStudy_data.filt1$`Exercise Design`)
allStudy_data.filt1$`Task Execution Time` <- tolower(allStudy_data.filt1$`Task Execution Time`)
allStudy_data.filt1$`Task Execution Time` <- str_replace_all(allStudy_data.filt1$`Task Execution Time`,'[-,]',';')

allStudy_data.filt1$`Task Execution Time` <- sapply(allStudy_data.filt1$`Task Execution Time`,
                                                    function(x){
                                                      gsub(" ", "",x, fixed=T)
                                                    })


## ---- Compute Effect Sizes ----
"Notes:
Lambourne & Tomporowski, 2010 conducted a meta-regression
on only within subject designs. They calculated effects for single
group by dividing the pre/post mean difference by the standard deviation
of the pretest. For cross-over designs with a control condition,
they calculated Hedge's g by subtracting the mean change of the experimental 
condition from the mean change of the control condition, and divide it by the pooled
standard deviation of pretest scores.

Ludyga et al., 2020 conducted a meta regression and 
calculated the difference for pre/post designs or changes scores and
standardized it by the within-group standard deviation.


McMorris et al., 2011, Moreau & Chou, 2019, and  also used the 
same approach described above for pre/post designs. 


*Converting Cohen's d to Pearson's r for Tad:
r = d/sqrt(d^2+a)

where a is a correction factor for cases when n_1 != n_2:

a = (n_1 + n_2)^2/(n_1*n_2)

and

V_r = (a^2*V_d)/(d^2+a)^3

"


allExLevel.cols <- which(grepl('Ex Level [[:digit:]]$',names(allStudy_data.filt1)) == T)

all_taskTimes <- unique(allStudy_data.filt1$`Task Execution Time`)

# separate index for task times with difference scores.
# values based off all_taskTimes vector
pre_post.idx <- c(1,6,9:12,14:16)
pre_during_post.idx <- c(4,7)
during.idx <- c(2,3,20)
post.idx <- c(5,13,17,21)
pre.idx <- c(18,19)
during_post.idx <- 8

# for study ID 7, exp 2, comparisons between exercise intensities are between subjects,
# while the rest condition is within subjects (i.e. every subject had a resting condition)


# -- Split Data Depending on Task Time --
allStudy_data.filt1$Task_Exec_Time.Int <- sapply(allStudy_data.filt1$`Task Execution Time`, 
                                function(x){
                                  which(all_taskTimes == x)
                                  })

pre_post.studies.df <- allStudy_data.filt1 %>% 
  filter(Task_Exec_Time.Int %in% c(pre_post.idx, pre_during_post.idx))


during.studies.df <- allStudy_data.filt1 %>% 
  filter(Task_Exec_Time.Int %in% c(during_post.idx,during.idx,pre_during_post.idx))

post.studies.df <- allStudy_data.filt1 %>% 
  filter(Task_Exec_Time.Int %in% c(during_post.idx,post.idx))

pre.studies.df <- allStudy_data.filt1 %>% 
  filter(Task_Exec_Time.Int %in% pre.idx)

## ---- Pre Post Data ----
print('Calculating Pre/Post Effects...')

studies_missing.data <- c()

pre_post_effects.df <- data.frame()

pre_post_acsmIntensities <- allStudy.acsmIntensities.filt1[which(allStudy_data.filt1$Task_Exec_Time.Int %in%
                                                                   c(pre_post.idx,pre_during_post.idx)),]

for (iRow in seq(nrow(pre_post.studies.df))){
  
  study_data <- pre_post.studies.df[iRow,]
  
  # -- Study Characteristics --
  study_characteristics <- extract_studyCharacteristics(study_data,allExLevel.cols)
  
  condLevels <- study_characteristics$condLevels
  n_condLevels <- study_characteristics$num_condLevels
  condLevels.n <- study_characteristics$condLevels.n
  design <- study_characteristics$design
  
  if (any(is.na(condLevels.n)) && design == 'within'){
    condLevels.n <- rep(study_data$N, n_condLevels)
  } else if (any(is.na(condLevels.n)) && design == 'between'){
    studies_missing.data <- append(studies_missing.data,study_data$`Article ID`)
  }
  
  # use the task time to grab the correct means and sds
  task_time <- str_split(study_characteristics$task_time,';')[[1]]
  
  pre_measures.idx <- grepl('pre|baseline',task_time)
  post_measures.idx <- grepl('post',task_time)
  
  # -- Compute Study Effect Sizes --
  
  # -- Special Cases --
  if (study_data$`Article ID` == 13){
    # Task 1
    pre_post_diff.1 <- (as.numeric(study_data$`Ex Level 2 Mean...39`[[1]]) - as.numeric(study_data$`Ex Level 1 Mean...37`[[1]])) 
    
    pre.sd.1 <- as.numeric(study_data$`Ex Level 1 SD...38`[[1]])
    
    standardized.diff.1 <- pre_post_diff.1 / pre.sd.1
    diff.var.1 <- es.var(design, condLevels.n[[2]], condLevels.n[[2]], standardized.diff.1)
    
    # Task 2
    pre_post_diff.2 <- (as.numeric(study_data$`Ex Level 2 Mean...53`[[1]]) - as.numeric(study_data$`Ex Level 1 Mean...51`[[1]]))
    
    pre.sd.2 <- as.numeric(study_data$`Ex Level 1 SD...52`[[1]])
    
    standardized.diff.2 <- pre_post_diff.2/  pre.sd.2
    
    diff.var.2 <- es.var(design, condLevels.n[[2]], condLevels.n[[2]], standardized.diff.2)
    
    standardized.diff <- c(standardized.diff.1, standardized.diff.2)
    standardized.diff_se <- sqrt(c(diff.var.1, diff.var.2))
    
    post.labels <- c('post','post')
    task_name.idx <- c(34,34)
    study.n <- c(condLevels.n[[2]],condLevels.n[[2]])
    ex_levelName.idx <- c(17,17)
    rest_levelName.idx <- c(13,13)
    
    
    a <- ((study.n + study.n)^2)/(study.n * study.n)
    corr.es <- standardized.diff / sqrt((standardized.diff^2) + a)
    
    corr.se <- ((a^2)*standardized.diff_se)/(((standardized.diff^2) + a)^3)
    
    study_effects.mat <- cbind(ex_levelName.idx,rest_levelName.idx,study.n,task_name.idx,standardized.diff,
                                 standardized.diff_se,post.labels, corr.es, corr.se)
    
  } else if (study_data$`Article ID` == 79) {
    
    # reported change scores
    
    allRest_mean.cols_idx <- allTask_mean.cols[seq(1,length(allTask_mean.cols), by = 5)]
    allEx_mean.cols_idx <- setdiff(allTask_mean.cols,allRest_mean.cols_idx)
    
    # grab indices of columns with data
    rest_mean.cols_bool <- sapply(study_data[allRest_mean.cols_idx], 
                                  function(x){
                                    !any(c(is.na(x),is.null(x[[1]])))
                                  })
    
    ex_mean.cols_bool <- sapply(study_data[allEx_mean.cols_idx],
                                function(x){
                                  !any(c(is.na(x),is.null(x[[1]])))
                                })
    
    rest_taskMean.cols_idx <- allRest_mean.cols_idx[rest_mean.cols_bool]
    
    ex_taskMean.cols_idx <- allEx_mean.cols_idx[ex_mean.cols_bool]
    
    ex.means <- unlist(study_data[ex_taskMean.cols_idx])
    
    rest.means <- unlist(study_data[rest_taskMean.cols_idx])
    
    rest.sds <- unlist(study_data[rest_taskMean.cols_idx+1])
    
    standardized.diff <- (as.numeric(ex.means)-as.numeric(rest.means))/as.numeric(rest.sds)
    
    study.n <- condLevels.n[[2]]
    
    diff.var <- es.var(design,study.n,study.n,standardized.diff)
    
    standardized.diff_se <- sqrt(diff.var)
    
    post.labels <- rep('post',length(standardized.diff))
    task_name.idx <- rest_taskMean.cols_idx-2
    study.n <- rep(study.n, length(post.labels))
    ex_levelName.idx <- rep(17,length(post.labels))
    rest_levelName.idx <- rep(13, length(post.labels))
    
    a <- (study.n + study.n)^2 / (study.n * study.n)
    corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
    corr.se <- ((a^2)*standardized.diff_se) / (((standardized.diff^2) + a)^3)
    
    study_effects.mat <- cbind(ex_levelName.idx,rest_levelName.idx,study.n,task_name.idx,standardized.diff,
                               standardized.diff_se,post.labels, corr.es, corr.se)
    
    
  } else if (study_data$`Article ID` == 51){
    
    # exercise levels 2:4 are pre, post, during
    design <- 'within'
    
    mean_cols.bool <- sapply(study_data[allTask_mean.cols],
                             function(x){
                               !any(c(is.na(x),is.null(x[[1]])))
                             })
    
    mean_cols.idx <- allTask_mean.cols[mean_cols.bool]
    
    
    exLevel.1_meanCols.idx <- intersect(mean_cols.idx,allTask_mean.cols[seq(1,length(allTask_mean.cols),by=5)])
    
    n_outcomes <- length(exLevel.1_meanCols.idx)
    
    non_rest.means_idx <- setdiff(mean_cols.idx, exLevel.1_meanCols.idx)
    
    task_means <- as.numeric(unlist(study_data[non_rest.means_idx]))
    task_sds <- as.numeric(unlist(study_data[non_rest.means_idx+1]))
    
    control_means <- as.numeric(unlist(study_data[exLevel.1_meanCols.idx]))
    control_sds <- as.numeric(unlist(study_data[exLevel.1_meanCols.idx+1]))
    
    w.size <- 2
    w.start <- 1
    control.cnt <- 1
    standardized.diff <- c()
    while(w.start+w.size <= length(task_means)){
      w.end <- w.start + w.size
      
      current_taskMeans <- task_means[w.start:w.end]
      current_taskSd.pre <- control_sds[control.cnt]
      
      smd <- (current_taskMeans - control_means[control.cnt])/current_taskSd.pre
      
      standardized.diff <- append(standardized.diff, smd)
      w.start <- w.end + 1
      control.cnt <- control.cnt + 1
    }
    
    study.n <- condLevels.n[[2]]
    diff.var <- es.var(design,study.n,study.n,standardized.diff)
    
    standardized.diff_se <- sqrt(diff.var)
    
    task_name.idx <- exLevel.1_meanCols.idx-2
    ex_levelName.idx <- rep(17,length(standardized.diff))
    post.labels <- rep('post',length(ex_levelName.idx))
    
    a <- ((study.n + study.n)^2)/(study.n * study.n)
    corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
    
    corr.se <- ((a^2)*standardized.diff_se)/(((standardized.diff^2) + a)^3)
    
    study_effects.mat <- cbind(ex_levelName.idx,ex_levelName.idx, study.n,task_name.idx,standardized.diff,
          standardized.diff_se,post.labels, corr.es, corr.se)
    
  }
  else if (any(grepl('rest', condLevels, ignore.case=T))){ # Studies with rest condition
  
    # rest should always be the first exercise level
    allRest_mean.cols_idx <- allTask_mean.cols[seq(1,length(allTask_mean.cols), by = 5)]
    allEx_mean.cols_idx <- setdiff(allTask_mean.cols,allRest_mean.cols_idx)
    
    # grab indices of columns with data
    rest_mean.cols_bool <- sapply(study_data[allRest_mean.cols_idx], 
                             function(x){
                               !any(c(is.na(x),is.null(x[[1]])))
                             })
    
    ex_mean.cols_bool <- sapply(study_data[allEx_mean.cols_idx],
                                function(x){
                                  !any(c(is.na(x),is.null(x[[1]])))
                                })
    
    if(!any(rest_mean.cols_bool) || !any(ex_mean.cols_bool)){
      studies_missing.data <- append(studies_missing.data,study_data$`Article ID`)
      next
    }
    
    rest_taskMean.cols_idx <- allRest_mean.cols_idx[rest_mean.cols_bool]
  
    ex_taskMean.cols_idx <- allEx_mean.cols_idx[ex_mean.cols_bool]
    
    
    rest_taskSd.cols_idx <- rest_taskMean.cols_idx+1
    ex_taskSd.cols_idx <- ex_taskMean.cols_idx+1
    
    
    rest_con.n <- unlist(condLevels.n[grepl('rest',condLevels,ignore.case = T)])
    exLevels.n <- condLevels.n[!grepl('rest',condLevels,ignore.case = T)]
    
    n_outcomes <- length(rest_taskMean.cols_idx)
    
    # for studies with multiple post tests
    n_postMeasures <- length(which(post_measures.idx))
    
    
    study_effects.mat <- c()
    # loop through each exercise measure
    for (iCon in 1:length(ex_taskMean.cols_idx)){
      
      current_exMean.idx <- ex_taskMean.cols_idx[iCon]
      
      current_restMean.idx <- rest_taskMean.cols_idx[which.min(abs(rest_taskMean.cols_idx-current_exMean.idx))]
      
      ex_taskMeans <- unlist(study_data[current_exMean.idx])
      
      rest_taskMeans <- unlist(study_data[current_restMean.idx])
      
      ex_taskSds <- unlist(study_data[current_exMean.idx+1])
      
      rest_taskSds <- unlist(study_data[current_restMean.idx+1])
      
      # current ex level sample size
      rest_levelName <- str_split(names(rest_taskMeans), ' Mean')[[1]][1]
      ex_levelName <- str_split(names(ex_taskMeans),' Mean')[[1]][1]
      ex_con.n <- unlist(condLevels.n[grepl(ex_levelName,names(condLevels))])
      
      # incase n not present
      if (any(c(is.na(ex_con.n), is.null(ex_con.n))) && design == 'within'){
        ex_con.n <- rest_con.n
      }
      
      # means
      ex_taskMeans.split <- as.numeric(str_split(ex_taskMeans,';')[[1]])
      ex_taskMeans.pre <- ex_taskMeans.split[pre_measures.idx]
      ex_taskMeans.post <- ex_taskMeans.split[post_measures.idx]
      
      rest_taskMeans.split <- as.numeric(str_split(rest_taskMeans,';')[[1]])
      rest_taskMeans.pre <- rest_taskMeans.split[pre_measures.idx]
      rest_taskMeans.post <- rest_taskMeans.split[post_measures.idx]
      
      # sds
      ex_taskSds.split <- as.numeric(str_split(ex_taskSds,';')[[1]])
      ex_taskSds.pre <- ex_taskSds.split[pre_measures.idx]
      ex_taskSds.post <- ex_taskSds.split[post_measures.idx]
      
      rest_taskSds.split <- as.numeric(str_split(rest_taskSds,';')[[1]])
      rest_taskSds.pre <- rest_taskSds.split[pre_measures.idx]
      rest_taskSds.post <- rest_taskSds.split[post_measures.idx]
      
      pooled_pre.sd <- wt.pooled_sd(c(rest_taskSds.pre, ex_taskSds.pre),
                                    c(rest_con.n, ex_con.n))
      # Pre/Post differences
      mean_diff.rest <- rest_taskMeans.post - rest_taskMeans.pre
      mean_diff.ex <- ex_taskMeans.post - ex_taskMeans.pre
      
      standardized.diff <- (mean_diff.ex - mean_diff.rest)/pooled_pre.sd
      
      diff.var <- sapply(standardized.diff,
                         function(x){
                           es.var(design, rest_con.n, ex_con.n, x)
                         })
      standardized.diff_se <- sqrt(diff.var)
      
      if (design == 'between'){
        study.n <- rest_con.n + ex_con.n
      } else{
        study.n <-  rest_con.n
      }
      
      
      # additional data
      study.n <-  rep(study.n, length(standardized.diff))
      task_name.idx <- current_restMean.idx-2
      task_name.idx <- rep(task_name.idx, length(standardized.diff))
      
      post.labels <- task_time[post_measures.idx]
      
      rest_levelName.idx <- which(names(study_data) == rest_levelName)
      ex_levelName.idx <- which(names(study_data) == ex_levelName)
      
      a <- ((rest_con.n + ex_con.n)^2) / (rest_con.n*ex_con.n)
      corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
      corr.se <- ((a^2)*standardized.diff_se) / (((standardized.diff^2) + a)^3)
      
      # store effects
      current_taskEffects <- cbind(ex_levelName.idx, rest_levelName.idx, study.n,task_name.idx,standardized.diff,
                                   standardized.diff_se,post.labels, corr.es, corr.se)
      
      study_effects.mat <- rbind(study_effects.mat, current_taskEffects)
    }
    
    
  } else {
    
    # for studies without the resting condition, just compute (post.mean-pre.mean)/pre.sd
    
    # since we are only doing pre/post effects, design becomes within
    design <- 'within'
    
    mean_cols.bool <- sapply(study_data[allTask_mean.cols],
                            function(x){
                              !any(c(is.na(x),is.null(x[[1]])))
                            })
    
    mean_cols.idx <- allTask_mean.cols[mean_cols.bool]
    
    
    exLevel.1_meanCols.idx <- intersect(mean_cols.idx,allTask_mean.cols[seq(1,length(allTask_mean.cols),by=5)])
    
    n_outcomes <- length(exLevel.1_meanCols.idx)
    
    task_means <- unlist(study_data[mean_cols.idx])
    task_sds <- unlist(study_data[mean_cols.idx+1])
    
    # loop through each measure
    study_effects.mat <- c()
    for (iMeasure in 1:length(task_means)){
      
      current_means <- task_means[iMeasure]
      current_sds <- task_sds[iMeasure]
      
      # extract pre/post measures
      pre.mean <- as.numeric(str_split(current_means,';')[[1]][pre_measures.idx])
      
      pre.sd <- as.numeric(str_split(current_sds,';')[[1]][pre_measures.idx])
      
      post.means <- as.numeric(str_split(current_means,';')[[1]][post_measures.idx])
      
      post.sds <- as.numeric(str_split(current_sds,';')[[1]][post_measures.idx])
      
      ex_levelName <- str_split(names(current_means),' Mean')[[1]][1]
      study.n <- unlist(condLevels.n[grepl(ex_levelName,names(condLevels.n))])
      
      # pre/post diff
      standardized.diff <- (post.means-pre.mean)/pre.sd
      
      # if doing only pre/post diff, then design is within
      diff.var <-  es.var(design, study.n, study.n, standardized.diff)
                        
      standardized.diff_se <- sqrt(diff.var)
      
      # additional data
      study.n <-  rep(study.n, length(standardized.diff))
      
      current_col.idx <- mean_cols.idx[iMeasure]
      
      task_name.idx <- exLevel.1_meanCols.idx[which.min(abs(exLevel.1_meanCols.idx-current_col.idx))]-2
      task_name.idx <- rep(task_name.idx, length(standardized.diff))
      
      post.labels <- task_time[post_measures.idx]
      
      ex_levelName.idx <- which(names(study_data) == ex_levelName)
      
      a <- 4 # since within subjects have equal sample sizes
      corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
      corr.se <- ((a^2)*standardized.diff_se)/(((standardized.diff^2) + a)^3)
      
      # store effects
      current_taskEffects <- cbind(ex_levelName.idx,ex_levelName.idx, study.n,task_name.idx,standardized.diff,
                                   standardized.diff_se,post.labels, corr.es, corr.se)
      
      study_effects.mat <- rbind(study_effects.mat, current_taskEffects)
      
    }
  }
  
  # store study effects in dataframe
  task_names <- unlist(study_data[as.numeric(study_effects.mat[,4])])
  task_domains <- unlist(study_data[(as.numeric(study_effects.mat[,4])-1)])
  
  # incase task name was not put in (happens when task does not change between entries)
  if (any(is.na(task_names), sapply(task_names,is.null))){
    blank_name.idx <- which((is.na(task_names)) | sapply(task_names,is.null))
    task_names[blank_name.idx] <- task_names[min(blank_name.idx)-1]
  }
  
  if (any(is.na(task_domains), sapply(task_domains,is.null))){
    blank_name.idx <- which((is.na(task_domains)) | sapply(task_domains,is.null))
    task_domains[blank_name.idx] <- task_domains[min(blank_name.idx)-1]
  }
  
  # additional study information
  
  ids <- rep(study_data$`Article ID`,nrow(study_effects.mat))
  
  author <- rep(study_data$`First Author`,nrow(study_effects.mat))
  
  year <- rep(study_data$Date, nrow(study_effects.mat))
  
  expNum <- rep(study_data$`Exp #`, nrow(study_effects.mat))
  
  dependent_variables <- unlist(study_data[(as.numeric(study_effects.mat[,4])+1)])
  
  condLevels.idx <- as.numeric(study_effects.mat[,1])
  
  cond1.Names <- unlist(study_data[condLevels.idx])
  
  
  # Use Ex Level Number and Row number to get ACSM Intensities
  cond1.acsm_intensity.idx <- unlist(lapply(names(cond1.Names), 
                                            function(x){
                                              which(grepl(paste(x,'ACSM', sep=' '),names(pre_post_acsmIntensities[iRow,])))
                                            }))
  cond1.acsm_intensities <- unlist(pre_post_acsmIntensities[iRow, cond1.acsm_intensity.idx])
  
  
  # exercise condition comparison was made with
  cond2.Names <- unlist(study_data[as.numeric(study_effects.mat[,2])])
  
  
  cond2.acsm_intensity.idx <- unlist(lapply(names(cond2.Names), 
                                            function(x){
                                              which(grepl(paste(x,'ACSM', sep=' '),names(pre_post_acsmIntensities[iRow,])))
                                            }))
  cond2.acsm_intensities <- unlist(pre_post_acsmIntensities[iRow, cond2.acsm_intensity.idx])
  

  cond.Intensities <- unlist(study_data[(condLevels.idx+1)])
  
  cond.Durations <- unlist(study_data[(condLevels.idx+2)])
                 
  # incase duration info not available     
  if (is.null(cond.Durations[[1]])){
    cond.Durations <- NA
  }
  
  ex.mode <- rep(study_data$`Ex Mode`,nrow(study_effects.mat))
  
  ex.type <- rep(study_data$`Aerobic/Anaerobic`,nrow(study_effects.mat))
  
  exp.design <- rep(design,nrow(study_effects.mat))
  
  exp.task_time <- rep(study_data$`Task Execution Time`,nrow(study_effects.mat))
  
  effect.time <- study_effects.mat[,7]
  
  overall.n <- rep(study_data$N, nrow(study_effects.mat))
  
  if(any(study_effects.mat[,9] < 0)){
    foo <- 0
  }
  
  
  study_effects.df <- data.frame('ID'= ids,
                                 'Author'= author,
                                 'Year' = year,
                                 'expNum' = expNum,
                                 'expDesign' = exp.design,
                                 'Overall_N' = overall.n,
                                 'TaskTime' = exp.task_time,
                                 'EffectTime' = effect.time,
                                 'Ex.Condition'= cond1.Names,
                                 'Ex.ACSM' = cond1.acsm_intensities,
                                 'Compare.Condition' = cond2.Names,
                                 'Compare.ACSM' = cond2.acsm_intensities,
                                 'Ex.Type' = ex.type,
                                 'Ex.Mode' = ex.mode,
                                 'Intensity'= cond.Intensities,
                                 'Duration'= cond.Durations,
                                 'Domain' = task_domains,
                                 'Task' = task_names,
                                 'DV' = dependent_variables,
                                 'SMD' = as.numeric(study_effects.mat[,5]),
                                 'SMD_SE' = as.numeric(study_effects.mat[,6]),
                                 'Effect_N' = as.numeric(study_effects.mat[,3]),
                                 'r' = as.numeric(study_effects.mat[,8]),
                                 'r_se' = as.numeric(study_effects.mat[,9])
                                 )
  
  pre_post_effects.df <- rbind(pre_post_effects.df,study_effects.df)
  
}


## ---- During Data ----

# Studies with control (rest) conditions can be combined with pre/post studies. 
# Studies w/o a control condition can only be used for subgroup analyses

print('Calculating During Effects...')
during_effects.df <- c()
during_acsmIntensities <- allStudy.acsmIntensities.filt1[which(allStudy_data.filt1$Task_Exec_Time.Int %in%
                                                                 c(during_post.idx,during.idx,pre_during_post.idx)),]
for (iRow in 1:nrow(during.studies.df)){
  
  study_data <- during.studies.df[iRow,]
  
  # Extract study characteristics
  study_characteristics <- extract_studyCharacteristics(study_data, allExLevel.cols)
  
  condLevels <- study_characteristics$condLevels
  n_condLevels <- study_characteristics$num_condLevels
  condLevels.n <- study_characteristics$condLevels.n
  design <- study_characteristics$design
  
  if (any(is.na(condLevels.n)) && design == 'within'){
    condLevels.n <- rep(study_data$N, n_condLevels)
    
  } else if (any(is.na(condLevels.n)) && design == 'between'){
    studies_missing.data <- append(studies_missing.data,study_data$`Article ID`)
  }
  
  # use the task time to grab the correct means and sds
  task_time <- str_split(study_characteristics$task_time,';')[[1]]
  
  during_measures.idx <- grepl('during',task_time)
  
  # there needs to be a comparison condition for calculating during effects
  # studies with pre, during, post designs, calcuate during-pre effects separately
  if (length(condLevels) < 2){
    next
  }
  
  
  # -- Compute Effect Sizes ---
  
  # Special Cases
  if ((study_data$`Article ID` == 51) || (study_data$`Article ID` == 13)){
    next
  } else if (any(grepl('rest|control',condLevels, ignore.case = T))){
    
    # -- Studies w/ control condition --
    
    allRest_mean.cols_idx <- allTask_mean.cols[seq(1,length(allTask_mean.cols), by = 5)]
    allEx_mean.cols_idx <- setdiff(allTask_mean.cols,allRest_mean.cols_idx)
    
    # grab indices of columns with data
    rest_mean.cols_bool <- sapply(study_data[allRest_mean.cols_idx], 
                                  function(x){
                                    !any(c(is.na(x),is.null(x[[1]])))
                                  })
    
    ex_mean.cols_bool <- sapply(study_data[allEx_mean.cols_idx],
                                function(x){
                                  !any(c(is.na(x),is.null(x[[1]])))
                                })
    
    if(!any(rest_mean.cols_bool) || !any(ex_mean.cols_bool)){
      studies_missing.data <- append(studies_missing.data,study_data$`Article ID`)
      next
    }
    
    rest_taskMean.cols_idx <- allRest_mean.cols_idx[rest_mean.cols_bool]
    
    ex_taskMean.cols_idx <- allEx_mean.cols_idx[ex_mean.cols_bool]
    
    
    rest_taskSd.cols_idx <- rest_taskMean.cols_idx+1
    ex_taskSd.cols_idx <- ex_taskMean.cols_idx+1
    
    
    rest_con.n <- unlist(condLevels.n[grepl('rest|control',condLevels,ignore.case = T)])
    exLevels.n <- condLevels.n[!grepl('rest|control',condLevels,ignore.case = T)]
    
    n_outcomes <- length(rest_taskMean.cols_idx)
    
    study_effects.mat <- c()
    for (iCon in 1:length(ex_taskMean.cols_idx)){
      current_exMean.idx <- ex_taskMean.cols_idx[iCon]
      
      current_restMean.idx <- rest_taskMean.cols_idx[which.min(abs(rest_taskMean.cols_idx-current_exMean.idx))]
      
      ex_taskMeans <- unlist(study_data[current_exMean.idx])
      
      rest_taskMeans <- unlist(study_data[current_restMean.idx])
      
      ex_taskSds <- unlist(study_data[current_exMean.idx+1])
      
      rest_taskSds <- unlist(study_data[current_restMean.idx+1])
      
      
      # means
      ex_taskMeans.split <- as.numeric(str_split(ex_taskMeans,';')[[1]])
      ex_taskMeans.during <- ex_taskMeans.split[during_measures.idx]
      
      rest_taskMeans.split <- as.numeric(str_split(rest_taskMeans,';')[[1]])
      rest_taskMeans.during <- rest_taskMeans.split[during_measures.idx]
      
      # sds 
      ex_taskSds.split <- as.numeric(str_split(ex_taskSds,';')[[1]])
      ex_taskSds.during <- ex_taskSds.split[during_measures.idx]
      
      rest_taskSds.split <- as.numeric(str_split(rest_taskSds,';')[[1]])
      rest_taskSds.during <- rest_taskSds.split[during_measures.idx]
      
      # current ex level sample size
      rest_levelName <- str_split(names(rest_taskMeans), ' Mean')[[1]][1]
      ex_levelName <- str_split(names(ex_taskMeans),' Mean')[[1]][1]
      ex_con.n <- unlist(condLevels.n[grepl(ex_levelName,names(condLevels))])
      
      # incase n not present
      if (any(c(is.na(ex_con.n), is.null(ex_con.n))) && design == 'within'){
        ex_con.n <- rest_con.n
      }
      
      # Ex - Rest Mean diff
      
      mean.diff <- ex_taskMeans.during - rest_taskMeans.during
      
      # divide by pooled exercise and rest sd
      if (design=='between'){
        pooled.sd <- wt.pooled_sd(c(rest_taskSds.during, ex_taskSds.during),
                                  c(rest_con.n, ex_con.n))
        
        study.n <- rest_con.n + ex_con.n
      } else {
        assumed.cor <- .6
        sum_term <- sum(c(rest_taskSds.during, ex_taskSds.during)^2)
        prod_term <- 2 * assumed.cor * rest_taskSds.during * ex_taskSds.during
        
        s_diff <- sqrt(sum_term-prod_term)
        
        pooled.sd <- s_diff/sqrt(2*(1-assumed.cor))
        
        study.n <-  rest_con.n
      }
      
      standardized.diff <- mean.diff/pooled.sd
      
      diff.var <- es.var(design,rest_con.n,ex_con.n,standardized.diff)
      
      standardized.diff_se <- sqrt(diff.var)
      
      a <- ((rest_con.n + ex_con.n)^2) / (rest_con.n*ex_con.n)
      corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
      corr.se <- (a^2)*standardized.diff_se / (((standardized.diff^2) + a)^3)
      
      # additional data
      study.n <-  rep(study.n, length(standardized.diff))
      task_name.idx <- current_restMean.idx-2
      task_name.idx <- rep(task_name.idx, length(standardized.diff))
      
      during.labels <- task_time[during_measures.idx]
      
      rest_levelName.idx <- which(names(study_data) == rest_levelName)
      ex_levelName.idx <- which(names(study_data) == ex_levelName)
      
      # store effects
      current_taskEffects <- cbind(ex_levelName.idx,rest_levelName.idx,study.n,task_name.idx,
                                   standardized.diff,standardized.diff_se,during.labels,
                                   corr.es, corr.se)
      
      study_effects.mat <- rbind(study_effects.mat, current_taskEffects)
      
    }
    
    
    
    
    
  } else { 
    
    # Studies w/o control condition
    
    mean_cols.bool <- sapply(study_data[allTask_mean.cols],
                             function(x){
                               !any(c(is.na(x),is.null(x[[1]])))
                             })
    
    mean_cols.idx <- allTask_mean.cols[mean_cols.bool]
    
    
    exLevel.1_meanCols.idx <- intersect(mean_cols.idx,allTask_mean.cols[seq(1,length(allTask_mean.cols),by=5)])
    
    n_outcomes <- length(exLevel.1_meanCols.idx)
    
    task_means <- unlist(study_data[mean_cols.idx])
    task_sds <- unlist(study_data[mean_cols.idx+1])
    
    
    task_startCols.idx <- seq(1,length(task_means), by = length(condLevels))
    
    
    during.labels <- task_time[during_measures.idx]
    
    # loop through each measure and subtract current mean from all other exercise means
    study_effects.mat <- c()
    
    for (iTask in 1:length(task_startCols.idx)){
      
      
      current_task.end_idx <- task_startCols.idx[iTask] + length(condLevels) - 1
      current_task.idx <- c(task_startCols.idx[iTask]:current_task.end_idx)
      
      current_task.means <- task_means[current_task.idx]
      current_task.sds <- task_sds[current_task.idx]
      
      task_name.idx <- exLevel.1_meanCols.idx[iTask]-2
      
      for (iMeasure in 1:(length(current_task.means)-1)){
        
        current_con.means <- current_task.means[iMeasure]
        current_con.sds <- current_task.sds[iMeasure]
        
        current_con.means.split <- as.numeric(unlist(str_split(current_con.means,';')[[1]]))
        current_con.sds.split <- as.numeric(unlist(str_split(current_con.sds,';')[[1]]))
        
        current_con.means.during <- current_con.means.split[during_measures.idx]
        current_con.sds.during <- current_con.sds.split[during_measures.idx]
        
        current_con.name_idx <- which(names(study_data) == names(condLevels)[iMeasure])
        
        current_con.n <- unlist(condLevels.n[iMeasure])
        
        for (jMeasure in (iMeasure+1):length(current_task.means)){
            
          comp_con.means <- task_means[jMeasure]
          comp_con.sds <- task_sds[jMeasure]
          
          comp_con.means.split <- as.numeric(unlist(str_split(comp_con.means,';')[[1]]))
          comp_con.sds.split <- as.numeric(unlist(str_split(comp_con.sds,';')[[1]]))
          
          comp_con.means.during <- comp_con.means.split[during_measures.idx]
          comp_con.sds.during <- comp_con.sds.split[during_measures.idx]
          
          comp_con.n <- unlist(condLevels.n[jMeasure])
          comp_con.name_idx <- which(names(study_data) == names(condLevels)[jMeasure])
          
          mean.diff <- current_con.means.during - comp_con.means.during
          
          # divide by pooled standard deviation
          if (design=='between'){
            pooled.sd <- wt.pooled_sd(c(current_con.sds.split, comp_con.sds.split),
                                      c(current_con.n, comp_con.n))
            
            study.n <- current_con.n + comp_con.n
          } else {
            assumed.cor <- .6
            sum_term <- sum(c(current_con.sds.during, comp_con.sds.during)^2)
            prod_term <- 2 * assumed.cor * current_con.sds.during * comp_con.sds.during
            
            s_diff <- sqrt(sum_term-prod_term)
            
            pooled.sd <- s_diff/sqrt(2*(1-assumed.cor))
            
            study.n <-  current_con.n
          }
          
          
          standardized.diff <- mean.diff/pooled.sd
          
          diff.var <- es.var(design,current_con.n,comp_con.n,standardized.diff)
          
          standardized.diff_se <- sqrt(diff.var)
          
          a <- ((current_con.n + comp_con.n)^2) / (current_con.n * comp_con.n)
          corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
          corr.se <- ((a^2)*standardized.diff_se) / (((standardized.diff^2) + a)^3)
          
          
          # store effects
          current_taskEffects <- cbind(current_con.name_idx,comp_con.name_idx, study.n,
                                       task_name.idx,standardized.diff, standardized.diff_se,during.labels,
                                       corr.es, corr.se)
          
          study_effects.mat <- rbind(study_effects.mat, current_taskEffects)
        
          
        }
        
      }
    }
    
    
  }
  
  
  
  # -- store study effects in dataframe --
  task_names <- unlist(study_data[as.numeric(study_effects.mat[,4])])
  task_domains <- unlist(study_data[(as.numeric(study_effects.mat[,4])-1)])
  
  # incase task name was not put in (happens when task does not change between entries)
  if (any(is.na(task_names), sapply(task_names,is.null))){
    blank_name.idx <- which((is.na(task_names)) | sapply(task_names,is.null))
    task_names[blank_name.idx] <- task_names[min(blank_name.idx)-1]
  }
  
  if (any(is.na(task_domains), sapply(task_domains,is.null))){
    blank_name.idx <- which((is.na(task_domains)) | sapply(task_domains,is.null))
    task_domains[blank_name.idx] <- task_domains[min(blank_name.idx)-1]
  }
  
  # additional study information
  
  ids <- rep(study_data$`Article ID`,nrow(study_effects.mat))
  
  author <- rep(study_data$`First Author`,nrow(study_effects.mat))
  
  year <- rep(study_data$Date, nrow(study_effects.mat))
  
  expNum <- rep(study_data$`Exp #`, nrow(study_effects.mat))
  
  dependent_variables <- unlist(study_data[(as.numeric(study_effects.mat[,4])+1)])
  
  condLevels.idx <- as.numeric(study_effects.mat[,1])
  
  cond1.Names <- unlist(study_data[condLevels.idx])
  
  # Use Ex Level Number and Row number to get ACSM Intensities
  cond1.acsm_intensity.idx <- unlist(lapply(names(cond1.Names), 
                                            function(x){
                                              which(grepl(paste(x,'ACSM', sep=' '),names(during_acsmIntensities[iRow,])))
                                            }))
  cond1.acsm_intensities <- unlist(during_acsmIntensities[iRow, cond1.acsm_intensity.idx])
  
  
  # exercise condition comparison was made with
  cond2.Names <- unlist(study_data[as.numeric(study_effects.mat[,2])])
  
  
  cond2.acsm_intensity.idx <- unlist(lapply(names(cond2.Names), 
                                            function(x){
                                              which(grepl(paste(x,'ACSM', sep=' '),names(during_acsmIntensities[iRow,])))
                                            }))
  cond2.acsm_intensities <- unlist(during_acsmIntensities[iRow, cond2.acsm_intensity.idx])
  
  cond.Intensities <- unlist(study_data[(condLevels.idx+1)])
  
  # incase duration info not available     
  if (is.null(cond.Intensities[[1]])){
    cond.Intensities <- NA
  }
  
  cond.Durations <- unlist(study_data[(condLevels.idx+2)])
  
  # incase duration info not available     
  if (is.null(cond.Durations[[1]])){
    cond.Durations <- NA
  }
  
  
  ex.mode <- rep(study_data$`Ex Mode`,nrow(study_effects.mat))
  
  ex.type <- rep(study_data$`Aerobic/Anaerobic`,nrow(study_effects.mat))
  
  exp.design <- rep(design,nrow(study_effects.mat))
  
  exp.task_time <- rep(study_data$`Task Execution Time`,nrow(study_effects.mat))
  
  effect.time <- study_effects.mat[,7]
  
  overall.n <- rep(study_data$N, nrow(study_effects.mat))
  
  if(any(study_effects.mat[,9] < 0)){
    foo <- 0
  }
  
  study_effects.df <- data.frame('ID'= ids,
                                 'Author'= author,
                                 'Year' = year,
                                 'expNum' = expNum,
                                 'expDesign' = exp.design,
                                 'Overall_N' = overall.n,
                                 'TaskTime' = exp.task_time,
                                 'EffectTime' = effect.time,
                                 'Ex.Condition'= cond1.Names,
                                 'Ex.ACSM' = cond1.acsm_intensities,
                                 'Compare.Condition' = cond2.Names,
                                 'Compare.ACSM' = cond2.acsm_intensities,
                                 'Ex.Type' = ex.type,
                                 'Ex.Mode' = ex.mode,
                                 'Intensity'= cond.Intensities,
                                 'Duration'= cond.Durations,
                                 'Domain' = task_domains,
                                 'Task' = task_names,
                                 'DV' = dependent_variables,
                                 'SMD' = as.numeric(study_effects.mat[,5]),
                                 'SMD_SE' = as.numeric(study_effects.mat[,6]),
                                 'Effect_N' = as.numeric(study_effects.mat[,3]),
                                 'r' = as.numeric(study_effects.mat[,8]),
                                 'r_se' = as.numeric(study_effects.mat[,9])
                                 )
  
  during_effects.df <- rbind(during_effects.df, study_effects.df)
  
}


## ---- Post Data ----


print('Calculating Post Effects...')

post_effects.df <- c()
post_acsmIntensities <- allStudy.acsmIntensities.filt1[which(allStudy_data.filt1$Task_Exec_Time.Int %in%
                                                               c(during_post.idx,post.idx)),]
for (iRow in 1:nrow(post.studies.df)){
  
  study_data <- post.studies.df[iRow,]
  
  # Extract study characteristics
  study_characteristics <- extract_studyCharacteristics(study_data, allExLevel.cols)
  
  condLevels <- study_characteristics$condLevels
  n_condLevels <- study_characteristics$num_condLevels
  condLevels.n <- study_characteristics$condLevels.n
  design <- study_characteristics$design
  
  if (any(is.na(condLevels.n)) && design == 'within'){
    condLevels.n <- rep(study_data$N, n_condLevels)
    
  } else if (any(is.na(condLevels.n)) && design == 'between'){
    studies_missing.data <- append(studies_missing.data,study_data$`Article ID`)
  }
  
  # use the task time to grab the correct means and sds
  task_time <- str_split(study_characteristics$task_time,';')[[1]]
  
  post_measures.idx <- grepl('post',task_time)
  
  # needs to be a comparison condition for studies with just post effects
  if (length(condLevels) < 2){
    next
  }
  
  # -- Special Cases -- 
  if (study_data$`Article ID` == 24){
    next
  } else if (any(grepl('rest|control',condLevels, ignore.case = T))){
    
    # -- Studies w/ control condition --
    
    allRest_mean.cols_idx <- allTask_mean.cols[seq(1,length(allTask_mean.cols), by = 5)]
    allEx_mean.cols_idx <- setdiff(allTask_mean.cols,allRest_mean.cols_idx)
    
    # grab indices of columns with data
    rest_mean.cols_bool <- sapply(study_data[allRest_mean.cols_idx], 
                                  function(x){
                                    !any(c(is.na(x),is.null(x[[1]])))
                                  })
    
    ex_mean.cols_bool <- sapply(study_data[allEx_mean.cols_idx],
                                function(x){
                                  !any(c(is.na(x),is.null(x[[1]])))
                                })
    
    if(!any(rest_mean.cols_bool) || !any(ex_mean.cols_bool)){
      studies_missing.data <- append(studies_missing.data,study_data$`Article ID`)
      next
    }
    
    rest_taskMean.cols_idx <- allRest_mean.cols_idx[rest_mean.cols_bool]
    
    ex_taskMean.cols_idx <- allEx_mean.cols_idx[ex_mean.cols_bool]
    
    
    rest_taskSd.cols_idx <- rest_taskMean.cols_idx+1
    ex_taskSd.cols_idx <- ex_taskMean.cols_idx+1
    
    
    rest_con.n <- unlist(condLevels.n[grepl('rest|control',condLevels,ignore.case = T)])
    exLevels.n <- condLevels.n[!grepl('rest|control',condLevels,ignore.case = T)]
    
    n_outcomes <- length(rest_taskMean.cols_idx)
    
    study_effects.mat <- c()
    for (iCon in 1:length(ex_taskMean.cols_idx)){
      current_exMean.idx <- ex_taskMean.cols_idx[iCon]
      
      current_restMean.idx <- rest_taskMean.cols_idx[which.min(abs(rest_taskMean.cols_idx-current_exMean.idx))]
      
      ex_taskMeans <- unlist(study_data[current_exMean.idx])
      
      rest_taskMeans <- unlist(study_data[current_restMean.idx])
      
      ex_taskSds <- unlist(study_data[current_exMean.idx+1])
      
      rest_taskSds <- unlist(study_data[current_restMean.idx+1])
      
      
      # means
      ex_taskMeans.split <- as.numeric(str_split(ex_taskMeans,';')[[1]])
      ex_taskMeans.post <- ex_taskMeans.split[post_measures.idx]
      
      rest_taskMeans.split <- as.numeric(str_split(rest_taskMeans,';')[[1]])
      rest_taskMeans.post <- rest_taskMeans.split[post_measures.idx]
      
      # sds 
      ex_taskSds.split <- as.numeric(str_split(ex_taskSds,';')[[1]])
      ex_taskSds.post <- ex_taskSds.split[post_measures.idx]
      
      rest_taskSds.split <- as.numeric(str_split(rest_taskSds,';')[[1]])
      rest_taskSds.post <- rest_taskSds.split[post_measures.idx]
      
      # current ex level sample size
      rest_levelName <- str_split(names(rest_taskMeans), ' Mean')[[1]][1]
      ex_levelName <- str_split(names(ex_taskMeans),' Mean')[[1]][1]
      ex_con.n <- unlist(condLevels.n[grepl(ex_levelName,names(condLevels))])
      
      # incase n not present
      if (any(c(is.na(ex_con.n), is.null(ex_con.n))) && design == 'within'){
        ex_con.n <- rest_con.n
      }
      
      # Ex - Rest Mean diff
      
      mean.diff <- ex_taskMeans.post - rest_taskMeans.post
      
      # divide by pooled exercise and rest sd
      if (design=='between'){
        pooled.sd <- wt.pooled_sd(c(rest_taskSds.post, ex_taskSds.post),
                                  c(rest_con.n, ex_con.n))
        
        study.n <- rest_con.n + ex_con.n
      } else {
        assumed.cor <- .6
        sum_term <- sum(c(rest_taskSds.post, ex_taskSds.post)^2)
        prod_term <- 2 * assumed.cor * rest_taskSds.post * ex_taskSds.post
        
        s_diff <- sqrt(sum_term-prod_term)
        
        pooled.sd <- s_diff/sqrt(2*(1-assumed.cor))
        
        study.n <-  rest_con.n
      }
      
      standardized.diff <- mean.diff/pooled.sd
      
      diff.var <- es.var(design,rest_con.n,ex_con.n,standardized.diff)
      
      standardized.diff_se <- sqrt(diff.var)
      
      a <- ((rest_con.n + ex_con.n)^2) / (rest_con.n*ex_con.n)
      corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
      corr.se <- ((a^2)*standardized.diff_se) / (((standardized.diff^2) + a)^3)
      
      
      # additional data
      study.n <-  rep(study.n, length(standardized.diff))
      task_name.idx <- current_restMean.idx-2
      task_name.idx <- rep(task_name.idx, length(standardized.diff))
      
      post.labels <- task_time[post_measures.idx]
      
      rest_levelName.idx <- which(names(study_data) == rest_levelName)
      ex_levelName.idx <- which(names(study_data) == ex_levelName)
      
      # store effects
      current_taskEffects <- cbind(ex_levelName.idx,rest_levelName.idx,study.n,task_name.idx,
                                   standardized.diff,standardized.diff_se,post.labels,
                                   corr.es, corr.se)
      
      study_effects.mat <- rbind(study_effects.mat, current_taskEffects)
      
    } 
  
  } else {
    
    # Studies w/o control condition
    
    mean_cols.bool <- sapply(study_data[allTask_mean.cols],
                             function(x){
                               !any(c(is.na(x),is.null(x[[1]])))
                             })
    
    mean_cols.idx <- allTask_mean.cols[mean_cols.bool]
    
    
    exLevel.1_meanCols.idx <- intersect(mean_cols.idx,allTask_mean.cols[seq(1,length(allTask_mean.cols),by=5)])
    
    n_outcomes <- length(exLevel.1_meanCols.idx)
    
    task_means <- unlist(study_data[mean_cols.idx])
    task_sds <- unlist(study_data[mean_cols.idx+1])
    
    
    task_startCols.idx <- seq(1,length(task_means), by = length(condLevels))
    
    
    post.labels <- task_time[post_measures.idx]
    
    # loop through each measure and subtract current mean from all other exercise means
    study_effects.mat <- c()
    
    for (iTask in 1:length(task_startCols.idx)){
      
      
      current_task.end_idx <- task_startCols.idx[iTask] + length(condLevels) - 1
      current_task.idx <- c(task_startCols.idx[iTask]:current_task.end_idx)
      
      current_task.means <- task_means[current_task.idx]
      current_task.sds <- task_sds[current_task.idx]
      
      task_name.idx <- exLevel.1_meanCols.idx[iTask]-2
      
      for (iMeasure in 1:(length(current_task.means)-1)){
        
        current_con.means <- current_task.means[iMeasure]
        current_con.sds <- current_task.sds[iMeasure]
        
        current_con.means.split <- as.numeric(unlist(str_split(current_con.means,';')[[1]]))
        current_con.sds.split <- as.numeric(unlist(str_split(current_con.sds,';')[[1]]))
        
        current_con.means.post <- current_con.means.split[post_measures.idx]
        current_con.sds.post <- current_con.sds.split[post_measures.idx]
        
        current_con.name_idx <- which(names(study_data) == names(condLevels)[iMeasure])
        
        current_con.n <- unlist(condLevels.n[iMeasure])
        
        for (jMeasure in (iMeasure+1):length(current_task.means)){
          
          comp_con.means <- task_means[jMeasure]
          comp_con.sds <- task_sds[jMeasure]
          
          comp_con.means.split <- as.numeric(unlist(str_split(comp_con.means,';')[[1]]))
          comp_con.sds.split <- as.numeric(unlist(str_split(comp_con.sds,';')[[1]]))
          
          comp_con.means.post <- comp_con.means.split[post_measures.idx]
          comp_con.sds.post <- comp_con.sds.split[post_measures.idx]
          
          comp_con.n <- unlist(condLevels.n[jMeasure])
          comp_con.name_idx <- which(names(study_data) == names(condLevels)[jMeasure])
          
          mean.diff <- current_con.means.post - comp_con.means.post
          
          # divide by pooled standard deviation
          if (design=='between'){
            pooled.sd <- wt.pooled_sd(c(current_con.sds.split, comp_con.sds.split),
                                      c(current_con.n, comp_con.n))
            
            study.n <- current_con.n + comp_con.n
          } else {
            assumed.cor <- .6
            sum_term <- sum(c(current_con.sds.post, comp_con.sds.post)^2)
            prod_term <- 2 * assumed.cor * current_con.sds.post * comp_con.sds.post
            
            s_diff <- sqrt(sum_term-prod_term)
            
            pooled.sd <- s_diff/sqrt(2*(1-assumed.cor))
            
            study.n <-  current_con.n
          }
          
          
          standardized.diff <- mean.diff/pooled.sd
          
          diff.var <- es.var(design,current_con.n,comp_con.n,standardized.diff)
          
          standardized.diff_se <- sqrt(diff.var)
          
          a <- ((current_con.n + comp_con.n)^2) / (current_con.n * comp_con.n)
          corr.es <- standardized.diff / sqrt(standardized.diff^2 + a)
          corr.se <- ((a^2)*standardized.diff_se) / (((standardized.diff^2) + a)^3)
          
          # store effects
          current_taskEffects <- cbind(current_con.name_idx,comp_con.name_idx, study.n,
                                       task_name.idx,standardized.diff, standardized.diff_se,post.labels,
                                       corr.es, corr.se)
          
          study_effects.mat <- rbind(study_effects.mat, current_taskEffects)
          
          
        }
        
      }
    }
    
    
  }
  
  
  # -- store study effects in dataframe --
  task_names <- unlist(study_data[as.numeric(study_effects.mat[,4])])
  task_domains <- unlist(study_data[(as.numeric(study_effects.mat[,4])-1)])
  
  # incase task name was not put in (happens when task does not change between entries)
  if (any(is.na(task_names), sapply(task_names,is.null))){
    blank_name.idx <- which((is.na(task_names)) | sapply(task_names,is.null))
    task_names[blank_name.idx] <- task_names[min(blank_name.idx)-1]
  }
  
  if (any(is.na(task_domains), sapply(task_domains,is.null))){
    blank_name.idx <- which((is.na(task_domains)) | sapply(task_domains,is.null))
    task_domains[blank_name.idx] <- task_domains[min(blank_name.idx)-1]
  }
  
  # additional study information
  
  ids <- rep(study_data$`Article ID`,nrow(study_effects.mat))
  
  author <- rep(study_data$`First Author`,nrow(study_effects.mat))
  
  year <- rep(study_data$Date, nrow(study_effects.mat))
  
  expNum <- rep(study_data$`Exp #`, nrow(study_effects.mat))
  
  dependent_variables <- unlist(study_data[(as.numeric(study_effects.mat[,4])+1)])
  
  condLevels.idx <- as.numeric(study_effects.mat[,1])
  
  cond1.Names <- unlist(study_data[condLevels.idx])
  
  # Use Ex Level Number and Row number to get ACSM Intensities
  cond1.acsm_intensity.idx <- unlist(lapply(names(cond1.Names), 
                                            function(x){
                                              which(grepl(paste(x,'ACSM', sep=' '),names(post_acsmIntensities[iRow,])))
                                            }))
  cond1.acsm_intensities <- unlist(post_acsmIntensities[iRow, cond1.acsm_intensity.idx])
  
  
  # exercise condition comparison was made with
  cond2.Names <- unlist(study_data[as.numeric(study_effects.mat[,2])])
  
  
  cond2.acsm_intensity.idx <- unlist(lapply(names(cond2.Names), 
                                            function(x){
                                              which(grepl(paste(x,'ACSM', sep=' '),names(post_acsmIntensities[iRow,])))
                                            }))
  cond2.acsm_intensities <- unlist(post_acsmIntensities[iRow, cond2.acsm_intensity.idx])
  
  cond.Intensities <- unlist(study_data[(condLevels.idx+1)])
  
  # incase duration info not available     
  if (is.null(cond.Intensities[[1]])){
    cond.Intensities <- NA
  }
  
  cond.Durations <- unlist(study_data[(condLevels.idx+2)])
  
  # incase duration info not available     
  if (is.null(cond.Durations[[1]])){
    cond.Durations <- NA
  }
  
  
  ex.mode <- rep(study_data$`Ex Mode`,nrow(study_effects.mat))
  
  ex.type <- rep(study_data$`Aerobic/Anaerobic`,nrow(study_effects.mat))
  
  exp.design <- rep(design,nrow(study_effects.mat))
  
  exp.task_time <- rep(study_data$`Task Execution Time`,nrow(study_effects.mat))
  
  effect.time <- study_effects.mat[,7]
  
  overall.n <- rep(study_data$N, nrow(study_effects.mat))
  
  study_effects.df <- data.frame('ID'= ids,
                                 'Author'= author,
                                 'Year' = year,
                                 'expNum' = expNum,
                                 'expDesign' = exp.design,
                                 'Overall_N' = overall.n,
                                 'TaskTime' = exp.task_time,
                                 'EffectTime' = effect.time,
                                 'Ex.Condition'= cond1.Names,
                                 'Ex.ACSM' = cond1.acsm_intensities,
                                 'Compare.Condition' = cond2.Names,
                                 'Compare.ACSM' = cond2.acsm_intensities,
                                 'Ex.Type' = ex.type,
                                 'Ex.Mode' = ex.mode,
                                 'Intensity'= cond.Intensities,
                                 'Duration'= cond.Durations,
                                 'Domain' = task_domains,
                                 'Task' = task_names,
                                 'DV' = dependent_variables,
                                 'SMD' = as.numeric(study_effects.mat[,5]),
                                 'SMD_SE' = as.numeric(study_effects.mat[,6]),
                                 'Effect_N' = as.numeric(study_effects.mat[,3]),
                                 'r' = as.numeric(study_effects.mat[,8]),
                                 'r_se' = as.numeric(study_effects.mat[,9])
                                 )
  
  post_effects.df <- rbind(post_effects.df, study_effects.df)
  
  
}





## ---- Combine all studies effects sizes ----
allStudies_effects.df <- rbind(pre_post_effects.df,during_effects.df,post_effects.df)


# recode RT and Error measures (i.e., negative RT effect indicates faster response during exercise. Multiply by negative 1 to make positive)
recode_rows.idx <- which(grepl('RT|Interference|Error',allStudies_effects.df$DV))
allStudies_effects.df$SMD[recode_rows.idx] <- allStudies_effects.df$SMD[recode_rows.idx] *-1
allStudies_effects.df$r[recode_rows.idx] <- allStudies_effects.df$r[recode_rows.idx] * -1

# reindex
rownames(allStudies_effects.df) <- 1:nrow(allStudies_effects.df)
saveRDS(allStudies_effects.df,file='allStudies_effects.rds')
