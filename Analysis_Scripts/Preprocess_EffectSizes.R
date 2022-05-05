library(readxl)
library(tidyverse)
library(esc)

# --- Setting Up Directories ---

parentDir <- getwd()
dataDir <- file.path(parentDir,'Data')
plotDir <- file.path(parentDir,'Figures')


setwd(dataDir)

# Load data
data_file <- 'PACMAN_Calculated Effect Sizes.csv'

allStudies_effects.df <- read.csv(data_file)


allStudies_effects.df$Domain.2 <- sapply(allStudies_effects.df$Cognitive.Domain, 
       function(x){
         if (grepl('perc',x, ignore.case = T)){
           x <- 'Perception'
         } else if (grepl('Information', x, ignore.case = T)){
           x <- 'Information Processing'
         } else if (grepl('Ex|Decision|Inhibition|working|WM|Control|Planning|Flexibility|Switching', 
                          x, ignore.case = T)){
           x <- 'Executive Function'
         } else if (grepl('Ep|Long Term|Learning &|Emotional', x, ignore.case = T)){
           x <- 'Memory'
         } else if (grepl('Motor', x, ignore.case=T)) {
           x <- 'Motor Skills'
         } else {
           x
         }
       })


allStudies_effects.df$Ex.Mode.2 <- sapply(allStudies_effects.df$Ex.Mode,
       function(x){
         if (grepl('cycling ', x, ignore.case = T)){
           x <- 'Cycling'
         } else if (grepl('Treadmill|walking|(treadmill)|Stair',x,ignore.case = F)){
           x <- 'Walking'
         } else if (grepl('jumping|yoga|climb|voll|punching|callis', x, ignore.case = T)){
           x <- 'Sport Activity'
         } else if (grepl('HITT',x,ignore.case = T)){
           x <- 'HITT'
         } else {
           x
         }
       })

allStudies_effects.df$EffectTime <-  sapply(allStudies_effects.df$Task.Execution.Time, 
       function(x){
         if (grepl('post', x, ignore.case = T)){
           time_point <- as.numeric(str_extract(x, '[0-9]{2}'))
           
           # if digit found
           if (!is.na(time_point)){
             if (time_point <= 5){
               x <- 'post'
             } else if (5 > time_point & time_point <= 15){
               x <- 'post ( < 15 min)'
             } else if (75 >= time_point & time_point >= 20) {
               x <- 'post (20-75 min)'
             } else {
               x <- 'post (> 180 min)'
             }
           } else {
             x <- 'post'
           }
           
         } else if (grepl('during', x, ignore.case = T)){
           x <- 'during'
         } else {
           x
         }
       })

# Dependent Measures
allStudies_effects.df$OutcomeVariable <- sapply(allStudies_effects.df$Cognitive.DV, 
                                          function(x){
                                            if (grepl('RT|Interference|Error|Time', x, ignore.case = T)){
                                              x <- 'RT'
                                            } else {
                                              x <- 'Accuracy'
                                            }
                                          })


allStudies_effects.df$Duration.2 <- apply(allStudies_effects.df, 1,  
                                                function(row){
                                                  
                                                  if (any(is.null(row['Ex.Level.Duration..mins.']),
                                                          is.na(row['Ex.Level.Duration..mins.']), 
                                                          row['Ex.Level.Duration..mins.'] != '')){
                                                    x <- tolower(row['Ex.Level.Duration..mins.'])
                                                  } else {
                                                    x <- tolower(row['Compare.Level.Duration..mins.'])
                                                  }
                                                  
                                                  if (grepl('exhaustion', x)){
                                                    t <- 'volitional exhaustion'
                                                  } else if (grepl('completion|game|task duration|duration of task',x)){
                                                    t <- 'task completion'
                                                  } else if (grepl('total 7|3 x 1', x)){
                                                    t <- '<=15'
                                                  } else if (grepl('> 55', x)) {
                                                    t <- '40-45'
                                                  } else if (grepl('sets', x)){
                                                    t <- 'sets duration'
                                                  } else if (grepl('n/a|need to', x)){
                                                    t <- NA
                                                  } else {
                                                    num <- as.numeric(str_extract(x, '[0-9]{1,3}'))
                                                    
                                                    if (!any(is.na(num), is.null(num))){
                                                      
                                                      if (num <= 15){
                                                        t <- '<=15'
                                                      } else if (num >= 20 & num < 30){
                                                        t <- '20-27'
                                                      } else if (num >= 30 & num < 40){
                                                        t <- '30-35'
                                                      } else if (num >= 40 & num <= 45){
                                                        t <- '40-45'
                                                      } else if (num >= 60){
                                                        t <- '>60'
                                                      }
                                                      
                                                    } else {
                                                      x
                                                    }
                                                  }
                                                })

factor_cols <- c('Article.ID', 'Title', 'First.Author', 'Ex.Mode.2', 'Ex.ACSM', 
                 'Domain.2', 'OutcomeVariable')

allStudies_effects.df[factor_cols] <- lapply(allStudies_effects.df[factor_cols], factor)

allStudies_effects.df$Ex.ACSM <- factor(allStudies_effects.df$Ex.ACSM, 
                                        levels = c('Very Light', 'Light', 'Light-Moderate',
                                                   'Moderate','Moderate-Vigorous', 'Vigorous', 'Maximal'))

allStudies_effects.df$Duration.2 <- factor(allStudies_effects.df$Duration.2, 
                                           levels=c('<=15', '20-27', '30-35', '40-45', '>60',
                                           'task completion', 'volitional exhaustion', 'sets duration'))

allStudies_effects.df$EffectTime <- factor(allStudies_effects.df$EffectTime,
       levels=c('during','post', 'post (5-15 min)',
                'post (20-75 min)', 'post (> 180 min)'))



allStudies_effects.df$Effect.N <- apply(allStudies_effects.df, 1, 
                                         function(row){
                                           if (grepl('Between', row['Exercise.Design'], ignore.case=T)){
                                             if (any(is.null(row['Ex.Level']),
                                                     is.na(row['Ex.Level']), 
                                                     row['Ex.Level'] != '')){
                                               N <- as.numeric(row['Compare.Level.N']) + as.numeric(row['Ex.Level.N'])
                                             } else {
                                               N <- as.numeric(row['Compare.Level.N'])
                                             }
                                           } else {
                                             N <- as.numeric(row['N'])
                                           }
                                         })  

allStudies_effects.df$g <- hedges_g(allStudies_effects.df$SMD, allStudies_effects.df$Effect.N)
allStudies_effects.df$g_se <- sqrt((1-(3/(4*allStudies_effects.df$Effect.N-1))^2)*allStudies_effects.df$Standard.Error^2)

rt_rows <- which(allStudies_effects.df$OutcomeVariable == 'RT')

allStudies_effects.df$g[rt_rows] <- allStudies_effects.df$g[rt_rows] * -1

allStudies_effects.df$Correlation[rt_rows] <- allStudies_effects.df$Correlation[rt_rows] * -1

allStudies_effects.df.clean <- allStudies_effects.df[which(!is.na(allStudies_effects.df$g)), ]

allStudies_effects.df.clean$es.ids <- factor(1:nrow(allStudies_effects.df.clean))


saveRDS(allStudies_effects.df.clean, file='study_effectSizes_hand.rds')



