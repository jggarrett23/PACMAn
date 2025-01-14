"
-----------------------------------------------------------
PACMAn Meta-Analysis: Trim and Fill Correction


Author: Jordan Garrett
UCSB Attention Lab
jordangarrett@ucsb.edu
-----------------------------------------------------------
"
library(metafor)
library(tidyverse)

# --- Setting Up Directories ---

parentDir <- '/work/garrett/PACMAn/NatComm_Update'
dataDir <- file.path(parentDir,'Data')

setwd(dataDir)

effectSizes.df <- readRDS('study_effectSizes_hand_NatCommPsy_searchUpdate.rds')

# --- Trim ---

res0 <- rma(yi=g, sei=g_se, data=effectSizes.df, test="t")

eggs.test <- regtest(res0)
eggs.p <- eggs.test$fit$pval[1]
d <- effectSizes.df %>%
        arrange(desc(g_se))
nEffects <- nrow(d)
d$Imputed <- F
remove_studiesIdx <- c()
remove_studies_esIDs <- c()
remove_cnt <- 1
a_thresh <- 0.145 # determined by running trimmed and filled until passing egger's regression test
while (eggs.p < a_thresh){
    
    # positive effect size with largest standard error
    for (i in remove_cnt:nEffects){
        if (d$g[i] > 0){
            
            remove_studiesIdx <- append(remove_studiesIdx, i)
            remove_studies_esIDs <- append(remove_studies_esIDs, d$es.ids[i])
            
            break
            }
        break
        }
    
    d_rm <- d[-c(remove_studiesIdx),]
    
    # fit model again
    test <- regtest(d_rm$g, sei=d_rm$g_se, test="t")
    
    eggs.p <- test$fit$pval[1]
    remove_cnt <- remove_cnt + 1
    
    }

# Remove effect sizes with low precision
keep_effectSizes.df <- effectSizes.df %>%
                            filter(!es.ids %in% remove_studies_esIDs) %>%
                            mutate(interpolated=F)

# Estimate overall effect size with fixed effects model
fix_ef <- rma(yi=keep_effectSizes.df$g,
              sei=keep_effectSizes.df$g_se,
              method='FE')

# Impute low precision effect sizes
imputed_effectSizes <- effectSizes.df %>%
                        filter(es.ids %in% remove_studies_esIDs) %>%
                        mutate(g = fix_ef$beta[[1]] - (g - fix_ef$beta[[1]]),
                               interpolated=T)

nImputed_effects <- nrow(imputed_effectSizes)

imputed_effectSizes$es.ids <- c(1:nImputed_effects) + nrow(effectSizes.df)

# Incorporate previously removed effects
prev_removed_effectSizes <- effectSizes.df %>%
                         filter(es.ids %in% remove_studies_esIDs) %>%
                         mutate(interpolated=F)

# prevents imputed effects ID from being dropped
keep_effectSizes.df$es.ids <- as.numeric(keep_effectSizes.df$es.ids)
imputed_effectSizes$es.ids <- as.numeric(imputed_effectSizes$es.ids)
prev_removed_effectSizes$es.ids <- as.numeric(prev_removed_effectSizes$es.ids)

effectSizes_TF.df <- rbind(keep_effectSizes.df, imputed_effectSizes, prev_removed_effectSizes)

effectSizes_TF.df$es.ids <- factor(effectSizes_TF.df$es.ids)

# for testing if egger's regression is significant
res <- rma(yi=effectSizes_TF.df$g, sei=effectSizes_TF.df$g_se)

regtest(res, test="t")

saveRDS(effectSizes_TF.df, 'study_effects_trimmed_filled.rds')