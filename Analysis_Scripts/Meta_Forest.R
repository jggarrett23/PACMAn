

library(tidyverse)
library(dplyr)
library(metaforest)
library(caret)

# --- Setting Up Directories ---

parentDir <- getwd()
dataDir <- file.path(parentDir,'Data')


setwd(dataDir)
set.seed(123)


# Load data
effectSizes.df <- readRDS('study_effectSizes_hand.rds')

data <- effectSizes.df %>% 
  select(g,Ex.ACSM,Ex.Mode.2,OutcomeVariable,Domain.2,Duration.2, EffectTime, Article.ID)

data$vi <- effectSizes.df$g_se

data <- data %>% drop_na()

# --- Metaforest ---

check_conv <- MetaForest(g ~., 
                         data=data,
                         study="Article.ID",
                         whichweights = "random",
                         num.trees=7500
                         )

plot(check_conv)


mf_rep <- MetaForest(g ~.,
                     data=data,
                     study='Article.ID',
                     whichweights = 'random',
                     num.trees=3000)

preselected <- preselect(mf_rep,
                         replications=100,
                         algorithm='recursive')

plot(preselected)


retain_mods <- preselect_vars(preselected, cutoff=0.5)

grouped_cv <- trainControl(method='cv',
                           index=groupKFold(data$Article.ID, k=3))


tuning_grid <- expand.grid(whichweights=c("random", "fixed"),
                           mtry=2:length(retain_mods),
                           min.node.size=2:6)

X <- data[, c("Article.ID", "vi", retain_mods)]

mf_cv <- train(y = data$g,
               x = X, 
               study = "Article.ID",
               method = ModelInfo_mf(),
               trControl = grouped_cv,
               tuneGrid = tuning_grid,
               num.trees = 3000)


mf_cv$results[which.min(mf_cv$results$RMSE), ]

final <- mf_cv$finalModel

r2_oob <- final$forest$r.squared

plot(final)


VarImpPlot(final)
