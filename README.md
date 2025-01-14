# PACMAn: Physical Activity and Cognition Meta-Analysis
-------------------------------------------------------
# DESCRIPTION
-------------
PACMAn contains scripts for conducting a meta-analysis of the acute effects of exercise on cognition. 

# REQUIREMENTS
--------------
Conducting the meta-analysis requires:
* R version > 4.X
* brms package 

The brms package is a little finicky to install. To install the **brms** package follow the directions for your operating system in the [brms repository](https://github.com/paul-buerkner/brms#faq).

# USAGE
The meta-analysis was conducted using a Bayesian hierarchical model. Model files can be downloaded from the Attention Lab [google drive](https://drive.google.com/drive/folders/18PDFOBtmQ1oSd7lSJtLcrRE9gLwu7nrC?usp=drive_link), and includes analyses of the following exercise factors on cognition:
* Duration
* Experimental task
* Task outcome measure (RT and Accuracy)
* Time of task completion
* Exercise type (e.g., cycling)
* Cognitive domain
* Exercise intensity

To work with these models immediately, download them from the google drive link into a folder called **Models**. Loading them requires a specification of the predictors
within the model, priors, data, and the saved file name. For example:
```R
# call brms package
library(brms)

modelDir <- 'Models'

# set priors. using a weakly informative normal prior for the overall effect and a cauchy prior for heterogenity
priors <- c(prior(normal(0,1), class=Intercept),
            prior(cauchy(0,0.5), class=sd))

# meta-analysis with random effects model
# Note: brms follows the formula convention of lme4, where the '|' and '/' indicate nesting of covariates.
overall_model <- brm(g|se(g_se) ~ 1 + (1|Author/es.ids),
                    data=data
                    prior=priors
                    file=paste(modelDir,'overall_random',sep='/'),
                    file_refit='on_change')

```
**NOTE**: Before running any of the scripts make sure to set the current project to *PACMAn.Rproj*. This will ensure that your working directory is set to the repository folder.

If either the predictors included in the model, priors, or the data are changed, then a new model will be created. 
Once the model has been loaded, the posteriors for the beta weight on each predictor can be extracted:
```R
# extract posteriors for the beta weights (i.e. any column name ending with a 'b') and heterogenities (i.e., 'sd')
post_samps <- posterior_samples(overall_model, c('^b', '^sd'))

# changes names to be more interpretable. these names will change according to the predictors in the model
names(post_samps) <- c('g','tau1','tau2')
```
To make predictions using the loaded model:
```R
# make predictions based on new data
preds <- predict(overall_model, newdata=new_data)
```

Models within the google drive were created using *Bayesian_Modeling.R*, which includeds their formulas and priors. You can use this script to generate your own models.
Effect sizes can be found in the *Data* folder within *allStudies_effects.rds*, which is passed into the modeling sript to convert them into Hedge's *g*.

If you want to incorporate your own studies in the analysis, add the study information (e.g., means, standard deviations, etc.) to
the proper columns in *PACMAN Effect Sizes.xlsx*. Afterwards, run *Compute_EffectSizes.R* and they will be added to the effects .rds file. Note, there is a lot of variability 
in experimental design across studies. Thus, many logical statements had to be included to ensure that each effect size was computed properly. You are encouraged to check the validity of computed effect sizes after running the script.
