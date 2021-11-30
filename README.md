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
The meta-analysis was conducted using a Bayesian hierarchical model. Model files can be downloaded from the Attention Lab [google drive](https://drive.google.com/drive/folders/1VJ0x-WGR0ZiCf7OIfjBIddwbkET4LIrR?usp=sharing), and includes an analyses of the following exercise factors on cognition:
* Duration
* Experimental task
* Task outcome measure (RT and Accuracy)
* Time of task completion
* Exercise type (e.g., cycling)
* Cognitive domain
* Exercise intensity

To work with these models immediately, download them from the google drive link into a folder called **Models**. Loading them requires a specification of the covariates
within the model, priors, data, and the saved file name. For example:
```R
# call brms package
library(brms)

modelDir <- 'Models/'

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
If either the covariates included in the model, priors, or the data are changed, then a new model will be created. Setting `file_refit='on_change'` will load the model and 
posteriors unless the aformentioned inputs have be altered. 

Models within the google drive were created using *Bayesian_Modeling.R*, which includeds their formulas and priors. You can use this script to generate your own models.
Effect sizes can be found in the *Data* folder within *allStudies_effects.rds*, which is passed into the modeling sript.

If you want to incorporate your own studies in the analysis, add the study information (e.g., means, standard deviations, etc.) to
the proper columns in *PACMAN Effect Sizes.xlsx*. Afterwards, run *Compute_EffectSizes.R* and they will be added to the effects .rds file. Note, there is a lot of variability 
in experimental design across studies. Thus, many logical statements had to be included to ensure that each effect size was computed properly. You are encouraged to check the validity of computed effect sizes after running the script.
