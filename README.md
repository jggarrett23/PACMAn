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

To work with these models immediately, download them from the google drive link into a folder called **Models**. Here is an example of how models can be loaded
```R
# call brms package
library(brms)

modelDir <- ('Models\')

# set priors. using a weakly informative normal prior for the overall effect and a cauchy prior for heterogenity
priors <- c(prior(normal(0,1), class=Intercept),
            prior(cauchy(0,0.5), class=sd))

# meta-analysis (i.e. intercept model)
overall_model <- brm(g|se(g_se) ~ 1 + (1|Author/es.ids),
                    data=data
                    prior=priors
                    file=paste(modelDir,'overall_random',sep='/'),
                    file_refit='on_change')

```

These models were created using the *Bayesisan_Modeling.R* script. Effect sizes can be found in the *Data* folder within *allStudies_effects.rds*. Then, the modeling script can be run and you can work with the 
models immediately rather than having to wait for them to converge.






If you want to include your own additional studies in the analysis, add the study information (e.g., means, standard deviations, etc.) to
the proper columns in *PACMAN Effect Sizes.xlsx*. Afterwards, run *Compute_EffectSizes.R* and they will be added to the effects .rds file. Note, there is a lot of variability 
in experimental design across studies. Thus, many logical statements had to be included to ensure that each effect size was computed properly. You are encouraged to check the validity
of computed effect sizes after running the script.
