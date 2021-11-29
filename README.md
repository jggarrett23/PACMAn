# PACMAn: Physical Activity and Cognition Meta-Analysis
-------------------------------------------------------
# Description
-------------
PACMAn contains scripts for conducting a meta-analysis of the acute effects of exercise on cognition. 

# REQUIREMENTS
--------------
Conducting the meta-analysis requires:
* R (> 4.X)
* brms 

The **brms** package can be tricky to install, so I recommend following the directions in the [brms repository](https://github.com/paul-buerkner/brms#faq).
Let me know if you need any help with that step.

# USAGE
For the meta-analysis, we utilized a Bayesian hierarchical model. This analysis can be carried out using *Bayesian_Modeling.R*. Modeled effect sizes are located in the Data folder,
within *processed_effects.rds*. If you want to include your own additional studies in the analysis, add the study information (e.g., means, standard deviations, etc.) to
the proper columns in *PACMAN Effect Sizes.xlsx*. Afterwards, run *Compute_EffectSizes.R* and they will be added to the processed effects file. Note, there is a lot of variability 
in experimental design across studies. Thus, many logical statements had to be included to ensure that each effect size was computed properly. You are encouraged to check the validity
of computed effect sizes after running the script.
