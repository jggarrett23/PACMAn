Workflow

Trim_Fill_Correction.R

# Estimate models
sbatch cluster_Bayesian_Modeling_job.slurm

# Estimate interaction models
sbatch cluster_Model_Interactions_job.slurm

# Secondary moderator analyses
sbatch cluster_NatCommPsy_Moderators_job.slurm

# Estimate marginals
sbatch cluster_Model_Marginals_job.slurm

sbatch cluster_Interaction_Marginals_job.slurm

# Estimate BFs for model comparison
sbatch cluster_Model_Compare_BFs_job.slurm

Model_Compare_InclusionBF.ipynb 

# Plot posteriors
Plot_Posteriors.R

Plot_Interaction_Model_Posteriors.R

# Sensitivity Analyses
sbatch cluster_Sensitivity_Analyses_job.slurm

# Executive Function meta-analysis
sbatch cluster_EF_Modeling_job.slurm

sbatch cluster_EF_Model_Interactions_job.slurm

sbatch cluster_EF_Model_Marginals_job.slurm

sbatch cluster_EF_Interaction_Model_Marginals_job.slurm

sbatch cluster_EF_Model_Compare_BFs_job.slurm

EF_Plot_Posteriors.R



