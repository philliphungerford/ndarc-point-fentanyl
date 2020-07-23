##############################################################################
# PROGRAM: bivariate.R
# PURPOSE: Bivariate models for behaviours paper
# WRITTEN BY: Phillip Hungerford
# DATE: 14/06/2020
# LAST EDITED: 07/07/2020
# DATA REQUIRED: imp_analyse_factor
##############################################################################
# 1.1. Setup
my.seed=2525
set.seed(my.seed, kind = "L'Ecuyer-CMRG" );
options(scipen = 999)
#-----------------------------------------------------------------------------
# 1.2. Specify where libraries are in HPC
.libPaths("/home/z5037298/R/x86_64-pc-linux-gnu-library/3.6/")

libs <- c("dplyr","mice", "lme4", "parallel")
missing <- !libs %in% installed.packages()

if (any(missing)) {
    install.packages(libs[missing])
}
#-----------------------------------------------------------------------------

# 1.3. Load libs for Katana
library(mice) # Mice will need randomForest to run
library(lme4) # Mice will need randomForest to run
library(parallel) # for parallel computing
library(dplyr) # for bind_rows
source("functions/glmm_run.R")
##############################################################################
# 2.Load data
#-----------------------------------------------------------------------------

# print("Loading data...")
load("data-fentanyl-03-processed.RData.RData")
# print("Done!")

##############################################################################
# 3. Create functions
#-----------------------------------------------------------------------------
# Model parameters
## Subset only for those using opioids bc we are comparing opioids users: fentanyl vs other opioids
### in predicting side effects. That is, fentanyl a significant predictor of a side effect
## Model has been tested with Random intercepts only vs random intercepts and slopes (RIS)
## RIS found to account for variability in the data better producing a lower AIC in preliminary
## complete data analysis.
#-----------------------------------------------------------------------------
##############################################################################
# specify variables for model
independent_variables <- c(
  'constipation_degree',
  'drowsiness_degree',
  'fatigue_degree',
  'itching_degree',
  'mental_cloud_degree',
  'nausea_degree',
  'sex_dys_degree',
  'sweating_degree',
  'vomiting_degree'
)
##############################################################################
# For each variable in independent variables, they are run in the function and returned into a list
#results <- mclapply(independent_variables, FUN = glmm_run, mc.cores=length(independent_variables)) # 8 cores needed
results <- lapply(independent_variables, FUN = glmm_run) # Windows PC

results_or <- lapply(results, FUN = pool)
results_or <- lapply(results_or, FUN = summary, conf.int = TRUE, exponentiate = TRUE)

for (i in 0:8){
  print(results_or[i])
}

# sweating <- glmm_run('sweating_degree') # test for single var
##############################################################################
file_name="12-side-effects-imp-RI.RData"
save(results, file = file_name)
##############################################################################
##############################################################################
##############################################################################
