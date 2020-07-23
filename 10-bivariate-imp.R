##############################################################################
# PROGRAM: bivariate.R
# PURPOSE: Bivariate models for behaviours paper
# WRITTEN BY: Phillip Hungerford
# DATE: 14/06/2020
# LAST EDITED: 07/07/2020
# DATA REQUIRED: imp_analyse
##############################################################################
# 1.1. Setup
my.seed=2525
set.seed(my.seed, kind = "L'Ecuyer-CMRG" );

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
options(scipen = 999)
##############################################################################
# 2.Load data
#-----------------------------------------------------------------------------

# print("Loading data...")
load("data/data-fentanyl-03-processed.RData")
# print("Done!")

##############################################################################
# 3. Create functions
#-----------------------------------------------------------------------------
# Model parameters
## Model has been tested with Random intercepts only vs random intercepts and slopes (RIS)
## RIS found to account for variability in the data better producing a lower AIC in preliminary
## complete data analysis.
## Given that we want to see what characteristics are associated with fentanyl use
## we do not need to subset the data for this analysis. This does change however when looking
## at harms related to fentanyl use.
#-----------------------------------------------------------------------------
# 3.1. glmm function
# build function to loop
glmm_run <- function(var) {
  #func <- paste("Fentanyl ~ time + ", var, "+ (1 | Participant_ID)")
  func <- paste("Fentanyl ~ time + ", var, "+ (1 + time | Participant_ID)")
  fit <- with(data=imp_analyse, exp = glmer(func, family = binomial()),
              # should control for the large eigenvector error
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 
  # Get summarised results
  print(summary(pool(fit)))
  
  # Extract Odds Ratios
  results_or <- summary(pool(fit), conf.int = TRUE, exponentiate = TRUE)
  return(results_or)
}

##############################################################################
# Specify characteristics we wish to examine
independent_variables <- c(
  # DEMOGRAPHICS
'under_58',
't1_sex',
'unemployed',
#PAIN
't1_pain_duration_yrs',
'pain_severity',
'pain_interference',
'pseq_low',
'SF12_PCS_M',
# MENTAL HEALTH
'depression',
'GAD',
't1_Any_lifetime_substance_dep_noalcohol',
'SF12_MCS_M',
#OTHER MEDICINE
'median_prescribed',
'benzo_week',
'Antidepressant_week',
'Antipsychotic_week',
'Pregabalin_week')

##############################################################################
# Run models.
# For each variable in independent variables, they are run in the function and returned into a list
#results <- mclapply(independent_variables, FUN = glmm_run, mc.cores=length(independent_variables)) # 21 cores needed
results <- lapply(independent_variables, FUN = glmm_run) # 21 cores needed

##############################################################################
# Save results
file_name="output/results/10-bivariate-imp.RData"
save(results, file = file_name)
##############################################################################
##############################################################################
##############################################################################
