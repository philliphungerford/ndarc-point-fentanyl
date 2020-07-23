################################################################################
# PROGRAM: 03-post-imputation-clean
# PURPOSE: Clean imputed data
# WRITTEN BY: Phillip Hungerford
# DATE: 03/07/2020
################################################################################
# 0. Setup
#=============================================================================
## 0.1. load libraries
library(mice) # for manipulating imputed datasets 
library(dplyr) # for select and pipeline ( %>% )
source("functions/clean_data.R") # for clean_data function
#=============================================================================
## 0.2. load data
load("data/data-fentanyl-02-imputed.RData") # point, df, df_analyse, df_od, imp, missing_n, missing_pattern
##############################################################################
# Check imputation
##############################################################################
# Step 1: Check imputation performance
#=============================================================================
# We can inspect the distributions of the original and the imputed data:
# Blue represents the observed data and red shows the imputed data. These
## colours are consistent with what they represent from now on. 
# Here, we expect the red points (imputed data) have almost the same shape
## as blue points (observed data). Blue points are constant across imputed
## datasets, but red points differ from each other, which represents our 
## uncertainty about the true values of missing data.

#=============================================================================
## To detect interesting differences between observed and imputed data
densityplot(imp)

# This plot compares the density of observed data with the ones of imputed 
## data. We expect them to be similar (though not identical) under MAR 
## assumption.

#=============================================================================
# CONVERGENCE MONITORING
# MICE runs m parallel chains, each with a certain number of iterations, and 
## imputes values from the final iteration. How many iterations does mice() 
## use and how can we make sure that this number is enough? To monitor 
## convergence we can use trace plots, which plot estimates against the number
## of iteration.
plot(imp)

# Shows mean and standard deviation of the variables through the iterations
## for the m imputed datasets. An indicator of convergence is how well the m 
## parallel chains mix. We can also see here for example that mice has treated
## hyp as a continuous variable.

# See the univariate imputation model for each incomplete variable that mice()
## used for your data as default. If you used random forests these will all 
## be 'rf'. 
#imp$meth

#=============================================================================
# PREDICTOR MATRIX & SEQUENCE
# Predictor matrix: Which variables does mice() use as predictors for
## imputation of each incomplete variable? 
imp$pred # this is very very large, good for inspection only

# Visiting sequence: In what sequence does mice() impute incomplete variables?
vis = imp$vis; vis
##############################################################################
# Clean
#=============================================================================
# turn into df
imp_df <- complete(imp, action='long', include=TRUE) # shape = (190764, 52)
imp_df <- as.data.frame(imp_df)
#=============================================================================
# Run cleaning function
imp_df_clean <- clean_data(imp_df) # (182574, 69)
#=============================================================================
## 1.5. CHECK formats and types (factors are factors etc)
imp_df_clean$Fentanyl <- as.factor(imp_df_clean$Fentanyl)
imp_df_clean$time <- as.integer(imp_df_clean$time)-1
summary(imp_df_clean) # shape = (190764, 52)
#=============================================================================
## 1.6. Turn back into a mice imputed list of datasets
imp_analyse <- as.mids(imp_df_clean)

##############################################################################
# 2. Save your cleaned data! Everything + imp_analyse
save(point, df, df.av, df.cc, missing_n, missing_pattern, imp, imp_analyse, file="data/data-fentanyl-03-processed.RData")
##############################################################################
################################# END ########################################
##############################################################################
