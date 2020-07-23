##############################################################################
# PROGRAM: 02-imputation.R
# PURPOSE: Opioid behaviours imputation
# WRITTEN BY: Philip Clare & Phillip Hungerford
# DATE: 25/05/2020
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------
# 1.0. Time Imputation
start_time <- Sys.time()

#-----------------------------------------------------------------------------
# 1.2. Specify where libraries are in HPC
.libPaths("/home/z5037298/R/x86_64-pc-linux-gnu-library/3.6/")

libs <- c("randomForest","mice")
missing <- !libs %in% installed.packages()

if (any(missing)) {
    
    install.packages(libs[missing])
    
}

#-----------------------------------------------------------------------------

# 1.3. Load libs for Katana
print("Loading libraries...")
library(mice) # Mice will need randomForest to run
print("Done!")

##############################################################################
# 2.Load data
#-----------------------------------------------------------------------------

# print("Loading data...")
load("data/data-fentanyl-01-interim.RData")
# print("Done!")

##############################################################################
# 3. Imputation
#-----------------------------------------------------------------------------

# 3.0. Define Parameters (Defaults, m=20, maxit=100)
m <- as.numeric(Sys.getenv('NCPUS')) # Number of imputations
maxit <- 100; # Number of mice iterations
set.seed(2525) # Not the seed used by parlmice, but needs to be set or parlmice fails
cluster.seed <- 23771 # Needs to be set within function for parallel computing
numcores <- 20 # Number of cores to use (default = ncore-1)
n.imp.core = m/numcores # number of imputations per core (default = 2)
cl.type = "FORK" # Could be FORK for *NIX based systems

#-----------------------------------------------------------------------------
# 3.1. Run Imputation
#-----------------------------------------------------------------------------

# 3.2. Serial
#imp <- mice(df, m=m, maxit=maxit, printFlag=TRUE);# standard
#imp <- mice(df, m=m, maxit=maxit, method="rf", printFlag=TRUE); #random forest 17min

#-----------------------------------------------------------------------------
# 3.3. Parallel
#-----------------------------------------------------------------------------
## Parameters
# cluster.seed = needs to be set within function for parallel computing
# n.core = number of cores to use (default = ncore-1)
# n.imp.core = number of imputations per core (default = 2)
# cl.type
# m = (n.core x n.imp.core) = (20 x 1)

## windows use PSOCK (works locally) testing m=3 imputations at 2 iterations
#imp <- parlmice(data = df, cluster.seed = 123, n.core = 3, n.imp.core = 1, maxit = maxit, method="rf", cl.type = "PSOCK");

# mac / katana use FORK (works locally) testing m=3 imputations at 2 iterations
#imp <- parlmice(data = df, cluster.seed = 123, n.core = 3, n.imp.core = 1, maxit = maxit, method="rf", cl.type = "FORK");

# Katana m = 20 imputations (request 20 or 22 cpu) at 100 iterations
imp <- parlmice(data=df,
                cluster.seed=cluster.seed,
                n.core=numcores,
                n.imp.core=n.imp.core,
                maxit=maxit,
                method="rf",
                cl.type=cl.type)

##############################################################################
# 4. Save all with the addition of 'imp' the imputed datasets. Should take around 5hrs
save(point, df, df.av, df.cc, missing_n, missing_pattern, imp, file="data/data-fentanyl-02-imputed.RData")

#-----------------------------------------------------------------------------
# 5. Calculate Time
end_time <- Sys.time()
end_time - start_time
time_taken <- end_time - start_time
cat('Using mice, ', m, 'imputations with ', maxit, 'iterations took:', time_taken, attr(time_taken,"units"), ".","\n")
##############################################################################
################################### END ######################################
##############################################################################
