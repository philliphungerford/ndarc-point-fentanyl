##############################################################################
# Purpose: Prepare data for fentanyl
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
# load libraries
library(dplyr) # for %>% 
library(mice) # for missing data 
library(VIM) # for aggr
library(sjPlot) # for tab_df
source("functions/rowname_convert.R") # for rowname_converter
source("functions/clean_data.R") # for clean_data function
##############################################################################
# load data
point <- read.csv("data/point-v0.9.1.2.csv", header = TRUE, fileEncoding = "UTF-8-BOM", na.strings=c(" ", ""))
##############################################################################
# 1. Create df (50,9084)
##############################################################################
df <- point %>% select(Participant_ID,
                       time,
                       followup,
                       death_flag,
                       
                       # covariates (n = 17)
                       ## demographics (n=5)
                       t1_age,
                       t1_sex,
                       
                       ## pain and physical health
                       BPI_PScore,
                       BPI_interference,
                       Arth_12m, 
                       Back_12m, 
                       Head_12m, 
                       Visc_12m,
                       num_chronic_cond_12m,
                       
                       ## mental and physical health
                       SF12_MCS,
                       SF12_PCS,
                       
                       ## substance use
                       t1_Any_lifetime_substance_dep_noalcohol,
                       
                       ## pharmaceutical opioid use
                       Pharm_Opioids_Dep_ICD10,
                       ORBIT_cont,
                       od_num_12m_clean, # outcome of interest
                       
                       ## medication use
                       Fentanyl, # OUTCOME
                       totalopioiddose,
                       t1_opcon, # duration of continuous opioid medication
                       benzo_week,
                       Antidepressant_week,
                       Pregabalin_week,
                       Antipsychotic_week,
                       
                       ## auxiliary
                       opi_sch8_use_clean,
                       Employ,
                       t1_pain_duration,
                       PSEQ_Score,
                       PODS_TOT,
                       opi_disc_mths_clean, # need for orbit calculations (past 3m)
                       PHQ9_severity,
                       GAD_severity,
                       WHO_QOL_q1,
                       WHO_QOL_q2,
                       
                       # side effects
                       sweating_degree,
                       vomiting_degree,
                       constipation_degree,
                       drowsiness_degree,
                       fatigue_degree,
                       itching_degree,
                       mental_cloud_degree,
                       nausea_degree,
                       sefct_other_degree,
                       sex_dys_degree,
                       
                       ### Other baseline variables
                       t1_iu67, # mother drug use
                       t1_iu69, # father drug use
                       t1_expect # expected duration to be on opioid medication
)
#===============================================================================
## Flag variables that have a not applicable option
# make missing for deaths 0 to censor post imputation
df$death_ind <- df$death_flag
df$death_ind[is.na(df$death_ind)] <- 0 # 7578 -> 8694

##----------------------------------------------------------------------------
## 1.4. Format our variables 
# make sure vars are classified

# Factors (n = 21/47. Do not need to include Participant_ID)
df <- df %>%
  mutate_at(c('time',
              'followup',
              'death_flag',
              'death_ind',
              
              # covariates (n = 17)
              ## demographics (n=5)
              't1_sex',
              'Arth_12m',
              'Back_12m', 
              'Head_12m', 
              'Visc_12m',
              
              ## substance use
              't1_Any_lifetime_substance_dep_noalcohol', # not in but will keep for now
              
              ## pharmaceutical opioid use
              'Pharm_Opioids_Dep_ICD10',
              
              ## medication use
              'Fentanyl',
              'benzo_week',
              'Antidepressant_week',
              'Pregabalin_week',
              'Antipsychotic_week',
              
              ## auxiliary
              'opi_sch8_use_clean',
              'Employ',

              ### Other baseline variables
              't1_iu67', # mother drug use
              't1_iu69', # father drug use
              't1_expect' # expected duration to be on opioid medication
  ), as.factor)

# FORMATS
# make sure vars are classified

# Ordinal (11/47 = 33/47)
df <- df %>%
  mutate_at(c(
    'opi_disc_mths_clean',
    't1_expect',
    'WHO_QOL_q1',
    'WHO_QOL_q2',
    'sweating_degree',
    'vomiting_degree',
    'constipation_degree',
    'drowsiness_degree',
    'fatigue_degree',
    'itching_degree',
    'mental_cloud_degree',
    'nausea_degree',
    'sefct_other_degree',
    'sex_dys_degree',
  ), as.ordered)

# Integers (9/47 = 42/47)
df <- df %>%
  mutate_at(c(
    't1_age',
    't1_pain_duration',
    'GAD_severity',
    'od_num_12m_clean',
    'ORBIT_cont',
    'PHQ9_severity',
    'PODS_TOT',
    'PSEQ_Score'
  ), as.integer)

# Floats / Numeric (5 /47 = 47/47 good!)
df <- df %>%
  mutate_at(c(
    't1_opcon',
    'BPI_interference',
    'BPI_PScore',
    'totalopioiddose',
    'SF12_PCS'
  ), as.numeric)

#===============================================================================
# Prepare for imputation (df will be imputed)
#-------------------------------------------------------------------------------
# Cleaning Fentanyl
# Need to clean No outcomes
# ome not missing data 
cat("\n Valid Fentanyl : \n")
fent_tb_before <- table(df$time, !is.na(df$Fentanyl))
fent_tb_before[,2]

cat("\n Valid OME : \n")
med_tb <- table(df$time, !is.na(df$totalopioiddose))
med_tb[,2]

# Examine Fentanyl variable
cat("\n Fentanyl y/n : \n")
fent_tb <- table(df$time, df$Fentanyl)
fent_tb

# Clean Fentanyl with valid OME data
df$Fentanyl[which(!is.na(df$totalopioiddose) & is.na(df$Fentanyl))] <- 0

# check updated cleaned variable
fent_tb_after <- table(df$time, !is.na(df$Fentanyl))

cat("\n Compare: pre-cleaned with cleaned \n ")
fent_tb_before[,2]
fent_tb_after[,2]
#-------------------------------------------------------------------------------
# Deductive imputation
deductive_clean <- c('Pharm_Opioids_Dep_ICD10', 'od_num_12m_clean', 'PODS_TOT', 'ORBIT_cont')

for (variable in deductive_clean){
  print(variable)
  print(table(is.na(df[,variable])))
  df[, variable][which(is.na(df[,variable]) & df$opi_sch8_use_clean == 0)] <- 0
  print(table(is.na(df[,variable])))
}

#-------------------------------------------------------------------------------
# Reorder from least to most missing for imputation process
n=1514
n_obs<-apply(apply(df,2,is.na),2,sum)
miss<-round(n_obs/n,digit=2)[order(n_obs)]
df<-df[,order(n_obs)]

#-------------------------------------------------------------------------------
## Check dataset is in correct format
summary(df) # 40 variables with 9084 observations READY FOR IMPUTING

##############################################################################
# Run function to actually clean. This function will be useful for cleaning 
## the imputed dataset
df.av <- clean_data(df) # (37, 8694)
#=============================================================================
##############################################################################
# create complete case data for characteristics
#-----------------------------------------------------------------------------
# Get complete case data = df.cc
# remove attrition
df.cc <- subset(df.av, followup==1) # 7578

# keep those based on medication diary
df.cc <- df.cc[complete.cases(df.cc$totalopioiddose), ] # 7152

# 1. Get count of ID's presented over time period (min = 1, max = 6) 
id_counts <- table(df.cc$Participant_ID)

# 2. Get ID's for people  who participated in all waves 
id_all_waves <- names(id_counts[id_counts == 6])

# 3. Subset data by these people (N = 4842)
df.cc <- df.cc[df.cc$Participant_ID %in% id_all_waves,] # 4830
table(df.cc$time) # 805 at each wave, N=4830

# ever use
table(df.cc$time, df.cc$ever_use)
##############################################################################
################################################################################
# 4. Missing data analysis 
#===============================================================================
# Create df_missing (20, 8694): only variables in the analysis
df_missing <- df.av %>%select(
  Participant_ID,
  time,
  
  # covariates (n = 17)
  ## demographics (n=5)
  under_58,
  t1_sex,
  Employ,
  
  ## pain and physical health
  pain_severity,
  pain_interference,
  pods,
  pseq_low,
  
  ## mental and physical health
  SF12_MCS,
  SF12_PCS,
  
  ## substance use
  t1_Any_lifetime_substance_dep_noalcohol, # not in but will keep for now
  
  #OUTCOME
  Fentanyl,
  
  ## pharmaceutical opioid use
  Pharm_Opioids_Dep_ICD10,
  orbit,
  overdose, # outcome of interest
  
  ## medication use
  totalopioiddose,
  median_prescribed, # duration of continuous opioid medication
  benzo_week,
  Antidepressant_week,
  Pregabalin_week,
  Antipsychotic_week,
  
  # side effects
  sweating_degree,
  vomiting_degree,
  constipation_degree,
  drowsiness_degree,
  fatigue_degree,
  itching_degree,
  mental_cloud_degree,
  nausea_degree,
  sefct_other_degree,
  sex_dys_degree
  
)
#===============================================================================
# Missingness pattern can also be visualised in VIM package by
df_aggr = aggr(df_missing, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df_missing), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# This plot gives the frequencies for different combination of variables missing. For example,
# that all the three variables chl, bmi & hyp are missing is the most frequent with about 28% frequency (7 observations). Note that, blue refers to observed data and red to the missing data. 

# You can also try it in mi package by
# missing.pattern.plot(nhanes, mis.col=mdc(2), obs.col=mdc(1), main="Missing Pattern")

# The margin plot of the pairs can be plotted using VIM package as
marginplot(df_missing[, c("Pharm_Opioids_Dep_ICD10", "under_58")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

# blue box plots summarise the distribution of observed data given the other variable is observed, 
# and red box plots summarise the distribution of observed data given the other variable is missing.
# For example, the red box plot in the bottom margin shows that bmi is rather missing for lower cholesterol levels. 
# Note that, if data are MCAR, we expect the blue and red box plots to be identical.

## If interested, you can also use VIM for scatterplots as
# scattMiss(nhanes[,c("bmi","chl")], inEllipse=TRUE, col=mdc(1:2),alpha=.8,bty="n", interactive=TRUE, axes=TRUE, lwd=c(1.5,1.5), pch=19, cex.lab=1.1, cex.axis=.9)
# rugNA(nhanes[,c("bmi")], nhanes[,c("chl")], side=2, col=mdc(2), lwd=1.5)
# rugNA(nhanes[,c("bmi")], nhanes[,c("chl")], side=1, col=mdc(2), lwd=1.5)

# missing data pattern
missing_pattern <- md.pattern(df_missing)

# for each time
for (i in 0:5){
  tmp <- subset(df_missing, time == i)
  df_aggr = aggr(tmp, col=mdc(1:2), plot=TRUE, numbers=TRUE, sortVars=TRUE, labels=names(tmp), cex.axis=.7, gap=3,  ylab=c("Proportion of missingness","Missingness Pattern"))
}

## numerator = number of missing cases
## denominator = 1514 - deaths
## attrition = 

#-----------------------------------------------------------------------------
# 1. Calculate the proportion of missing due to death
# Top 3 rows, deaths, attrition and alive
total <- 1514

for (t in 0:5){
  # extract data for time t 
  tmp <- subset(df, time==t)
  
  # calculate rows attributing to death
  deaths <- sum(tmp$death_ind==1) #20, 43, 82, 108, 137
  
  # find number of people who followed up 
  n <- sum(tmp$followup==1)
  
  # calculate attrition
  attrition <- (total - n) - deaths
  cat("########################## \n")
  cat("Time: ", t, "\n")
  cat("Missing due to death: ", deaths, "\n")
  cat("Followed up: ", n, "\n")
  cat("Attrition: ", attrition, "\n\n")
  cat("########################## \n")
  
}

#-----------------------------------------------------------------------------
# 2. missing proportions for variables
rounder = 2
tmp <- subset(df_missing, time==0)
t0 <- format(round((sapply(tmp, function(x) sum(is.na(x)))/nrow(tmp))*100,2), nsmall = rounder)

tmp <- subset(df_missing, time==1)
t1 <- format(round((sapply(tmp, function(x) sum(is.na(x)))/nrow(tmp))*100,2), nsmall = rounder)

tmp <- subset(df_missing, time==2)
t2 <- format(round((sapply(tmp, function(x) sum(is.na(x)))/nrow(tmp))*100,2), nsmall = rounder)

tmp <- subset(df_missing, time==3)
t3 <- format(round((sapply(tmp, function(x) sum(is.na(x)))/nrow(tmp))*100,2), nsmall = rounder)

tmp <- subset(df_missing, time==4)
t4 <- format(round((sapply(tmp, function(x) sum(is.na(x)))/nrow(tmp))*100,2), nsmall = rounder)

tmp <- subset(df_missing, time==5)
t5 <- format(round((sapply(tmp, function(x) sum(is.na(x)))/nrow(tmp))*100,2), nsmall = rounder)

# combine results
missing_n <- data.frame(t0,t1,t2,t3,t4,t5)
missing_n <- rowname_converter(missing_n)
names(missing_n)[names(missing_n) == 'rn'] <- 'variable'
print(missing_n)
tab_df(missing_n, show.rownames = T)
################################################################################
# FENTANYL USE
df %>%
  group_by(time, Fentanyl) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

################################################################################
# Save everything
save(point, df, df.av, df.cc, missing_n, missing_pattern, file="data/data-fentanyl-01-interim.RData")
##############################################################################
#################################### END #####################################
##############################################################################
