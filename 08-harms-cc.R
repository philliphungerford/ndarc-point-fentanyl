##############################################################################
# Purpose: Generalised Linear Mixed Models
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
library(dplyr)
library(lme4)
##############################################################################
# Specify dataset (dv.cc = complete case, dv.ac = available data)
load("data/data-fentanyl-01-interim.RData")
##############################################################################
# Run function to actually clean. This function will be useful for cleaning 
## the imputed dataset
df.av$time <- as.integer(df.av$time)-1
df <- df.av
##############################################################################
# test for RI
fit.1 <- glmer(Fentanyl ~ time + (1 | Participant_ID), data = df, family=binomial) # AIC = 1455
summary(fit.1)

# test for RIS: RIS WINS
fit.2 <- glmer(Fentanyl ~ time + (1 + time | Participant_ID), data = df, family=binomial) # AIC = 1334
summary(fit.2)

##############################################################################
# specify variables for model
independent_variables <- c(
  # DEMOGRAPHICS
  'under_58',
  't1_sex',
  'unemployed',
  # PAIN RELATED
  'pain_severity',
  'pain_interference',
  'pseq_low',
  'SF12_PCS_M',
  # MENTAL HEALTH
  'depression',
  'GAD',
  't1_Any_lifetime_substance_dep_noalcohol',
  'SF12_MCS_M',
  # OTHER MEDICATIONS
  'benzo_week',
  'Antidepressant_week',
  'Pregabalin_week',
  'Antipsychotic_week')

##############################################################################
# Create placeholder
bv_df <- data.frame('characteristics' = independent_variables, "or"=NA, "p"=NA)

##############################################################################
# GLMM: Associations of fentanyl with harms
for (i in 1:length(independent_variables)){
  print("###################################################################")
  print(table(df$time, df[, independent_variables[i]]))
  func <- paste0("Fentanyl ~ time + ", independent_variables[i], " + (1 + time |Participant_ID)")
  print(func)
  fit <- glmer(func, data=df, family=binomial(),
               # should control for the large eigenvector error
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  print(summary(fit))
  se <- sqrt(diag(vcov(fit)))
  # table of estimates with 95% CI
  tab <- cbind(Est = fixef(fit), LL = fixef(fit) - 1.96 * se, UL = fixef(fit) + 1.96 * se)
  or <- format(round(exp(tab), 2), nsmall=2)
  cat(independent_variables[i], ": ", or[3,1], " (", or[3,2], "-", or[3,3], ")", sep ="")
  results <- paste0(or[3,1], " (", or[3,2], "-", or[3,3], ")")
  bv_df$or[which(bv_df$characteristics == independent_variables[i])] <- results
}
##############################################################################
# ORDINAL VARIABLES EMPLOYMENT YRS IN PAIN
print("###################################################################")
print(table(df$time, df[, independent_variables[i]]))
func <- paste0("Fentanyl ~ time + Employ + (1 + time |Participant_ID)")
print(func)
fit <- glmer(func, data=df, family=binomial(),
             # should control for the large eigenvector error
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
print(summary(fit))
se <- sqrt(diag(vcov(fit)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(fit), LL = fixef(fit) - 1.96 * se, UL = fixef(fit) + 1.96 * se)
or <- format(round(exp(tab), 2), nsmall=2)
or <- as.data.frame(or)
or$result <- paste0(or[,1]," (", or[,2],"-", or[,3],")")
print(or$result)

print("###################################################################")
print(table(df$time, df[, independent_variables[i]]))
func <- paste0("Fentanyl ~ time + t1_pain_duration_yrs + (1 + time |Participant_ID)")
print(func)
fit <- glmer(func, data=df, family=binomial(),
             # should control for the large eigenvector error
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
print(summary(fit))
se <- sqrt(diag(vcov(fit)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(fit), LL = fixef(fit) - 1.96 * se, UL = fixef(fit) + 1.96 * se)
or2 <- format(round(exp(tab), 2), nsmall=2)
or2 <- as.data.frame(or2)
or2$result <- paste0(or2[,1]," (", or2[,2],"-", or2[,3],")")
print(or2)

##############################################################################
# Harms associated
#=============================================================================
# BINARY HARMS
harms_bin <- c(
  'pain_severity',
  'pain_interference',
  "overdose", # 2,0,3,4,1,0
  "Pharm_Opioids_Dep_ICD10", # 9,0,4,8,4,4
  "orbit", # sig
  "pods"
)

fit <- lmer(BPI_interference ~ time + Fentanyl + (1 + time | Participant_ID), data=df.cc,
            control=lmerControl(optimizer="Nelder_Mead",
                                optCtrl=list(maxfun=1e4)))
summary(fit)

fit <- lmer(BPI_PScore ~ time + Fentanyl + (1 + time | Participant_ID), data=df.cc,
            control=lmerControl(optimizer="Nelder_Mead",
                                optCtrl=list(maxfun=1e4)))
summary(fit)

# calculate N of those experiencing over time
for (t in 0:5){
  tmp <- subset(df, time == t)
  print(table(tmp$Fentanyl, tmp[, harms_bin[2]]))
}

# GLMM: Associations of fentanyl with harms
library(lme4)
for (i in 1:length(harms_bin)){
  print("###################################################################")
  print(table(df$time, df[, harms_bin[i]]))
  func <- paste0(harms_bin[i], " ~ time + Fentanyl + (1 + time |Participant_ID)")
  print(func)
  fit <- glmer(func, data=df, family=binomial())
  print(summary(fit))
  se <- sqrt(diag(vcov(fit)))
  # table of estimates with 95% CI
  tab <- cbind(Est = fixef(fit), LL = fixef(fit) - 1.96 * se, UL = fixef(fit) + 1.96 * se)
  or <- format(round(exp(tab), 2), nsmall=2)
  results <- paste0(harms_bin[i], ": ", or[3,1], " (", or[3,2], "-", or[3,3], ")")
  print(results)
}

##############################################################################
##############################################################################
# test for RI
#fit.1 <- glmer(Fentanyl ~ time + (1 | Participant_ID), data = df, family=binomial) # AIC = 1455
#summary(fit.1)

# test for RIS: RIS WINS
#fit.2 <- glmer(Fentanyl ~ time + (1 + time | Participant_ID), data = df, family=binomial) # AIC = 1334
#summary(fit.2)

# specify variables for model
independent_variables <- c(
  # DEMOGRAPHICS
  'under_58',
  't1_sex',
  # PAIN RELATED
  'pain_severity',
  'pain_interference',
  'pseq_low',
  'SF12_PCS_M',
  # MENTAL HEALTH
  'depression',
  'GAD',
  't1_Any_lifetime_substance_dep_noalcohol',
  'SF12_MCS_M',
  # OTHER MEDICATIONS
  'benzo_week',
  'Antidepressant_week',
  'Pregabalin_week',
  'Antipsychotic_week')
#=============================================================================
# Create placeholder
bv_df <- data.frame('characteristics' = independent_variables, "or"=NA, "p"=NA)

# GLMM: Associations of fentanyl with harms
for (i in 1:length(independent_variables)){
  print("###################################################################")
  print(table(df$time, df[, independent_variables[i]]))
  func <- paste0("Fentanyl ~ time + ", independent_variables[i], " + t1_age + t1_sex + (1 + time |Participant_ID)")
  print(func)
  fit <- glmer(func, data=df, family=binomial(),
               # should control for the large eigenvector error
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  print(summary(fit))
  se <- sqrt(diag(vcov(fit)))
  # table of estimates with 95% CI
  tab <- cbind(Est = fixef(fit), LL = fixef(fit) - 1.96 * se, UL = fixef(fit) + 1.96 * se)
  or <- format(round(exp(tab), 2), nsmall=2)
  cat(independent_variables[i], ": ", or[3,1], " (", or[3,2], "-", or[3,3], ")", sep ="")
  results <- paste0(or[3,1], " (", or[3,2], "-", or[3,3], ")")
  bv_df$or[which(bv_df$characteristics == independent_variables[i])] <- results
}
##############################################################################
#################################### END #####################################
##############################################################################
