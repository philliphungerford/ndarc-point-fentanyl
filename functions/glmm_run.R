##############################################################################
# PROGRAM: glmm_run
# PURPOSE: glmm function to run for bivariate tests
# WRITTEN BY: Phillip Hungerford
# DATE: 07/07/2020
##############################################################################
# build function to loop
glmm_run <- function(var, mod='ri') {
  #---------------------------------------------------------------------------
  require(mice) # Mice will need randomForest to run
  require(lme4) # Mice will need randomForest to run
  #---------------------------------------------------------------------------
  print(var)
  #---------------------------------------------------------------------------
  if (mod == "ri"){
    func <- paste(var, " ~ time + Fentanyl + (1 | Participant_ID)")
  } else {
    func <- paste(var, " ~ time + Fentanyl + (1 + time | Participant_ID)")
  }
  print(func)
  fit <- with(data=imp_analyse, exp = glmer(func, family = binomial()),
              # should control for the large eigenvector error
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              subset(opi_sch8_use_clean==1))
  #---------------------------------------------------------------------------
  #print(summary(pool(fit)))
  results_or <- summary(pool(fit), conf.int = TRUE, exponentiate = TRUE)
  print(results_or)
  #---------------------------------------------------------------------------
  return(fit)
}
##############################################################################
##############################################################################
##############################################################################