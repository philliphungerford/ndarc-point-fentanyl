##############################################################################
# Purpose: Univariate Tests # nothing significant
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
# Load libs
library(sjPlot)
library(sjmisc)
library(sjlabelled)
## tableone package itself
library(tableone)
# Unadjusted tests 
library(questionr)
##############################################################################
# Specify dataset (dv.cc = complete case, dv.ac = available data)
load("data/data-fentanyl-03-processed.RData")
df <- df.cc # should be the same either dataframe as waves is complete case
baseline <- subset(df.cc, time == 0) # 805
##############################################################################
univariate_tests <- function(labels, var.names, df, rounding=6){
  ###################################################################
  # PURPOSE:
  ## This function runs univariate tests for dichotomous variables
  # INPUTS:
  ## labels = list of desired labels for output that are not var.names
  ### well they could be if you want them to be.v
  ## var.names = list of independent variables
  ## df = dataframe of interest
  ## rounding = how much you want the results rounded
  # OUTPUTS:
  ## returns a dataframe with results
  # EXAMPLE:
  ## univariate_table <-univariate_tests(var.names, df = analysisData)
  ###################################################################
  df <- as.data.frame(df)
  # Make placeholder df 
  output <- data.frame(variable="variable", or=1.00, leftbracket = "(",
                       lower = 0.10, dash = "-", upper = 0.90, rightbracket =")",
                       p=0.005, stringsAsFactors = FALSE)
  # apply a univariate test to all variables on overdose variable
  fits <- lapply(var.names, function(x) {glm(substitute(ever_use ~ i, list(i = as.name(x))), family=binomial, data = df)})
  
  print(cat("Univariate Results (OR, CI, p-value): \n"))
  #writeLines(text = text, con = fileConn)
  
  for (i in 1:length(var.names)){
    # calculate odds ratio, with OR CI's and p value
    results <- odds.ratio(fits[[i]], level = 0.95)
    
    # coefs
    coefs <- results[2, ]
    output[i, 1] <- labels[i]
    output[i, 2] <- round(x = coefs[1,1], digits = 2)
    output[i, 3] <- "("
    output[i, 4] <- round(x = coefs[1,2], digits = 2)
    output[i, 5] <- "-"
    output[i, 6] <- round(x = coefs[1,3], digits = 2)
    output[i, 7] <- ")"
    output[i, 8] <- round(coefs[1,4], digits = 3)
    
  }
  output[, 2] <- as.character(output[, 2])
  output[, 4] <- as.character(output[, 4])
  output[, 6] <- as.character(output[, 6])
  
  or_ci <- paste0(output[, 2], " ", output[, 3], output[, 4], output[, 5], output[, 6], output[, 7])
  # Combines the OR and CI into one variable
  p <- output[, 8]
  p <- format.pval(pv = p, eps = 0.001, digits = 3)
  
  # Create new output with variables, OR(95%CI), P-value
  table <- data.frame(variables = output[,1], or_ci=or_ci, bivariate_p=p, stringsAsFactors = FALSE)
  write.table(x = table, file = "univariate_table.txt", sep = ",", quote = FALSE, row.names = F)
  return(table)
}
##############################################################################
# EXTRACT VARIABLES OF INTEREST
# create analysis data
analysisData <- baseline %>% 
  select(
    "Participant_ID",
    "t1_age", 
    "t1_sex",
    "Employ",
    "t1_pain_duration_yrs",
    "BPI_PScore", 
    "BPI_interference",
    "PSEQ_Score", 
    "Arth_12m", 
    "Back_12m", 
    "Head_12m", 
    "Visc_12m", 
    "num_chronic_cond_12m",
    "depression",
    "GAD",
    "SF12_PCS",
    "benzo_week",
    "Antidepressant_week",
    "Antipsychotic_week",
    # Not sure which one to use 
    "t1_Any_Lifetime_Substance_Disorder",
    #"t1_Any_lifetime_substance_dep",
    "WHO_QOL_q1",
    "WHO_QOL_q2",
    "totalopioiddose",
    "ever_use")
##############################################################################
# Clean variables
## more than one condition
analysisData$num_chronic_cond_12m <- ifelse(analysisData$num_chronic_cond_12m > 1 ,1,0)

## employment only unemployed
analysisData$Employ <- analysisData$Employ == "Not employed / on disability" # TRUE / FALSE

# analysisData$t1_Any_Lifetime_Substance_Disorder[is.na(analysisData$t1_Any_Lifetime_Substance_Disorder)]<- "No"
# analysisData$t1_Any_Lifetime_Substance_Disorder[is.na(analysisData$t1_Any_lifetime_substance_dep)]<- "No"

# create variables 
analysisData <- analysisData %>% mutate(ome_49 = totalopioiddose > 0 & totalopioiddose < 50,
                                        ome_89 = totalopioiddose > 49 & totalopioiddose < 90,
                                        ome_199 = totalopioiddose > 89 & totalopioiddose < 200,
                                        ome_200 = totalopioiddose > 199)

summary(analysisData)
##############################################################################
# Tests
labels <- c("Age", 
            "Male",
            "Unemployed",
            "Median years living with Pain", 
            "Pain severity score", 
            "Pain interference",
            "Pain: self-efficacy questionnaire score", 
            "Arthritis or rheumatism", 
            "Back or neck problems",
            "Frequent/severe headaches", 
            "Visceral",
            "% more than one pain condition",
            "% currently moderate/severe depression", 
            "% currently moderate/severe generalised anxiety disorder",
            "% meeting lifetime ICD-10 substance dependence no alcohol",
            "SF12 physical health score",
            "% benzodiazepines",
            "% antidepressants",
            "% antipsychotics",
            "% benzodiazepines",
            "% antidepressants",
            "% antipsychotics",
            "% overdose",
            "% score 1 or more on orbit",
            "% ICD10 Opioid dependence")

var.names <- c("t1_age", 
               "t1_sex",
               "t1_pain_duration_yrs",
               "BPI_PScore", 
               "BPI_interference",
               "PSEQ_Score", 
               "Arth_12m", 
               "Back_12m", 
               "Head_12m", 
               "Visc_12m", 
               "num_chronic_cond_12m",
               "depression",
               "GAD",
               "t1_Any_lifetime_substance_dep_noalcohol",
               "SF12_PCS",
               "benzo_week",
               "Antidepressant_week",
               "Antipsychotic_week",
               "overdose",
               "orbit",
               "Pharm_Opioids_Dep_ICD10")

## Note: You may have to chanfge the dependent variable to user. (DONE!)
univariate_table <- univariate_tests(labels = var.names, var.names = var.names, df = baseline)
univariate_table$bivariate_p <- as.numeric(univariate_table$bivariate_p)
univariate_table$bivariate_p <- round(univariate_table$bivariate_p, 2)
print(univariate_table)
##############################################################################
# save
write.csv(x = univariate_table, file = "output/results/univariate_table.csv", row.names = F)
##############################################################################
#################################### END #####################################
##############################################################################
