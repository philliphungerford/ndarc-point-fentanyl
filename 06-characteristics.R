##############################################################################
# Purpose: Characteristics
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
# Specify dataset (dv.cc = complete case, dv.ac = available data)
load("data/data-fentanyl-03-processed.RData")
df <- df.cc # should be the same either dataframe as waves is complete case
baseline <- subset(df.cc, time == 0)
##############################################################################
# Functions for summaries
mean_sd <- function(df, name, var){
  # msd <- mean_sd("mean_sd_vars[i]"BPI Score", "T1_BPI_PScore")
  df <- as.data.frame(df)
  tmp <- df[,var]
  m <- round(mean(tmp, na.rm=T), 2)
  sd <- round(sd(tmp, na.rm=T),2)
  result <- paste0(m, " (",sd,")")
  return(result)
}

median_iqr <- function(df, name, var){
  # m_iqr <- median_iqr("SF12 Mental health score", "T1_SF12_MCS")
  df <- as.data.frame(df)
  tmp <- df[,var]
  m <- round(median(tmp, na.rm=T),2)
  # Calculate IQR
  q <- quantile(tmp, na.rm=T)
  result <- paste0(m, " (", round(q[2],2), "-", round(q[4],2), ")")
  return(result)
}

percent_ci <- function(df, name, var, outcome="Yes"){
  # pci <- percent_ci(df, "Male", "T1_sex", outcome="Male")
  df <- as.data.frame(df)
  tmp <- df[,var]
  
  # Calculate the p and se
  observed <- sum(df[, var] == outcome, na.rm=T)
  n <- length(tmp)
  
  # calculate the proportion
  p <- ( observed / n) * 100
  
  # standard error = proportion of outcome 1 * proportion of outcome 2 / n
  se <- sqrt(p * (100 - p) / n)
  
  ## 2. Create % (95% CI)
  # return result
  estimate <- round(p, 2)
  lower <- round(p - (1.96*se), 2)
  upper <- round(p + (1.96*se), 2)
  
  # print results
  result <- paste0(estimate, " (", lower, "-", upper, ")")
  
  # return results
  return(result)
}
##############################################################################
# Calculate characteristics
#=============================================================================
# Characteristics names
chars <- c(
  "DEMOGRAPHICS", 
  "Mean age (SD)", 
  "% male (95%CI)", 
  "Employment status % (95%CI)", 
  "…Retired", 
  "…Unemployed",
  "…Employed, student or home duties",
  "PAIN RELATED FACTORS",
  "Years living with pain",
  "…0-5 years",
  "…6-15years",
  "…>15 years",
  "Mean baseline pain severity (SD)",
  "Mean Baseline pain interference (SD)",
  "Mean baseline pain self-efficacy (SD)",
  "PAIN CONDITIONS",
  "…Arthritis",
  "…Back/neck problems",
  "…Migraines",
  "…Visceral",
  "…more than one pain condition",
  "Mean SF-12 PCS (SD)",
  "MENTAL AND PHYSICAL HEALTH",
  "% mod-severe depression (PHQ)",
  "% mod-severe anxiety (GAD)",
  "% history substance disorder ",
  "% history ICD-10 substance dependence+",
  "Mean SF-12 MCS (SD)",
  "OTHER MEDICINE USE",
  "Median years using prescribed opioids (IQR)",
  "% benzodiazepines ",
  "% antidepressants ",
  "% anti-psychotics ",
  "HARMS",
  "% overdose",
  "% score 1 or more on orbit",
  "% ICD10 Opioid dependence",
  "% intermediate/high pods")
##############################################################################
# Create table maker function 
table2 <- function(baseline){
  # create placeholder table 
  t2 <- data.frame('Characteristics' = chars, "Never" = NA, "Ever" = NA, "All" = NA)
  
  # Calculate for Never, ever and all (n=3)
  for (i in 1:3){
    if (i == 1) {
      # No fentanyl users
      print("Calculating for: Non-Fentanyl Users -------------------------------")
      tmp <- subset(baseline, ever_use == FALSE)
      #tmp <- subset(baseline, Fentanyl == 0)
      N <- nrow(tmp)
      t2[1,2] <- paste0("N = ", N, "\n")
    } else if (i == 2) {
      i = 2
      # Fentanyl users
      print("Calculating for: Fentanyl Users -----------------------------------")
      tmp <- subset(baseline, ever_use == TRUE)
      #tmp <- subset(baseline, Fentanyl == 1)
      N <- nrow(tmp)
      t2[1,3] <- paste0("N = ", N, "\n")
    } else {
      # all 
      print("Calculating for: Total -----------------------------------")
      tmp <- baseline
      N <- nrow(tmp)
      t2[1,4] <- paste0("N = ", N, "\n")
    }
    
    t2[2, i+1] <- mean_sd(tmp, "Age", "t1_age")
    t2[3, i+1] <- percent_ci(tmp, "Male", "t1_sex", outcome=1)
    t2[5, i+1] <- percent_ci(tmp, "...Retired", "Employ", outcome="Retired")
    t2[6, i+1] <- percent_ci(tmp, "...Unemployed", "Employ", outcome="Not employed / on disability")
    t2[7, i+1] <- percent_ci(tmp, "...Employed, student or home duties", "Employ", outcome="Employed")
    
    print("Years living in pain")
    t2[10, i+1] <- percent_ci(tmp, "...0-5 years", "t1_pain_duration_yrs", outcome="0-5 years")
    t2[11, i+1] <- percent_ci(tmp, "... 6-15 years", "t1_pain_duration_yrs", outcome="6-15 years")
    t2[12, i+1] <- percent_ci(tmp, "...>15 years", "t1_pain_duration_yrs", outcome=">15 years")
    
    t2[13, i+1] <- mean_sd(tmp, "Pain severity score", "BPI_PScore")
    t2[14, i+1] <- mean_sd(tmp, "Pain interference", "BPI_interference")
    t2[15, i+1] <- mean_sd(tmp, "Pain: self-efficacy questionnaire score", "PSEQ_Score")

    t2[17, i+1]  <- percent_ci(tmp, "Arthritis or rheumatism", "Arth_12m", outcome=1)
    t2[18, i+1] <- percent_ci(tmp, "Back or neck problems", "Back_12m", outcome=1)
    t2[19, i+1] <- percent_ci(tmp, "Frequent/severe headaches", "Head_12m", outcome=1)
    t2[20, i+1] <- percent_ci(tmp, "Visceral", "Visc_12m", outcome=1)
    
    tmp$num_chronic_cond_12m_bin <- tmp$num_chronic_cond_12m > 1
    t2[21, i+1]  <- percent_ci(tmp, "% more than one pain condition", "num_chronic_cond_12m_bin", outcome=TRUE)
    t2[22, i+1]  <- mean_sd(tmp, "Mean SF-12 PCS", 'SF12_PCS')
    
    # mental health
    t2[24, i+1]  <- percent_ci(tmp, "% currently moderate/severe depression", "depression", outcome=1)
    t2[25, i+1]  <- percent_ci(tmp, "% currently moderate/severe generalised anxiety disorder", "GAD", outcome=1)
    #t2[26, i+1]  <- percent_ci(tmp, "% meeting lifetime ICD-10 substance disorder", "t1_Any_Lifetime_Substance_Disorder", outcome = 1)
    t2[27, i+1]  <- percent_ci(tmp, "% meeting lifetime ICD-10 substance dependence", "t1_Any_lifetime_substance_dep_noalcohol", outcome = 1)
    t2[28, i+1]  <- mean_sd(tmp, "SF12 mental component score", "SF12_MCS")
    
    #other medicines
    t2[30, i+1]  <- median_iqr(tmp, "Median years using prescribed opioids", "t1_opcon_yrs")
    t2[31, i+1]  <- percent_ci(tmp, "% benzodiazepines", "benzo_week", outcome = 1)
    t2[32, i+1]  <- percent_ci(tmp, "% antidepressants", "Antidepressant_week", outcome = 1)
    t2[33, i+1]  <- percent_ci(tmp, "% antipsychotics", "Antipsychotic_week", outcome = 1)
    
    # harms
    t2[35, i+1]  <- percent_ci(tmp, "% overdose", "overdose", outcome = 1)
    t2[36, i+1]  <- percent_ci(tmp, "% score 1 or more on orbit", "orbit", outcome = 1)
    t2[37, i+1]  <- percent_ci(tmp, "% opioid dependence", "Pharm_Opioids_Dep_ICD10", outcome = 1)
    t2[38, i+1]  <- percent_ci(tmp, "% pods", "pods", outcome = 1)
    
    #pci <- percent_ci(tmp, "% OME 1-49mg", "ome_group", outcome = 1) # "1-49mg"
    #pci <- percent_ci(tmp, "% OME 50-89mg", "ome_group", outcome = 2) # "50-89mg"
    #pci <- percent_ci(tmp, "% OME 90-199mg", "ome_group", outcome = 3 ) # "90-199mg"
    #pci <- percent_ci(tmp, "% OME +200mg", "ome_group", outcome = 4) # "+200mg"
  }
  return(t2)
}

##############################################################################
# Run 
t2 <- table2(baseline)
print(t2)
##############################################################################
# save
write.csv(x = t2, file = "output/results/t2.csv", row.names = F)
##############################################################################
#################################### END #####################################
##############################################################################
