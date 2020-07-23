##############################################################################
# Purpose: Side effects analysis
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
# libraries
library(dplyr)
library(ggplot2)
source("functions/percent_ci.R")
# Specify dataset (dv.cc = complete case, dv.ac = available data)
load("data-fentanyl-03-processed2.RData")
##############################################################################
# grab side effects 24,9084
df.se <- df.cc %>% select(
  Participant_ID,
  time,
  followup,
  Fentanyl,
  ever_use,
  # degree available at all years (0;none, 1;mild, 2;moderate, 3;severe)
  sweating_degree,
  vomiting_degree, # too small
  constipation_degree,
  drowsiness_degree,
  fatigue_degree,
  itching_degree,
  mental_cloud_degree,
  nausea_degree,
  sex_dys_degree # too small
  # sefct_other_degree
)
# make time as numeric for model
df.se$time <- as.integer(df.se$time)-1
##############################################################################
# EXPLORE SIDE EFFECTS
# Characteristics names
chars <- c(
  'vomiting_degree', # too small
  'constipation_degree',
  'drowsiness_degree',
  'fatigue_degree',
  'itching_degree',
  'mental_cloud_degree',
  'nausea_degree',
  'sex_dys_degree'
)

# Specify names for variables for tidy appearance in table
se_names <- c(
  'Vomiting',
  'Constipation',
  'Drowsiness',
  'Fatigue',
  'Itching',
  'Mental cloudiness',
  'Nausea',
  'Sexual dysfunction'
)

##############################################################################
# Descriptives
##############################################################################
for (var in chars){
  print("###################################################################")
  print(var)
  for (i in 0:1){
    if(i == 1){
      print("Fentanyl Users")
    } else {
      print("Non-Fentanyl Users")
    }
    # for non fentanyl users, 
    tmp <- subset(df.cc, Fentanyl== i)
    # print their outcomes 
    print(table(tmp$time, tmp[,var]))
  }
}

##############################################################################
# create function 
table_se <- function(baseline){
  # create placeholder table 
  t2 <- data.frame('Characteristics' = chars, "Never" = NA, "Ever" = NA, "All" = NA)
  # Calculate for Never, ever and all (n=3)
  for (i in 1:3){
    if (i == 1) {
      # No fentanyl users
      print("Calculating for: Non-Fentanyl Users -------------------------------")
      tmp <- subset(baseline, ever_use == FALSE)
      tmp <- subset(baseline, Fentanyl == 0)
      N <- nrow(tmp)
      t2[1,2] <- paste0("N = ", N, "\n")
    } else if (i == 2) {
      # Fentanyl users
      print("Calculating for: Fentanyl Users -----------------------------------")
      tmp <- subset(baseline, ever_use == TRUE)
      tmp <- subset(baseline, Fentanyl == 1)
      N <- nrow(tmp)
      t2[1,3] <- paste0("N = ", N, "\n")
    } else {
      # all 
      print("Calculating for: Total -----------------------------------")
      tmp <- baseline
      N <- nrow(tmp)
      t2[1,4] <- paste0("N = ", N, "\n")
    }
    
    for (j in 1:length(chars)){
      t2[j, i+1] <- percent_ci(tmp, chars[j], chars[j], outcome=1)
    }
  }
  return(t2)
}
##############################################################################
# RUN
baseline <- subset(df.se, time == 0)
t_se <- table_se(baseline)
##############################################################################
# PLOT of mild to severe side effects. p=9 plots faceted by side effect
# Y: proportion
# X: time 
# group: fentanyl use
#=============================================================================
# we need to summarise the data beforehand

# build place holder
side_effect <- rep(chars, 12)
side_effect <- side_effect[order(side_effect, decreasing = FALSE)]

se_names <- rep(se_names, 12)
se_names <- se_names[order(se_names, decreasing = FALSE)]

se_df <- data.frame(
  'time'= rep(0:5, length(chars)*2),
  'side_effect' = side_effect,
  'se_name' = se_names,
  'Fentanyl' = rep(c(0,0,0,0,0,0,1,1,1,1,1,1), length(chars)),
  'Fentanyl_tmp' = rep(c(0,0,0,0,0,0,1,1,1,1,1,1), length(chars)),
  'prop' = NA
)

# for time point (t = 6)
for (t in 0:5){
  print("###################################################################")
  cat("TIME = ", t, "\n")
  print("###################################################################")
  # subset the data to that time period
  tmp1 <- subset(df.se, time == t)
  # now we get the proportions for the side effect
  ## for fentanyl and non fentanyl 
  
  for (var in chars){
    
    print("#===================================================================")
    cat("SIDE EFFECT = ", var, "\n")
    print("#===================================================================")
    # counts
    
    # FENTANYL USERS
    numerator_y <- sum(tmp1[which(tmp1$Fentanyl == 1), var], na.rm=T)# Fentanyl users with se = 20
    denominator_y <- sum(tmp1$Fentanyl == 1, na.rm = T) # Fentanyl users in total 162
    prop_fent_y <- (numerator_y / denominator_y)*100
    prop_fent_y <- as.numeric(format(round(prop_fent_y,2), nsmall=2))

    
    numerator_n <- sum(tmp1[which(tmp1$Fentanyl == 0), var], na.rm=T)# Fentanyl users with se = 170
    denominator_n <- sum(tmp1$Fentanyl == 0, na.rm = T) # Non Fentanyl users in total 967
    prop_fent_n <- (numerator_n / denominator_n)*100
    prop_fent_n <- as.numeric(format(round(prop_fent_n,2), nsmall=2))
    
    se_df$prop[which(se_df$time == t & se_df$side_effect == var & se_df$Fentanyl == 1)] <- prop_fent_y
    se_df$prop[which(se_df$time == t & se_df$side_effect == var & se_df$Fentanyl == 0)] <- prop_fent_n
    
    cat(var, "FENT YES: ", prop_fent_y, "\n")
    cat(var, "FENT NO: ", prop_fent_n, "\n")
    cat("\n")
  }
}
#-----------------------------------------------------------------------------
# Build plot

# recode for nicer plot 
se_df$opioid <- ifelse(se_df$Fentanyl == 1, "Fentanyl", "Non-Fentanyl")

p <- ggplot(data = se_df, aes(x = time, y = prop, color = opioid)) + 
  geom_line() +
  # geom_point(size=1) + 
  theme(legend.title = element_blank())  + # remove legend title
  scale_color_manual(values = c('#2c7bb6', '#d7191c')) + 
  labs(x = "Time (years)",
       y = "Proportion (%)") +
  facet_wrap(. ~ se_name)
##############################################################################
# Plot and save
plot(p)
#ggsave('side-effects.png', width = 190, height = 240, units = 'mm')
##############################################################################
# GLMM: Associations of fentanyl with side effects
##############################################################################
library(lme4)
for (i in 1:length(chars)){
  print("###################################################################")
  print(table(df.se$time, df.se[, chars[i]]))
  func <- paste0(chars[i], " ~ time + Fentanyl + (1 |Participant_ID)")
  print(func)
  fit <- glmer(func, data=df.se, family=binomial())
  print(summary(fit))
  se <- sqrt(diag(vcov(fit)))
  # table of estimates with 95% CI
  tab <- cbind(Est = fixef(fit), LL = fixef(fit) - 1.96 * se, UL = fixef(fit) + 1.96 * se)
  or <- format(round(exp(tab), 2), nsmall=2)
  results <- paste0(chars[i], ": ", or[3,1], " (", or[3,2], "-", or[3,3], ")")
  print(results)
}
##############################################################################
# save
write.csv(x = t_se, file = "output/results/t-side-effects.csv", row.names = F)
##############################################################################
#################################### END #####################################
##############################################################################
