##############################################################################
# Purpose: Clean data Function
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################

clean_data <-function(df){
  # """
  # PURPOSE: function takes a dataframe and cleans it creating the following:
  ## median age
  ## 1 or more on ORBIT "ORBIT_PROB"
  ## ome >= 50
  ## median years prescribed opioids
  ## has_od which is just a binary of od_num_12m_clean
  # """ 
  ############################################################################
  # Clean
  ############################################################################
  # Create ever use
  df$Fentanyl <- as.numeric(df$Fentanyl)-1
  sub <- df[, names(df) %in% c("Participant_ID", 'time', "Fentanyl")] # 7152
  
  # 3. make the dataset wide
  sub_wide <- reshape(sub, idvar = 'Participant_ID' , timevar = 'time', direction="wide") # 1410 
  
  # 4. sum across waves
  sub_wide$waves.used <- rowSums(sub_wide[2:7], na.rm=T)
  
  # 5. get user
  sub_wide$user <- sub_wide$waves.used > 0
  ids <- sub_wide$Participant_ID[sub_wide$user] # n = 260
  
  # 6. create dataframe with results for waves
  waves_df <- data.frame(table(sub_wide$waves.used))
  waves_df$percent <- round((waves_df$Freq / nrow(sub_wide))*100,2)
  names(waves_df)[names(waves_df) == "Var1"] <- "waves.used"
  
  # 7. return results in list format where the first element is the waves table and 2nd are users
  results <- vector("list", 2)
  results[[1]] <- sub_wide
  results[[2]] <- waves_df
  results[[3]] <- sub_wide$Participant_ID[sub_wide$user]
  
  #create ever use variable 
  df$ever_use <- df$Participant_ID %in% ids
  ############################################################################
  # Dichotomise vars for analysis
  ## median age (median is 58)
  df$under_58 <- ifelse(df$t1_age < median(df$t1_age, na.rm=T), 1, 0)
  df$under_58 <- as.factor(df$under_58)
  
  ## 1 or more on ORBIT "ORBIT_PROB"
  df$orbit <- ifelse(df$ORBIT_cont > 0, 1, 0)
  df$orbit <- as.factor(df$orbit)
  
  ## ome >= 50 
  df$ome_50 <- ifelse(df$totalopioiddose >= 50, 1,0)
  df$ome_50 <- as.factor(df$ome_50)
  
  ## median years prescribed opioids (median is 4)
  df$t1_opcon_yrs <- df$t1_opcon/12
  df$median_prescribed <- ifelse(df$t1_opcon_yrs >= median(df$t1_opcon_yrs, na.rm=T), 1, 0)
  df$median_prescribed <- as.factor(df$median_prescribed)
  
  # pain severity 7+
  df$pain_severity <- ifelse(df$BPI_PScore >=7, 1, 0)
  
  # pain interference 7+ 
  df$pain_interference <- ifelse(df$BPI_interference >= 7, 1, 0)
  
  # pseq < 30
  df$pseq_low <- ifelse(df$PSEQ_Score < 30, 1, 0)
  
  # pods mod - high -> PODS_TOTbinary
  df$pods <- ifelse(df$PODS_TOT >= 8, 1, 0)
  
  #---------------------------------------------------------------------------
  ## 2.3. Mental Health
  
  # GAD mod-sev -> GADMod2Sev
  df$GAD <- ifelse(df$GAD_severity >= 10, 1, 0)
  
  # depression mod-sev -> PHQ9_Mod_sev >= 10
  df$depression <- ifelse(df$PHQ9_severity >=10, 1, 0)
  
  # pain duration
  df$t1_pain_duration_yrs <- df$t1_pain_duration/12
  df$t1_pain_duration_yrs_old = df$t1_pain_duration_yrs
  
  df$t1_pain_duration_yrs[df$t1_pain_duration_yrs_old >=0 & df$t1_pain_duration_yrs_old < 6] <- 1
  df$t1_pain_duration_yrs[df$t1_pain_duration_yrs_old > 5 & df$t1_pain_duration_yrs_old < 16] <- 2
  df$t1_pain_duration_yrs[df$t1_pain_duration_yrs_old > 15] <- 3
  
  df$t1_pain_duration_yrs <- ordered(df$t1_pain_duration_yrs, levels = 1:3,
                                     labels = c("0-5 years", "6-15 years", ">15 years")) # conversion
  
  # Employment
  df$Employ_old = df$Employ
  
  ## Employment fix (1:FT, 2:FTS, 3:HD, 6:PT, 8:STEMPLOYED)
  df <- df %>% mutate(Employ =
                        Employ_old == 1 | 
                        Employ_old == 2 | 
                        Employ_old == 3 |
                        Employ_old == 6| 
                        Employ_old == 8)
  
  df$Employ[df$Employ == FALSE] <- "OTHER"
  df$Employ[df$Employ == TRUE] <- "Employed"
  df$Employ[df$Employ_old == 4] <- "Not employed / on disability"
  df$Employ[df$Employ_old == 7] <- "Retired"
  table(df$time, df$Employ)
  
  # create unemployed var
  df$unemployed <- as.factor(ifelse(df$Employ == "Employed", 1,0))
  
  # keep only those who are alive (8694)
  df <- subset(df, death_ind==0)
  
  # remove variables 
  df <- df %>% select(-death_flag,
                      -death_ind)
  
  # create outcomes
  df$overdose <- ifelse(df$od_num_12m_clean > 0, 1, 0)
  
  # SF12 to binary by median (not sure what the cutoffs are)
  df$SF12_MCS_M <- ifelse(df$SF12_MCS >= median(df$SF12_MCS, na.rm=T), 1, 0)
  df$SF12_MCS_M <- as.factor(df$SF12_MCS_M)
  df$SF12_PCS_M <- ifelse(df$SF12_PCS >= median(df$SF12_PCS, na.rm=T), 1, 0)
  df$SF12_PCS_M <- as.factor(df$SF12_PCS_M)
  
  # Side effects
  se <- c(
    'constipation_degree',
    'drowsiness_degree',
    'fatigue_degree',
    'itching_degree',
    'mental_cloud_degree',
    'nausea_degree',
    #'sefct_other_degree',
    'sex_dys_degree',
    'sweating_degree',
    'vomiting_degree'
  )
  
  for (var in se){
    print("Cleaning side effects, cutting at none vs having side effects")
    print(var)
    print(table(df$time, df[,var]))
    # convert
    print("=============================================================================")
    df[, var] <- ifelse(df[,var] > 0,1,0) # none vs showing side effect
    #df.se[, var] <- ifelse(df.se[,var] > 1,1,0) # none/mild vs moderate/severe
    #df.se[, var] <- ifelse(df.se[,var] == 3,1,0) # none/mild/moderate vs severe
    print(table(df$time, df[,var]))
  }
  
  
  # Formats
  df <- df %>% mutate_at(c('orbit',
                           "depression",
                           "GAD",
                           "overdose",
                           'pain_interference',
                           'pain_severity',
                           'pseq_low',
                           'median_prescribed',
                           'Fentanyl',
                           'SF12_PCS_M',
                           'SF12_MCS_M'), as.factor)
  # return cleaned dataset
  return(df)
}
##############################################################################
#################################### END #####################################
##############################################################################