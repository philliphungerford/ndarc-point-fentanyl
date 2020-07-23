##############################################################################
# Purpose: 03-waves
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
# Load libs
library(ggplot2) # for plotting
library(dplyr) # for %>% manipulation
##############################################################################
# Specify dataset (dv.cc = complete case, dv.ac = available data)
load("data/data-fentanyl-03-processed.RData")
df <- df.cc # should be the same either dataframe as waves is complete case

##############################################################################
# create wave calculator function
num_waves <- function(df, variable, desired_value, time_var, id_var){
  ###################################################################
  # This function takes longitudinal data and calculates
  # the number of waves a variable of interest has been presented
  # INPUTS: 
  ## df = is a longitudinal dataframe with variables of interest
  ## variable = the variable of interest must be dichotomous
  ## desired_value = the positive value (e.g. 0/1 is the 1 or yes/no is yes)
  ## time_var = the time variable of interest
  ## id_var = the id variable
  # OUTPUTS: 
  ## A list of three components:
  ### 1: Wide point dataframe of use across time
  ### 2. Summary of use over waves
  ### 3. IDs of users who did use
  # example_result <- num_waves(point, variable = "Fentanyl", desired_value = "yes", time_var = "wave", id_var = "Participant_ID")
  ###################################################################
  # 1. subset the data based on variables entered
  sub <- df %>% select(id_var, time_var, variable)
  
  # 2. convert to numeric if necessary
  sub[, variable] <- ifelse(sub[, variable] == desired_value,1,0)
  
  # 3. make the dataset wide
  sub_wide <- reshape(sub, idvar = id_var, timevar = time_var, direction="wide")
  sub_wide <- na.omit(sub_wide)
  
  # 4. sum across waves
  sub_wide$waves.used <- rowSums(sub_wide[2:7], na.rm=T)
  
  # 5. get user 
  sub_wide$user <- sub_wide$waves.used > 0
  
  # 6. create dataframe with results for waves
  waves_df <- data.frame(table(sub_wide$waves.used))
  waves_df$percent <- round((waves_df$Freq / nrow(sub_wide))*100,2)
  names(waves_df)[names(waves_df) == "Var1"] <- "waves.used"
  
  # 7. return results in list format where the first element is the waves table and 2nd are users
  results <- vector("list", 2)
  results[[1]] <- sub_wide
  results[[2]] <- waves_df
  results[[3]] <- sub_wide$Participant_ID[sub_wide$user]
  return(results)
}
##############################################################################
# calculate waves
num_waves_fentanyl <- num_waves(df, variable = "Fentanyl", desired_value = 1, time_var = "time", id_var = "Participant_ID")

# get the ids of people who used Fentanyl at least once 
ids <- num_waves_fentanyl[[3]]

# prepare figure data
test <- num_waves_fentanyl[[2]]
test <- test[, c("waves.used", "percent")]
test$category <- "Users"
print(test)

##############################################################################
# figure: stacked
b <- ggplot(test, aes(x = category, y = percent, fill = waves.used)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Number of Waves Fentanyl Patches Were Used",
       x = "",
       y = "Proportion of Participants") + 
  theme_bw() + scale_fill_manual("Waves Used", aesthetics = "fill", values=c("#009E73", "#56B4E9", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) 
plot(b + coord_flip())
##############################################################################
#################################### END #####################################
##############################################################################