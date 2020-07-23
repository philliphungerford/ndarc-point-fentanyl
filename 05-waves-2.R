##############################################################################
# Purpose: 03-waves
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
# Load libs
library(ggplot2) # for plotting
library(dplyr) # for %>% manipulation
library(tidyverse) # for reshape
##############################################################################
# Specify dataset (dv.cc = complete case, dv.ac = available data)
load("data/data-fentanyl-03-processed.RData")
df <- df.cc # should be the same either dataframe as waves is complete case

##############################################################################
# create wave calculator function
variable = "Fentanyl"
desired_value = 1
time_var = "time"
id_var = "Participant_ID"
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

## REMOVE THE 0 WAVES PEOPLE 
sub_wide <- subset(sub_wide, waves.used != 0) # N=153

# 6. create dataframe with results for waves
waves_df <- data.frame(table(sub_wide$waves.used))
waves_df$percent <- round((waves_df$Freq / nrow(sub_wide))*100,2)
names(waves_df)[names(waves_df) == "Var1"] <- "waves.used"

# 7. return results in list format where the first element is the waves table and 2nd are users
results <- vector("list", 2)
results[[1]] <- sub_wide
results[[2]] <- waves_df
results[[3]] <- sub_wide$Participant_ID[sub_wide$user]

##############################################################################

# prepare figure data
test <- results[[2]]
test <- test[, c("waves.used", "percent")]
test$category <- "Users"
print(test)

##############################################################################
# figure: stacked
#test <- test[order(test$waves.used, decreasing=T), ]
#test$waves = factor(test$waves.used, levels = c(6,5,4,3,2,1))

b <- ggplot(test, aes(x = category, y = percent, fill = fct_rev(waves.used))) +
  geom_bar(position="stack", stat="identity") + 
  # Set colours and legend title
  scale_fill_brewer("Waves\nUsed", 
                    type = 'qual',
                    palette = 3,
                    guide = guide_legend(reverse = TRUE)) + 
  #scale_fill_manual("Waves\nUsed", aesthetics = "fill", values=c("#009E73", "#56B4E9", "#E69F00", "#F0E442", "#0072B2", "#D55E00","#CC79A7")) +
  labs(title="", x ="", y = "Proportion of Participants") + 
  coord_flip() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "left",
        legend.direction = 'vertical')

# display plot 
plot(b)

# save plot 
ggsave(filename = "output/figures/waves-fentanyl-only.png", device = "png", height = 80, width = 200, units = "mm")
##############################################################################
#################################### END #####################################
##############################################################################