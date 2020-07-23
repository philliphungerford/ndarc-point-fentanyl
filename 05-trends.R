##############################################################################
# Purpose: Trends
# Author: Phillip Hungerford
# Date: 06/03/2020
##############################################################################
# library
library(dplyr)
library(ggplot2)
# Specify dataset (dv.cc = complete case, dv.ac = available data)
load("data/data-fentanyl-03-processed.RData")
##############################################################################
percent_ci <- function(df, name, var, outcome="Yes", time_ind=0){
  df <- subset(df, time == time_ind)
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
  result <- c(estimate, lower, upper, time_ind)
  #result <- paste0(estimate, " (", lower, "-", upper, ")")
  
  # return results
  return(result)
}
##############################################################################
t0 <- percent_ci(df.cc, "Fentanyl", "Fentanyl", outcome=1, time_ind=0)
t1 <- percent_ci(df.cc, "Fentanyl", "Fentanyl", outcome=1, time_ind=1)
t2 <- percent_ci(df.cc, "Fentanyl", "Fentanyl", outcome=1, time_ind=2)
t3 <- percent_ci(df.cc, "Fentanyl", "Fentanyl", outcome=1, time_ind=3)
t4 <- percent_ci(df.cc, "Fentanyl", "Fentanyl", outcome=1, time_ind=4)
t5 <- percent_ci(df.cc, "Fentanyl", "Fentanyl", outcome=1, time_ind=5)

tall <- as.data.frame(rbind(t0,t1,t2,t3,t4,t5))
names(tall)[names(tall) == "V1"] <- 'estimate'
names(tall)[names(tall) == "V2"] <- 'lower'
names(tall)[names(tall) == "V3"] <- 'upper'
names(tall)[names(tall) == "V4"] <- 'time'
##############################################################################
p <- ggplot(data=tall, aes(x=time, y=estimate, ymin=lower, ymax=upper, group=1))+
  #geom_line(size = 8, colour="black",) +
  #geom_point(size=1, color='blue') +
  ylim(0, 20)+
  geom_ribbon(alpha=0.43, fill='blue') +
  geom_line(size=2, color='blue') +
  labs(title="",
       x = "Time",
       y = "Proportion of Fentanyl Users") + theme_bw()
# display plot 
plot(b)

# save plot 
ggsave(filename = "output/figures/fentanyl-trend.png", device = "png", height = 130, width = 150, units = "mm")
##############################################################################
#################################### END #####################################
##############################################################################