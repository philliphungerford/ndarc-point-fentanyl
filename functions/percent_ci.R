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