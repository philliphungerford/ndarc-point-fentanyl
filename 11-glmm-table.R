##############################################################################
# PROGRAM: 06-bivariate-table.R
# PURPOSE: Convert the bivariate results to table 
# WRITTEN BY: Phillip Hungerford
# DATE: 07/07/2020
##############################################################################
# Setup
library(dplyr)
library(data.table) # for setDT & rbindlist
source("functions/rowname_convert.R")
##############################################################################
# Create function to convert
results_convert <- function(variable = "overdose"){
    #"""
    ## REQUIRES: rowname_converter function
    #"""
    #---------------------------------------------------------------------------
    # 1. Load the results to clean
    load("output/results/10-bivariate-imp.RData")
    
    #---------------------------------------------------------------------------
    # 2. need to turn the rownames into a column variable 
    results <- lapply(results, rowname_converter)
    
    #---------------------------------------------------------------------------
    # 3. convert the list of dfs to one big df 
    results_combined <- rbindlist(results)
    
    #---------------------------------------------------------------------------
    # 4. remove the unnecessary rows 
    # remove intercept values and time values
    results_combined = results_combined[!(results_combined$rn == '(Intercept)'),]
    results_combined = results_combined[!(results_combined$rn == 'time'),]
    
    #---------------------------------------------------------------------------
    # 5. create conf
    results_combined$conf <- paste0(
        format(round(results_combined$estimate,2),nsmall=2)," (", 
        format(round(results_combined$`2.5 %`,2),nsmall=2),"-",
        format(round(results_combined$`97.5 %`,2),nsmall=2),")")
    
    #---------------------------------------------------------------------------
    # 6. format p value too
    results_combined$p.value <- format(round(results_combined$p.value,2), nsmall=2)
    
    #---------------------------------------------------------------------------
    # 7. Rename to avoid confusion 
    names(results_combined)[names(results_combined) == "conf"] <- variable
    names(results_combined)[names(results_combined) == "p.value"] <- paste0(variable, "_p")
    
    #---------------------------------------------------------------------------
    # 8. Extract independent variables, confidence intervals and pvalues
    final <- results_combined[,c(1,9,6)]
    
    #---------------------------------------------------------------------------
    # 9. save output
    write.csv(x = final, 'output/results/11-bivariate-imp-table.csv')
    return(final)
}
##############################################################################
## RUN 
od_results <- results_convert(variable = "fentanyl")
##############################################################################
################################# END ########################################
##############################################################################

