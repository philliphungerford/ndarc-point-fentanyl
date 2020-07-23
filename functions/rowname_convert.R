################################################################################
# PROGRAM: row name converter
# PURPOSE: Convert the bivariate results to table 
# WRITTEN BY: Phillip Hungerford
# DATE: 07/07/2020
##############################################################################
# We need a function that turns the row name into a column vector
rowname_converter <- function(df){
  require(data.table) # for setDT & rbindlist
  #"""
  # Takes a df and converts the row name to a column vector
  #"""
  df <- setDT(x = df, keep.rownames = TRUE)[]
  return(df)
}
##############################################################################
#################################### END #####################################
##############################################################################