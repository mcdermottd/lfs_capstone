######################################################################
# notes:
# - purpose:
# - inputs:
# - outputs:
# - keywords:
# - general:
######################################################################

#######################################
# load packages and clear objects/log #
#######################################

  # load easimple and clear objects log
  library(easimple)
  ea_start()
  
  # load packages
  library(readxl)
  library(data.table)
  library(eaanalysis)

#############
# set parms #
#############

  # output toggle
  p_opt_exp <- 0

#############
# load data #
#############

  # load ed outcomes data
  in_ed_outcomes_set <- fread("X:/LFS-Education Outcomes/DCFmatchedSample03012016.csv")
  
  # load wide ohc file
  in_ohc_2008 <- data.table(read_excel("X:/LFS-Education Outcomes/dcf_wide_2008.xlsx"))
  
##################
# format ed data #
##################
  
  # copy ed data
  format_ed_outcomes <- copy(in_ed_outcomes_set)
  
  # change var names to lowercase
  setnames(format_ed_outcomes, colnames(format_ed_outcomes), tolower(colnames(format_ed_outcomes)))
  
##################
# format ed data #
##################
  
  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write( , ".csv")
    
  }

