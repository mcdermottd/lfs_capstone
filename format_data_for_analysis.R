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
  library(data.table)

#############
# set parms #
#############

  # output toggle
  p_opt_exp <- 0

#############
# load data #
#############

  # load ed outcomes data
  in_ed_outcomes_set <- fread("X:/LFS-Education Outcomes/raw_data/DCFmatchedSample03012016.csv")
  
  # load long ohc file
  in_ohc_2012 <- fread("X:/LFS-Education Outcomes/raw_data/dcf_placement_2012.csv")
  
  # load id xwalk
  in_id_xwalk <- fread("X:/LFS-Education Outcomes/raw_data/xwalk_child_id.csv")

##################
# general format #
##################
  
  # copy raw files
  format_id_xwalk <- copy(in_id_xwalk)
  format_ed_outcomes <- copy(in_ed_outcomes_set)
  format_ohc_2012  <- copy(in_ohc_2012)

  # convert var names to lowercase
  setnames(format_ed_outcomes, colnames(format_ed_outcomes), tolower(colnames(format_ed_outcomes)))

###################
# format ohc data #
###################
  
  # create list of vars to check on duplicates
  vars_dup <- colnames(format_ohc_2012)
  vars_nocheck <- c("case_type", "end_reason", "child_level_of_need", "days_plcmt_in_rpt_period")
  vars_dup <- setdiff(vars_dup, vars_nocheck)
  
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

