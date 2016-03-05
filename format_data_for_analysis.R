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

  # load id xwalk
  in_id_xwalk <- data.table(read_excel("X:/LFS-Education Outcomes/xwalk_child_id.xlsx"))
  
  # load ed outcomes data
  in_ed_outcomes_set <- fread("X:/LFS-Education Outcomes/DCFmatchedSample03012016.csv")
  
  # load long ohc file
  in_ohc_2012_long <- data.table(read_excel("X:/LFS-Education Outcomes/dcf_placement_2012.xlsx"))
  
  # load wide ohc file
  in_ohc_2012_wide <- data.table(read_excel("X:/LFS-Education Outcomes/dcf_wide_2012.xlsx"))

##################
# general format #
##################
  
  # copy raw files
  format_id_xwalk <- copy(in_id_xwalk)
  format_ed_outcomes <- copy(in_ed_outcomes_set)
  format_ohc_2012  <- copy(in_ohc_2012_wide)
  format_ohc_2012_long  <- copy(in_ohc_2012_long)

  
  # convert var names to lowercase
  setnames(format_id_xwalk, colnames(format_id_xwalk), tolower(colnames(format_id_xwalk)))
  setnames(format_ed_outcomes, colnames(format_ed_outcomes), tolower(colnames(format_ed_outcomes)))
  setnames(format_ohc_2012, colnames(format_ohc_2012), tolower(colnames(format_ohc_2012)))
  setnames(format_ohc_2012_long, colnames(format_ohc_2012_long), tolower(colnames(format_ohc_2012_long)))

  # remove na rows from ohc data
  format_id_xwalk <- subset(format_id_xwalk, !is.na(new_id))
  format_ohc_2012 <- subset(format_ohc_2012, !is.na(child_id))
  format_ohc_2012_long <- subset(format_ohc_2012_long, !is.na(child_id))

##################
# format ed data #
##################
  

  
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

