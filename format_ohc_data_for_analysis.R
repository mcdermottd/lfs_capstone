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
  
  # load long ohc file
  in_ohc_2012 <- fread("X:/LFS-Education Outcomes/raw_data/dcf_placement_2012.csv")
  
  # load id xwalk
  in_id_xwalk <- fread("X:/LFS-Education Outcomes/raw_data/xwalk_child_id.csv")

############################
# remove duplicate entries #
############################
  
  # copy raw file
  sub_ohc_2012  <- copy(in_ohc_2012)
  
  # # create list of vars to check on duplicates
  # vars_dup <- colnames(sub_ohc_2012)
  # vars_nocheck <- c("site_region", "case_type", "child_person_type", "child_level_of_need", "provider_loc", "plcmt_with_sibling", 
  #                   "days_plcmt_in_rpt_period") 
  # vars_dup <- setdiff(vars_dup, vars_nocheck)
  # 
  # # subset based on duplicate vars list #brule
  # sub_ohc_2012 <- ea_no_dups(sub_ohc_2012, vars_dup)
  # 
  # # sort based on child id, placement end date, and discharge variables (reverse sort discharge so "yes" appears first)
  # setorder(sub_ohc_2012, child_id, plcmt_end_date, -discharge, discharge_reason, end_reason)
  # 
  # # remove discharge vars from no dup check
  # vars_dup <- setdiff(vars_dup, c("discharge", "discharge_reason", "end_reason"))
  # 
  # # remove additional duplicates ignoring discharge vars #brule
  # sub_ohc_2012 <- ea_no_dups(sub_ohc_2012, vars_dup)
  
  # remove duplicates based on id, and placement start and end date #brule
  sub_ohc_2012 <- ea_no_dups(sub_ohc_2012, c("child_id", "plcmt_begin_date", "plcmt_end_date"))
  
##########################
# subset to current year #
##########################
  
  # create placement year vars
  sub_ohc_2012[, plcmt_begin_year := as.numeric(ea_scan(plcmt_begin_date, 1, "-"))]
  sub_ohc_2012[, plcmt_end_year := as.numeric(ea_scan(plcmt_end_date, 1, "-"))]
  
  # create flag for in placement year
  sub_ohc_2012[, flag_in_year := 0]
  sub_ohc_2012[plcmt_begin_date <= 2012 & plcmt_end_year >= 2012, flag_in_year := 1]
  
  # subset to placements from current year
  sub_ohc_2012 <- subset(sub_ohc_2012, flag_in_year == 1)

  

  
  



  
  
  care_type
days_plcmt_in_rpt_period
child_level_of_need
site_region
end_reason
discharge
discharge_reason
  
  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write( , ".csv")
    
  }

