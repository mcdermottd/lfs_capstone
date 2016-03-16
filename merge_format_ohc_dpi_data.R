######################################################################
# notes:
# - purpose:
# - inputs:
# - outputs:
# - keywords: #brule
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
  
  # load stacked ohc data
  in_stacked_ohc <- fread("X:/LFS-Education Outcomes/data/lfs_data/lst_set_agg_plcmt.csv")
  
  # load ed outcomes data
  in_stacked_dpi <- fread("X:/LFS-Education Outcomes/data/raw_data/DCFmatchedSample03012016.csv")

###################################
# remove duplicates from dpi data #
###################################
  
  # copy raw file
  format_stacked_dpi <- copy(in_stacked_dpi)
  
  # remove exact duplicates #brule
  format_stacked_dpi <- ea_no_dups(format_stacked_dpi, opt_key_all = 1)
  
  # convert variable names to lowercase
  setnames(format_stacked_dpi, colnames(format_stacked_dpi), tolower(colnames(format_stacked_dpi)))

  # sort based on child id, school year, and test scores (to remove rows with missing scores first) #brule
  setorder(format_stacked_dpi, lds_student_key, school_year, math_kce_scale_score, rdg_kce_scale_score, na.last = TRUE)
  
  # remove duplicates based on child_id and school_year #brule
  format_stacked_dpi <- ea_no_dups(format_stacked_dpi, c("lds_student_key", "school_year"))
  
  # rename child id var
  setnames(format_stacked_dpi, "child_id", "merge_id")

#######################
# merge with ohc data #
#######################
  
  # copy raw ohc file
  ohc_data_for_merge <- copy(in_stacked_ohc)
  
  # merge data sets
  merged_output <- ea_merge(ohc_data_for_merge, format_stacked_dpi, "merge_id", opt_out_mismatches = 1)
  
  # copy merged set
  merged_data <- copy(merged_output$out_merged_data)
  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write( , ".csv")
    
  }

