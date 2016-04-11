######################################################################
# notes:
# - purpose: merge ohc with dpi set
# - inputs: ohc data aggregated to latest acad year (one row per student), full stacked dpi data 
# - outputs: merged set, latest ohc placement data merged with dpi outcomes over multiple years (+ control data)
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
  library(foreign)
  library(readstata13)
  library(lubridate)
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
  in_stacked_ohc <- ea_load("X:/LFS-Education Outcomes/data/lfs_data/stacked_ohc_analysis_set.rdata")
  
  # load ed outcomes dat
  in_stacked_dpi <- read.dta13("X:/LFS-Education Outcomes/data/lfs_data/dpimerged_w.dta")
  
############################
# format dpi data to merge #
############################
  
  # copy dpi file
  format_stacked_dpi <- copy(in_stacked_dpi)
  
  # replace NA strings ("NA", "") with actual NA
  format_stacked_dpi[ format_stacked_dpi == "NA"] <- NA
  format_stacked_dpi[ format_stacked_dpi == ""] <- NA
  
  # delete unneeded variables
  format_stacked_dpi <- subset(format_stacked_dpi, select = -c(schoolyr, dups, dups_1, dups_2, `_merge`))

#######################
# merge with ohc data #
#######################
  
  # copy ohc file
  stacked_ohc <- copy(in_stacked_ohc)
  
  # subset to dpi data with merge id
  dpi_merge_data <- subset(format_stacked_dpi, !is.na(child_id))
  
  ##################
  ###### TEMP ######
  ##################

    # remove entries with duplicate child ids
    dpi_merge_data <- ea_no_dups(dpi_merge_data, "child_id", opt_delete_all = 1)
  
  ##################
  ###### TEMP ######
  ##################
  
  # merge ohc and dpi data
  merged_set <- ea_merge(dpi_merge_data, stacked_ohc, "child_id", "x")

  # create merge flag variable
  merged_set[, flag_merge := ifelse(is.na(acad_year), 0, 1)]

  # subset to unmerged records
  unmerged_set <- subset(merged_set, flag_merge == 0, select = c(child_id, lds_student_key))
  
  # remove dups to create unique unmerged ids
  unmerged_set <- ea_no_dups(unmerged_set, "child_id", opt_print = 0)
  
  # remove ohc demo vars, coming from dpi data #brule
  # ohc_dpi_data <- subset(ohc_dpi_data, select = -c(child_gender, child_race, child_ethnicity, child_hispanic, child_disability, disabilities, 
  #                                                 icwa_child, fl_mntal_retardatn, fl_phys_disabled, fl_vis_hearing_impr, fl_emotion_dstrbd, 
  #                                                 fl_othr_spc_care, fl_lrn_disability, child_level_of_need))
  
  # create controls set
  controls_set <- subset(format_stacked_dpi, is.na(child_id))
  
  # stack merged set with controls
  full_set <- rbind(merged_set, controls_set, fill = TRUE)
  
  # create OHC flag
  full_set[, flag_ohc := ifelse(!is.na(child_id), 1, 0)]

  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(full_set, "X:/LFS-Education Outcomes/data/lfs_data/combined_dpi_dcf_set.csv")
    
    ea_write(unmerged_set, "X:/LFS-Education Outcomes/qc/unmerged_id_set.csv")
    ea_write(qc_merge_rate, "X:/LFS-Education Outcomes/qc/merge_rates_by_yr.csv")
    ea_write(qc_id_merge, "X:/LFS-Education Outcomes/qc/merge_rates_by_id.csv")
    
  }

