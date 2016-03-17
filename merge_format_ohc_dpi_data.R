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
  in_stacked_ohc <- fread("X:/LFS-Education Outcomes/data/lfs_data/stacked_acad_yr_set.csv", colClasses = "character")
  
  # load ed outcomes data
  in_stacked_dpi <- fread("X:/LFS-Education Outcomes/data/raw_data/DCFmatchedSample03012016.csv")
  
  # load id xwalk
  in_id_xwalk <- fread("X:/LFS-Education Outcomes/data/raw_data/xwalk_child_id.csv")

############################
# format ohc data to merge #
############################
  
  # copy raw files
  format_stacked_ohc <- copy(in_stacked_ohc)
  format_id_xwalk <- copy(in_id_xwalk)
  
  # rename id vars in xwalk
  setnames(format_id_xwalk, c("new_id", "CHILD_ID"), c("child_id", "merge_id"))
  
  # remove id duplicates #brule
  format_id_xwalk <- ea_no_dups(format_id_xwalk, "child_id")
  
  # format child id var to merge
  format_id_xwalk[, child_id := as.character(child_id)]
  
  # merge with ohc data
  format_stacked_ohc <- ea_merge(format_id_xwalk, format_stacked_ohc, "child_id", "y")
  
###############################################
# remove duplicates, format dpi data to merge #
###############################################
  
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
  
  # create academic year var for merge
  format_stacked_dpi[, acad_year := as.character(paste0("20", ea_scan(school_year, 2, "-")))]

  # rename child id var for merge
  setnames(format_stacked_dpi, "child_id", "merge_id")

#######################
# merge with ohc data #
#######################
  
  # merge ohc and dpi data, on merge id and academic year var
  merged_set <- ea_merge(format_stacked_ohc, format_stacked_dpi, c("merge_id", "acad_year"), "x")

  # create merge flag variable
  merged_set[, flag_merge := 0]
  merged_set[!is.na(lds_student_key), flag_merge := 1]
  
  # create frequency of merge flag by year
  qc_merge_rate <- ea_table(merged_set, c("acad_year", "flag_merge"))

  # subset to unmerged records
  unmerged_set <- subset(merged_set, flag_merge == 0, select = c(child_id, merge_id, lds_student_key))
  
  # remove dups to create unique unmerged ids
  unmerged_set <- ea_no_dups(unmerged_set, "merge_id", opt_print = 0)
  
  
  # remove ohc demo vars, coming from dpi data #brule
  ohc_dpi_data <- subset(ohc_dpi_data, select = -c(child_gender, child_race, child_ethnicity, child_hispanic, child_disability, disabilities, 
                                                  icwa_child, fl_mntal_retardatn, fl_phys_disabled, fl_vis_hearing_impr, fl_emotion_dstrbd, 
                                                  fl_othr_spc_care, fl_lrn_disability, child_level_of_need))
  
  # create controls set
  controls_set <- subset(format_stacked_dpi, is.na(merge_id))
  
  # stack merged set with controls
  full_set <- rbind(ohc_dpi_data, controls_set, fill = TRUE)
  
  # create OHC flag
  full_set[, flag_ohc := 0]
  full_set[!is.na(merge_id), flag_ohc := 1]

  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(full_set, "X:/LFS-Education Outcomes/data/lfs_data/combined_dpi_dcf_set.csv")
    
    ea_write(unmerged_set, "X:/LFS-Education Outcomes/qc/unmerged_id_set.csv")
    ea_write(qc_merge_rate, "X:/LFS-Education Outcomes/qc/merge_rates_by_yr.csv")
    
  }

