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
  
  # merge on academic outcomes to ohc data
  merged_output <- ea_merge(ohc_data_for_merge, format_stacked_dpi, "merge_id", "both", opt_out_mismatches = 1)
  
  # copy merged set
  ohc_dpi_data <- copy(merged_output$out_merged_data)
  
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
    
  }

