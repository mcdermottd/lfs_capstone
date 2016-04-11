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
  # library(foreign)
  # library(readstata13)
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
  # in_stacked_dpi <- read.dta13("X:/LFS-Education Outcomes/data/lfs_data/dpimerged_w.dta")
  in_stacked_dpi <- fread("X:/LFS-Education Outcomes/data/raw_data/DCFmatchedSample03012016.csv")

############################
# format dpi data to merge #
############################
  
  # copy dpi file
  format_stacked_dpi <- copy(in_stacked_dpi)
  
  # remove exact duplicates #brule
  format_stacked_dpi <- ea_no_dups(format_stacked_dpi, opt_key_all = 1)
  
  # convert variable names to lowercase
  setnames(format_stacked_dpi, colnames(format_stacked_dpi), tolower(colnames(format_stacked_dpi)))
  
  # create academic year var
  format_stacked_dpi[, acad_year := paste0("20", ea_scan(school_year, 2, "-"))]
  
  # # replace NA strings ("NA", "") with actual NA
  # format_stacked_dpi[ format_stacked_dpi == "NA"] <- NA
  # format_stacked_dpi[ format_stacked_dpi == ""] <- NA
  # 
  # # delete unneeded variables
  # format_stacked_dpi <- subset(format_stacked_dpi, select = -c(schoolyr, dups, dups_1, dups_2, `_merge`))

########################################
# create new ids for merge / anaalysis #
########################################
  
  # create lf ids
  format_stacked_dpi[, ":="(lf_dpi_id = paste0("dpi_", lds_student_key), lf_dcf_id = paste0("dcf_", child_id))]

  # combine ids, use dcf id if not NA, otherwise use dpi id #brule
  format_stacked_dpi[, lf_child_id := ifelse(lf_dcf_id != "dcf_NA", lf_dcf_id, lf_dpi_id)]

###############################################
# subset to one row per student per acad year #
###############################################
  
  # sort based on child id, school year, and test scores (to remove rows with missing scores first) #brule
  setorder(format_stacked_dpi, lf_child_id, acad_year, math_kce_scale_score, rdg_kce_scale_score, na.last = TRUE)
   
  # remove duplicates based on child_id and school_year #brule
  stacked_dpi_no_dups <- ea_no_dups(format_stacked_dpi, c("lf_child_id", "acad_year"))
  
#######################
# merge with ohc data #
#######################
  
  # copy ohc file
  stacked_ohc <- copy(in_stacked_ohc)
  
  # create id for merge
  setnames(stacked_ohc, "child_id", "lf_child_id")
  stacked_ohc[, lf_child_id := paste0("dcf_", lf_child_id)]
  
  # create ohc flag
  stacked_ohc[, flag_ohc := 1]

  # # subset to dpi data with merge id
  # dpi_merge_data <- subset(format_stacked_dpi, !is.na(child_id))
  # 
  # ##################
  # ###### TEMP ######
  # ##################
  # 
  #   # remove entries with duplicate child ids
  #   dpi_merge_data <- ea_no_dups(dpi_merge_data, "child_id", opt_delete_all = 1)
  # 
  # ##################
  # ###### TEMP ######
  # ##################
  
  # merge ohc and dpi data
  merged_set <- ea_merge(stacked_dpi_no_dups, stacked_ohc, c("lf_child_id", "acad_year"), "x")

  # subset out unmerged ohc acad outcomes
  analysis_set <- subset(merged_set, !(is.na(flag_ohc) & ea_scan(lf_child_id, 1, "_") == "dcf"))
  unmerged_ohc <- subset(merged_set, is.na(flag_ohc) & ea_scan(lf_child_id, 1, "_") == "dcf")

  # create set of unduplicated, unmerged ids
  unmerged_ohc_ids <- subset(unmerged_ohc, select = c(lds_student_key, child_id))
  unmerged_ohc_ids <- ea_no_dups(unmerged_ohc_ids, "child_id", opt_print = 0)
  
  # add 0 value to flag_ohc for comparison students
  analysis_set[is.na(flag_ohc), flag_ohc := 0]

  # reorder variables
  ea_colorder(analysis_set, c("lf_child_id", "flag_ohc"))
  
  # remove ohc demo vars, coming from dpi data #brule
  # ohc_dpi_data <- subset(ohc_dpi_data, select = -c(child_gender, child_race, child_ethnicity, child_hispanic, child_disability, disabilities, 
  #                                                 icwa_child, fl_mntal_retardatn, fl_phys_disabled, fl_vis_hearing_impr, fl_emotion_dstrbd, 
  #                                                 fl_othr_spc_care, fl_lrn_disability, child_level_of_need))
  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(full_set, ".csv")
    ea_write(unmerged_ohc_ids, "X:/LFS-Education Outcomes/qc/unmerged_id_set.csv")
    
  }

