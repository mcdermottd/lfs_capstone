######################################################################
# notes:
# - purpose: merge ohc with dpi set
# - inputs: ohc data aggregated to one row per academic year, full stacked dpi data 
# - outputs: merged set, matched by academic year (+ control data), with unmatched ohc data removed
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
  # library(readstata13)
  library(data.table)

#############
# set parms #
#############

  # output toggle
  p_opt_exp <- 1

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
  
  # remove temporary lf ids
  format_stacked_dpi <- subset(format_stacked_dpi, select = -c(lf_dpi_id, lf_dcf_id))

###############################################
# subset to one row per student per acad year #
###############################################
  
  # sort based on child id, school year, and test scores (to remove rows with missing scores first) #brule
  setorder(format_stacked_dpi, lf_child_id, acad_year, math_kce_scale_score, rdg_kce_scale_score, na.last = TRUE)
   
  # remove duplicates based on child_id and school_year #brule
  stacked_dpi_no_dups <- ea_no_dups(format_stacked_dpi, c("lf_child_id", "acad_year"))
  
#####################################
# format ohc data to merge with dpi #
#####################################
  
  # copy ohc file
  stacked_ohc <- copy(in_stacked_ohc)
  
  # create id for merge
  setnames(stacked_ohc, "child_id", "lf_child_id")
  stacked_ohc[, lf_child_id := paste0("dcf_", lf_child_id)]
  
  # create set of child characteristics
  ohc_child_info <- subset(stacked_ohc, select = c(lf_child_id, child_gender, child_dob, child_race, child_ethnicity, child_hispanic, child_disability,
                                                   disabilities, icwa_child, n_ohc_tot, tot_ohc_days, first_ohc_start_date, last_ohc_end_date, 
                                                   n_plcmt_tot, tot_plcmt_days, first_pstart_date, last_pend_date))
  
  # remove duplicates on child id
  ohc_child_info <- ea_no_dups(ohc_child_info, opt_key_all = 1)
  
  # create ohc flag
  ohc_child_info[, flag_ohc := 1]
  
  # create set with academic year info
  ohc_acad_info <- subset(stacked_ohc, select = -c(child_gender, child_dob, child_race, child_ethnicity, child_hispanic, child_disability, 
                                                   disabilities, icwa_child, n_ohc_tot, tot_ohc_days, first_ohc_start_date, last_ohc_end_date, 
                                                   n_plcmt_tot, tot_plcmt_days, first_pstart_date, last_pend_date))
  
  # create ohc acad year flag
  ohc_acad_info[, flag_ohc_yr := 1]

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

###############################
# merge with dpi and ohc data #
###############################
  
  # merge dpi data with child info set
  dpi_ohc_data <- ea_merge(stacked_dpi_no_dups, ohc_child_info, "lf_child_id", "x")

  # fill in NAs with 0 for flag ohc
  dpi_ohc_data[is.na(flag_ohc), flag_ohc := 0]

  # subset out unmerged ohc acad outcomes #brule
  dpi_ohc_data <- subset(dpi_ohc_data, !(flag_ohc == 0 & ea_scan(lf_child_id, 1, "_") == "dcf"))
  unmerged_ohc <- subset(dpi_ohc_data, flag_ohc == 0 & ea_scan(lf_child_id, 1, "_") == "dcf")

  # create set of unduplicated, unmerged ids
  unmerged_ohc_ids <- subset(unmerged_ohc, select = c(lf_child_id, flag_ohc))
  unmerged_ohc_ids <- ea_no_dups(unmerged_ohc_ids, "lf_child_id", opt_print = 0)
  
  # create set of unduplicated, merged ids
  merged_ohc_ids <- subset(dpi_ohc_data, flag_ohc == 1, select = c(lf_child_id, flag_ohc))
  merged_ohc_ids <- ea_no_dups(merged_ohc_ids, "lf_child_id", opt_print = 0)
  
  # stack merged and unmerged ids
  ohc_ids <- rbind(merged_ohc_ids, unmerged_ohc_ids)
  
  # merge on placement data by academic year
  analysis_set <- ea_merge(dpi_ohc_data, ohc_acad_info, c("lf_child_id", "acad_year"), "x")

  # fill in NAs with 0 for flag ohc
  analysis_set[is.na(flag_ohc_yr), flag_ohc_yr := 0]

##########################
# format sets for export #
##########################
  
  # keep only constant over time variables
  dpi_ohc_no_dups <- subset(dpi_ohc_data, select = -c(school_year, age_in_years_cd, grade_level_cd, dist_acctbl_code_cd, sch_acctbl_code_cd, retained,
                                                      count_subm_districts_wi_ye, count_subm_schls_wi_ye, att_rate_wi, days_enrolled, days_removed_os,
                                                      incidents_os, days_removed_exp, incidents_ex, grade_level_wsas, dist_acctbl_code, 
                                                      sch_acctbl_code, test_date, math_kce_scale_score, rdg_kce_scale_score, math_accom_ind,
                                                      rdg_accom_ind))
  
  # subset to one row per child, keeping latest entry #brule
  setorder(dpi_ohc_no_dups, lf_child_id, -acad_year)
  dpi_ohc_no_dups <- ea_no_dups(dpi_ohc_no_dups, "lf_child_id", opt_print = 0)
  
  # delete acad year variable
  dpi_ohc_no_dups$acad_year <- NULL
  
  # reorder variables
  ea_colorder(analysis_set, c("lf_child_id", "lds_student_key", "flag_ohc", "flag_ohc_yr"))
  ea_colorder(dpi_ohc_data, c("lf_child_id", "lds_student_key", "flag_ohc"))
  ea_colorder(dpi_ohc_no_dups, c("lf_child_id", "lds_student_key", "flag_ohc"))

##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(analysis_set, "X:/LFS-Education Outcomes/data/lfs_data/analysis_set_full.csv")
    save(analysis_set, file = "X:/LFS-Education Outcomes/data/lfs_data/analysis_set_full.rdata")

    ea_write(dpi_ohc_data, "X:/LFS-Education Outcomes/data/lfs_data/analysis_set_no_plcmt.csv")
    save(dpi_ohc_data, file = "X:/LFS-Education Outcomes/data/lfs_data/analysis_set_no_plcmt.rdata")
    
    ea_write(dpi_ohc_no_dups, "X:/LFS-Education Outcomes/data/lfs_data/analysis_set_child_info.csv")
    save(dpi_ohc_no_dups, file = "X:/LFS-Education Outcomes/data/lfs_data/analysis_set_child_info.rdata")
    
    ea_write(ohc_ids, "X:/LFS-Education Outcomes/qc/ohc_merged_ids.csv")
    
  }

