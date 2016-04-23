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
  
  # load stacked ohc data
  in_stacked_ohc <- ea_load("X:/LFS-Education Outcomes/data/lfs_interim_sets/stacked_ohc_formatted.rdata")
  
  # load ed outcomes dat
  in_stacked_dpi <- fread("")
  
#######################################
# create new ids for merge / analysis #
#######################################
  
  # create lf ids
  format_stacked_dpi[, ":="(lf_dpi_id = paste0("dpi_", lds_student_key), lf_dcf_id = paste0("dcf_", child_id))]

  # combine ids, use dcf id if not NA, otherwise use dpi id #brule
  format_stacked_dpi[, lf_child_id := ifelse(lf_dcf_id != "dcf_NA", lf_dcf_id, lf_dpi_id)]
  
  # remove temporary lf ids
  format_stacked_dpi <- subset(format_stacked_dpi, select = -c(lf_dpi_id, lf_dcf_id))

###########################################
# separate demographics and acad outcomes #
###########################################
  
  # create set of child characteristics
  dpi_child_info <- subset(stacked_dpi_no_dups, select = c(lf_child_id, lds_student_key, child_id, d_male, d_female, d_elp, d_sped, d_frl, d_fpl, 
                                                           d_rpl, d_race_indian, d_race_missing, d_race_hispanic, d_race_white, d_race_black, 
                                                           d_race_asian, econ_disadv_code_cd, elp_code_cd, gender_code_cd, homeless_status_ind_cd, 
                                                           migrant_status_ind_cd, native_lang_code_cd, primary_disab_code_cd, race_eth_code_cd,
                                                           composite_eng_prof_lvl_cd, frl_cd, frl_ye, disab_cd, disab_ye)) 
  
  # remove duplicates on child id
  dpi_child_info <- ea_no_dups(dpi_child_info, "lf_child_id")
  
  # create set with academic year info
  dpi_acad_info <- subset(stacked_dpi_no_dups, select = -c(lds_student_key, child_id, d_male, d_female, d_elp, d_sped, d_frl, d_fpl, d_rpl, 
                                                           d_race_indian, d_race_missing, d_race_hispanic, d_race_white, d_race_black, d_race_asian,
                                                           econ_disadv_code_cd, elp_code_cd, gender_code_cd, homeless_status_ind_cd, 
                                                           migrant_status_ind_cd, native_lang_code_cd, primary_disab_code_cd, race_eth_code_cd,
                                                           composite_eng_prof_lvl_cd, frl_cd, frl_ye, disab_cd, disab_ye)) 

#####################################
# format ohc data to merge with dpi #
#####################################
  
  # copy ohc file
  stacked_ohc <- copy(in_stacked_ohc)
  
  # create id for merge
  setnames(stacked_ohc, "child_id", "lf_child_id")
  stacked_ohc[, lf_child_id := paste0("dcf_", lf_child_id)]
  
  # create set of child characteristics
  ohc_child_info <- subset(stacked_ohc, select = c(lf_child_id, child_gender, child_dob, child_race, child_ethnicity, child_hispanic, 
                                                   child_disability, disabilities, icwa_child, n_ohc_tot, tot_ohc_days, first_ohc_start_date, 
                                                   last_ohc_end_date, n_plcmt_tot, tot_plcmt_days, first_pstart_date, last_pend_date))
  
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

###################################
# merge child characteristic info #
###################################
  
  # merge dpi data with child info set
  dpi_ohc_child_info <- ea_merge(dpi_child_info, ohc_child_info, "lf_child_id")
  
  # subset to id vars
  ohc_ids_merge <- subset(dpi_ohc_child_info, ea_scan(lf_child_id, 1, "_") == "dcf", select = c(lf_child_id, lds_student_key, flag_ohc))
  
  # create flags based on merge
  ohc_ids_merge[, flag_merge := ifelse(!is.na(lds_student_key) & !is.na(flag_ohc), 1, 0)]
  ohc_ids_merge[, flag_no_dpi := ifelse(is.na(lds_student_key) & ea_scan(lf_child_id, 1, "_") == "dcf", 1, 0)]
  ohc_ids_merge[, flag_no_dcf := ifelse(is.na(flag_ohc) & ea_scan(lf_child_id, 1, "_") == "dcf", 1, 0)]

  # create frequency table of merge types
  a_merge_stats <- ea_table(ohc_ids_merge, c("flag_merge", "flag_no_dpi", "flag_no_dcf"), opt_percent = 1)

  # subset out unmerged data #brule
  dpi_ohc_child_info <- subset(dpi_ohc_child_info, (!is.na(child_id) & flag_ohc == 1) | ea_scan(lf_child_id, 1, "_") == "dpi")
  
  # fill in NAs with 0 for flag ohc
  dpi_ohc_child_info[is.na(flag_ohc), flag_ohc := 0]
  
###############################
# merge on academic year info #
###############################
  
  # merge on dpi acad year info
  dpi_ohc_acad <- ea_merge(dpi_ohc_child_info, dpi_acad_info, "lf_child_id", "x", opt_print = 0)
  
  # subset placement data to merged ids
  sub_ohc_acad <- ea_merge(ohc_acad_info, subset(ohc_ids_merge, flag_merge == 1), "lf_child_id", "both")
  
  # remove unneeded merge flags
  sub_ohc_acad[, c("lds_student_key", "flag_merge", "flag_no_dpi", "flag_no_dcf", "flag_ohc") := NULL]
  
  # merge on placement data by academic year
  analysis_set <- ea_merge(dpi_ohc_acad, sub_ohc_acad, c("lf_child_id", "acad_year"))

  # fill in missing OHC flags
  analysis_set[is.na(flag_ohc), flag_ohc := 1]
  analysis_set[is.na(flag_ohc_yr), flag_ohc_yr := 0]
  
  # create additional flag when acad outcomes are missing
  analysis_set[, flag_dpi_yr := ifelse(!is.na(lds_student_key), 1, 0)]

  # reorder variables
  ea_colorder(dpi_ohc_child_info, c("lf_child_id", "lds_student_key", "flag_ohc"))
  ea_colorder(analysis_set, c("lf_child_id", "lds_student_key", "child_id", "flag_ohc", "flag_ohc_yr", "flag_dpi_yr"))

##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(analysis_set, "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.csv")
    save(analysis_set, file = "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

    ea_write(dpi_ohc_child_info, "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set_child_info.csv")
    save(dpi_ohc_child_info, file = "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set_child_info.rdata")
    
    ea_write(a_kce_stats, "X:/LFS-Education Outcomes/qc/kce_stats_by_grade.csv")
    ea_write(kce_standardize, "X:/LFS-Education Outcomes/qc/zscore_statistics.csv")
    
    ea_write(ohc_ids_merge, "X:/LFS-Education Outcomes/qc/ohc_merged_ids.csv")
    ea_write(a_merge_stats, "X:/LFS-Education Outcomes/qc/ohc_merged_rates.csv")

  }

