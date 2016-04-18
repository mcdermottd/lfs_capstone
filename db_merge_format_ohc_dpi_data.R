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

#######################################
# create dummies for dpi demographics #
#######################################
  
  # create race dummies
  format_stacked_dpi[!is.na(gender_code_cd), d_male := ifelse(gender_code_cd == "M", 1, 0)]
  format_stacked_dpi[!is.na(gender_code_cd), d_female := ifelse(gender_code_cd == "F", 1, 0)]

  # convert elp scale to dummy variable
  format_stacked_dpi[!is.na(elp_code_cd), d_elp := ifelse(elp_code_cd <= 5, 1, 0)]
  
  # convert disability code to dummy variable
  format_stacked_dpi[!is.na(disab_ye), d_sped := ifelse(disab_ye == "N", 0, 1)]
  
  # convert frl code to dummy variable
  format_stacked_dpi[!is.na(frl_ye), d_frl := ifelse(frl_ye == "N", 0, 1)]
  format_stacked_dpi[!is.na(frl_ye), d_fpl := ifelse(frl_ye == "F", 1, 0)]
  format_stacked_dpi[!is.na(frl_ye), d_rpl := ifelse(frl_ye %in% c("A", "R"), 1, 0)]

  # dummy out race variable
  format_stacked_dpi <- db_dummy(format_stacked_dpi, "race_eth_code_cd", opt_data_frequency = 0)
  
  # rename dummied race variables
  setnames(format_stacked_dpi, c("d_race_eth_code_cd_W", "d_race_eth_code_cd_missing", "d_race_eth_code_cd_I", "d_race_eth_code_cd_B", 
                                 "d_race_eth_code_cd_H", "d_race_eth_code_cd_A"), c("d_race_white", "d_race_missing", "d_race_indian", "d_race_black",
                                                                                 "d_race_hispanic", "d_race_asian"))
  
  # set all race variables to missing, if race_missing == 1
  format_stacked_dpi[d_race_missing == 1, c("d_race_white", "d_race_missing", "d_race_indian", "d_race_black", "d_race_hispanic", 
                                            "d_race_asian") := NA]

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

###########################
# standardize test scores #
###########################
  
  # subset to necessary vars for melt
  score_data_to_melt <- subset(dpi_acad_info, select = c(lf_child_id, acad_year, grade_level_cd, math_kce_scale_score, rdg_kce_scale_score))
  
  # melt test score data long to summarize
  score_data_long <- melt.data.table(score_data_to_melt, id.vars = c("lf_child_id", "acad_year", "grade_level_cd"))
  
  # sort data
  setorder(score_data_long, grade_level_cd, acad_year)
  
  # calc test stats by grade and acad yr
  a_kce_stats <- score_data_long[!is.na(value), list(n_obs = length(value),
                                                     min = min(value),
                                                     q25 = quantile(value, .25),
                                                     q50 = quantile(value, .5),
                                                     q75 = quantile(value, .75),
                                                     max = max(value),
                                                     mean = round(mean(value), 3),
                                                     var = round(var(value), 3),
                                                     sd = round(sd(value), 3)), 
                                 by = c("grade_level_cd", "acad_year", "variable")]
  
  # subset to vars for standardization
  sub_kce_stats <- subset(a_kce_stats, select = c(grade_level_cd, acad_year, variable, mean, sd))
  
  # cast wide by academic year
  kce_standardize <- data.table::dcast(sub_kce_stats, grade_level_cd + acad_year ~ variable, value.var = c("mean", "sd"))
  
  # merge back with main set
  dpi_acad_info <- ea_merge(dpi_acad_info, kce_standardize, c("grade_level_cd", "acad_year"), "x")
  
  # create z-scored scores
  dpi_acad_info[, zscore_math_kce := (math_kce_scale_score - mean_math_kce_scale_score) / sd_math_kce_scale_score]
  dpi_acad_info[, zscore_rdg_kce := (rdg_kce_scale_score - mean_rdg_kce_scale_score) / sd_rdg_kce_scale_score]
  
  # delete info for standardization
  dpi_acad_info[, c("mean_math_kce_scale_score", "mean_rdg_kce_scale_score", "sd_math_kce_scale_score", "sd_rdg_kce_scale_score") := NULL]
  
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

