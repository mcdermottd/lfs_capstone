######################################################################
# notes:
# - purpose: format and structure dpi data, merge with school covariates
# - inputs: raw dpi data set, school covariate set, file with wkce avgs (to standardize scores)
# - outputs: analysis set (with necessary vars) and set with all vars
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

  # load ed outcomes data
  in_stacked_dpi <- fread("X:/LFS-Education Outcomes/data/raw_data/DCFmatchedSample03012016.csv")
  
  # load school attributes
  in_school_att <- fread("X:/LFS-Education Outcomes/data/raw_data/DPI_SCHOOL_ATTRIBUTES.csv")
  
  # load wkce summary score info
  wkce_avgs <- fread("X:/LFS-Education Outcomes/documents/wkce_summary_stats.csv")
  
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

  # create flag if hs student #brule
  format_stacked_dpi[, flag_hs := ifelse(grade_level_cd %in% c("08", "09", "10", "11", "12"), 1, 0)]
  
###############################################
# subset to one row per student per acad year #
###############################################
  
  # sort based on child id, school year, and test scores (to remove rows with missing scores first) #brule
  setorder(format_stacked_dpi, lds_student_key, child_id, acad_year, math_kce_scale_score, rdg_kce_scale_score, na.last = TRUE)
  
  # subset based on ohc students or not
  dpi_compare <- subset(format_stacked_dpi, is.na(child_id))
  dpi_ohc <- subset(format_stacked_dpi, !is.na(child_id))

  # remove duplicates based on id var and acad_year #brule (in some cases, multiple child_ids linked to one lds_student_key)
  dpi_compare_no_dups <- ea_no_dups(dpi_compare, c("lds_student_key", "acad_year"))
  dpi_ohc_no_dups <- ea_no_dups(dpi_ohc, c("lds_student_key", "acad_year"))  

  # second remove duplicates for ohc data #brule (in some cases, multiple child_ids linked to one lds_student_key)
  dpi_ohc_no_dups <- ea_no_dups(dpi_ohc_no_dups, c("child_id", "acad_year"))

  # re-stack two sets
  stacked_dpi_no_dups <- rbind(dpi_compare_no_dups, dpi_ohc_no_dups)

###########################
# standardize test scores #
###########################

  # copy wkce avg file
  wkce_avgs_merge <- copy(wkce_avgs)
  
  # create vars for merge
  wkce_avgs_merge[, ":="(grade_level_cd = as.character(grade), acad_year = as.character(acad_year))]
  
  # add additional 0 to grade var for merge
  wkce_avgs_merge[nchar(grade_level_cd) == 1, grade_level_cd := paste0("0", grade_level_cd)]
  
  # merge avgs with main data set
  stacked_dpi_standard <- ea_merge(stacked_dpi_no_dups, wkce_avgs_merge, c("acad_year", "grade_level_cd"), "x")
  
  # create z-scored scores
  stacked_dpi_standard[, zscore_math_kce := (math_kce_scale_score - math_score_mean) / math_score_sd]
  stacked_dpi_standard[, zscore_rdg_kce := (rdg_kce_scale_score - rdg_score_mean) / rdg_score_sd]

#################################
# create proficiency level vars #
#################################
  
  # create math proficiency level
  stacked_dpi_standard[math_kce_scale_score >= math_cp_advanced, perf_level_math := 4]
  stacked_dpi_standard[is.na(perf_level_math) & math_kce_scale_score >= math_cp_proficient, perf_level_math := 3]
  stacked_dpi_standard[is.na(perf_level_math) & math_kce_scale_score >= math_cp_basic, perf_level_math := 2]
  stacked_dpi_standard[is.na(perf_level_math) & !is.na(math_kce_scale_score), perf_level_math := 1]

  # create reading proficiency level
  stacked_dpi_standard[rdg_kce_scale_score >= rdg_cp_advanced, perf_level_rdg := 4]
  stacked_dpi_standard[is.na(perf_level_rdg) & rdg_kce_scale_score >= rdg_cp_proficient, perf_level_rdg := 3]
  stacked_dpi_standard[is.na(perf_level_rdg) & rdg_kce_scale_score >= rdg_cp_basic, perf_level_rdg := 2]
  stacked_dpi_standard[is.na(perf_level_rdg) & !is.na(rdg_kce_scale_score), perf_level_rdg := 1]
  
############################
# format school covariates #
############################
  
  # copy raw data
  school_att <- copy(in_school_att)
  
  # change var names to lowercase
  setnames(school_att, tolower(colnames(school_att)))
  
  # create scaler to convert vars
  school_att[, scaler := pupil_count / 1000]
  
  # create values scaled per 1000 students
  school_att[, ":="(frl_scaled = frl_count / scaler, sped_scaled = swd_count / scaler,  elp_scaled = (pupil_count - noelp_count) / scaler,
                    non_white_scaled = (pupil_count - race_w_count) / scaler, removal_scaled = totalremovals_total / scaler, 
                    dist_acctbl_code_cd = as.numeric(distid), sch_acctbl_code_cd = as.numeric(schid), acad_year = as.character(year))]
  
  # subset to covariates
  school_covar <- subset(school_att, select = c(acad_year, dist_acctbl_code_cd, sch_acctbl_code_cd, county_name, pupil_count, fte1000, frl_scaled,
                                                sped_scaled, elp_scaled, non_white_scaled, removal_scaled, mean_math_z_score, mean_rdg_z_score))

  # rename vars for merge
  rename_vars <- c("county_name", "pupil_count", "fte1000", "frl_scaled", "sped_scaled", "elp_scaled", "non_white_scaled", "removal_scaled", 
                   "mean_math_z_score", "mean_rdg_z_score")
  setnames(school_covar, rename_vars, paste0("sch_", rename_vars))

  # melt school covariates long to summarize
  school_covar_long <- melt.data.table(school_covar, id.vars = c("acad_year", "dist_acctbl_code_cd", "sch_acctbl_code_cd", "sch_county_name"))

  # calc stats for school covariates
  a_summ_sch_covariates <- school_covar_long[!is.na(value), list(n_obs = length(value),
                                                                   min = min(value),
                                                                   q25 = quantile(value, .25),
                                                                   q50 = quantile(value, .5),
                                                                   q75 = quantile(value, .75),
                                                                   max = max(value),
                                                                   mean = round(mean(value), 3),
                                                                   var = round(var(value), 3),
                                                                   sd = round(sd(value), 3)), 
                                              by = c("variable")]

  # merge with latest year set
  full_dpi_set <- ea_merge(stacked_dpi_standard, school_covar, c("acad_year", "dist_acctbl_code_cd", "sch_acctbl_code_cd"), "x")
  
#####################
# format and export #
#####################
  
  # subset to analysis vars
  out_stacked_dpi <- subset(full_dpi_set, select = c(lds_student_key, child_id, d_male, d_female, d_elp, d_sped, d_frl, d_fpl, d_rpl, d_race_white,
                                                     d_race_black, d_race_hispanic, d_race_asian, d_race_indian, d_race_missing, acad_year, 
                                                     dist_acctbl_code_cd, sch_acctbl_code_cd, sch_county_name, age_in_years_cd, grade_level_cd, 
                                                     flag_hs, att_rate_wi, days_removed_os, incidents_os, test_date, zscore_math_kce, perf_level_math,
                                                     zscore_rdg_kce, perf_level_rdg, sch_pupil_count, sch_fte1000, sch_frl_scaled, sch_sped_scaled,
                                                     sch_elp_scaled, sch_non_white_scaled, sch_removal_scaled, sch_mean_math_z_score, 
                                                     sch_mean_rdg_z_score))

  # sort by child id and acad year
  setorder(out_stacked_dpi, lds_student_key, acad_year)
  setorder(full_dpi_set, lds_student_key, acad_year)

  # export
  if (p_opt_exp == 1) { 

    # output school covariate avgs
    ea_write(a_summ_sch_covariates, "X:/LFS-Education Outcomes/qc/sch_covar_avgs.csv")

    # output Rdata files
    save(out_stacked_dpi, file = "X:/LFS-Education Outcomes/data/lfs_interim_sets/dpi_analysis_set.rdata")
    save(full_dpi_set, file = "X:/LFS-Education Outcomes/data/lfs_interim_sets/dpi_format_full_set.rdata")

    # output csv files
    # ea_write(out_stacked_dpi, "X:/LFS-Education Outcomes/data/lfs_interim_sets/dpi_analysis_set.csv")
    # ea_write(full_dpi_set, "X:/LFS-Education Outcomes/data/lfs_interim_sets/dpi_format_full_set.csv")

  }
  
