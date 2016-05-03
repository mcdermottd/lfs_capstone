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

  # load analysis set
  in_outcomes_set <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

########################
# format analysis data #
########################

  # copy input sets
  full_outcomes_set <- copy(in_outcomes_set)

  # sort by academic year
  setorder(full_outcomes_set, acad_year)
  
  # create set with only analysis grades (7 - 12) #brule
  analysis_sample <- subset(full_outcomes_set, flag_analysis_grd == 1)
  
  # remove observations for ohc students occuring prior to a placement #brule
  analysis_sample <- subset(analysis_sample, !(flag_ohc == 1 & flag_cur_plcmt == 0 & flag_prior_plcmt == 0))
  
  # create subset with wkce scores
  wkce_sample <- subset(analysis_sample, !is.na(zscore_math_kce) | !is.na(nxt_zscore_math_kce))
  
############################################
# acad outcomes - summary tables - general #
############################################
  
  # sort by grade
  setorder(analysis_sample, grade, -flag_cur_plcmt, -flag_prior_plcmt)
  setorder(wkce_sample, grade, -flag_cur_plcmt, -flag_prior_plcmt)

  # calc acad outcomes for analysis sample, by ohc status
  a_acad <- analysis_sample[, list(n_obs = .N,
                                   avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                   avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                   avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                   avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3)),
                            by = c("flag_cur_plcmt", "flag_prior_plcmt")]
  
  
  # calc acad outcomes by gender
  a_acad_by_gender <- analysis_sample[, list(n_obs = .N,
                                             avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                             avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                             avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                             avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3)),
                                      by = c("flag_cur_plcmt", "flag_prior_plcmt", "d_male")]

  # calc acad outcomes, by ohc status, race
  a_acad_by_race <- analysis_sample[, list(n_obs = .N,
                                           avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                           avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                           avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                           avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3)),
                                    by = c("flag_cur_plcmt", "flag_prior_plcmt", "d_race_white")]
  
  # calc attendance and removals, by ohc status, grade
  a_acad_by_grd <- analysis_sample[, list(n_obs = .N,
                                           avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                           avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3)),
                                   by = c("flag_ohc", "grade")]
  
  # calc current and and next wkce scores by grade
  a_wkce_by_grd <- wkce_sample[, list(n_obs = .N,
                                      avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                      avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                      per_col_rdy = round(mean(flag_col_rdy, na.rm = TRUE), 3),
                                      avg_math_kce_nxt = round(mean(nxt_zscore_math_kce, na.rm = TRUE), 3),
                                      avg_rdg_kce_nxt = round(mean(nxt_zscore_rdg_kce, na.rm = TRUE), 3),
                                      per_col_rdy_nxt = round(mean(flag_col_rdy_nxt, na.rm = TRUE), 3)),
                        by = c("flag_ohc", "grade")]
  
  # calc acad outcomes, by ohc status, region
  a_acad_by_region <- analysis_sample[, list(n_obs = .N,
                                             avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                             avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                             avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                             avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                             per_col_rdy = round(mean(flag_col_rdy, na.rm = TRUE), 3)),
                                      by = c("flag_cur_plcmt", "flag_prior_plcmt", "lf_region")]
  
###################################################
# acad outcomes - summary tables - placement type #
###################################################
  
  # calc HS acad outcomes, by ohc status, placement type
  a_acad_by_type <- analysis_sample[, list(n_obs = .N,
                                           avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                           sd_atten = round(sd(att_rate_wi, na.rm = TRUE), 3),
                                           avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                           sd_remove = round(sd(days_removed_os, na.rm = TRUE), 3),
                                           avg_incidents = round(mean(incidents_os, na.rm = TRUE), 3),
                                           sd_incidents = round(sd(incidents_os, na.rm = TRUE), 3)),
                                    by = c("flag_cur_plcmt", "flag_prior_plcmt", "p_type")]
  
  a_wkce_by_type <- wkce_sample[, list(n_obs = .N,
                               avg_math_kce = round(mean(nxt_zscore_math_kce, na.rm = TRUE), 3),
                               sd_math_kce = round(sd(nxt_zscore_math_kce, na.rm = TRUE), 3),
                               avg_rdg_kce = round(mean(nxt_zscore_rdg_kce, na.rm = TRUE), 3),
                               sd_rdg_kce = round(sd(nxt_zscore_rdg_kce, na.rm = TRUE), 3)),
                      by = c("flag_cur_plcmt", "flag_prior_plcmt", "p_type")]
  
  
  # calc HS acad outcomes, by ohc status, gender and placement type
  a_acad_by_gender_type <- analysis_sample[, list(n_obs = .N,
                                                   avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                                   sd_atten = round(sd(att_rate_wi, na.rm = TRUE), 3),
                                                   avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                                   sd_remove = round(sd(days_removed_os, na.rm = TRUE), 3),
                                                   avg_incidents = round(mean(incidents_os, na.rm = TRUE), 3),
                                                   sd_incidents = round(sd(incidents_os, na.rm = TRUE), 3),
                                                   avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                                   sd_math_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3),
                                                   avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                                   sd_rdg_kce = round(sd(zscore_rdg_kce, na.rm = TRUE), 3)),
                                           by = c("flag_cur_plcmt", "flag_prior_plcmt", "d_male", "p_type")]
  
  # calc wkce scores by grade (8 and 10), gender, and placement type
  a_wkce_grade_gender_type <- analysis_sample[grade == "08" | grade == "10" , list(n_obs = .N,
                                                                                   avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                                                                   avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3)),
                                              by = c("flag_cur_plcmt", "flag_prior_plcmt", "grade", "d_male", "p_type")]
  
#####################
# format and export #
#####################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/outcomes/"
  
  # export
  if (p_opt_exp == 1) { 
    
    # output outcome descriptive tables
    ea_write(a_acad, paste0(p_dir_out, "acad_outcomes_overall_hs.csv"))
    ea_write(a_acad_by_grd, paste0(p_dir_out, "acad_outcomes_by_grd.csv"))
    ea_write(a_acad_by_type, paste0(p_dir_out, "acad_outcomes_by_type.csv"))
    ea_write(a_acad_by_region, paste0(p_dir_out, "acad_outcomes_by_region.csv"))
    ea_write(a_acad_by_gender, paste0(p_dir_out, "acad_outcomes_by_gender.csv"))
    ea_write(a_acad_by_gender_type, paste0(p_dir_out, "acad_outcomes_by_gender_type.csv"))
    ea_write(a_wkce_grade_gender_type, paste0(p_dir_out, "wkce_by_grade_gender_type.csv"))
    
  }
  
