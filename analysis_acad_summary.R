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
  
  # create hs subset
  analysis_sample <- subset(full_outcomes_set, flag_analysis_grd == 1)

############################################
# acad outcomes - summary tables - general #
############################################
  
  # sort by grade
  setorder(full_outcomes_set, grade, -flag_ohc)

  # calc acad outcomes, by ohc status, grade
  a_acad_by_grd <- full_outcomes_set[, list(n_obs = .N,
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
                                by = c("flag_cur_plcmt", "flag_prior_plcmt", "grade")]
  
  # calc HS acad outcomes, by ohc status
  a_acad <- analysis_sample[, list(n_obs = .N,
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
                            by = c("flag_cur_plcmt", "flag_prior_plcmt")]

  # calc HS acad outcomes, by ohc status, gender
  a_acad_by_gender <- analysis_sample[, list(n_obs = .N,
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
                                      by = c("flag_cur_plcmt", "flag_prior_plcmt", "d_male")]
  
  # calc HS acad outcomes, by ohc status, region
  a_acad_by_region <- analysis_sample[, list(n_obs = .N,
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
                                           sd_incidents = round(sd(incidents_os, na.rm = TRUE), 3),
                                           avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                           sd_math_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3),
                                           avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                           sd_rdg_kce = round(sd(zscore_rdg_kce, na.rm = TRUE), 3)),
                                    by = c("flag_cur_plcmt", "p_type")]
  

  
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
  
