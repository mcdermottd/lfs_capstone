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
  in_analysis_set <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

########################
# format analysis data #
########################

  # copy input sets
  analysis_set <- copy(in_analysis_set)

  # sort by academic year
  setorder(analysis_set, acad_year)
  
  # create combined school id var
  analysis_set[, lf_sch_id := paste0(dist_acctbl_code_cd, "_", sch_acctbl_code_cd)]

  # create hs subset
  analysis_set_hs <- subset(analysis_set, flag_hs == 1)
  
  # create set with only 7th and 9th graders with next year test score for wkce analysis
  analysis_set_wkce <- subset(analysis_set, (grade %in% c("07", "09") & !is.na(nxt_zscore_math_kce)))

##########################################
# examine acad outcomes - summary tables #
##########################################
  
  # sort by grade
  setorder(analysis_set, grade, -flag_ohc)

  # calc acad outcomes, by ohc status, grade
  a_acad_outcomes_by_grd <- analysis_set[, list(n_obs = .N,
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
  a_acad_outcomes <- analysis_set_hs[, list(n_obs = .N,
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

  # calc HS acad outcomes, by ohc status, placement type
  a_acad_outcomes_by_type <- analysis_set_hs[, list(n_obs = .N,
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
  
  # calc HS acad outcomes, by ohc status, region
  a_acad_outcomes_by_region <- analysis_set_hs[, list(n_obs = .N,
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

#####################
# format and export #
#####################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/outcomes/"
  
  # export
  if (p_opt_exp == 1) { 
    
    # output outcome descriptive tables
    ea_write(a_acad_outcomes, paste0(p_dir_out, "acad_outcomes_overall_hs.csv"))
    ea_write(a_acad_outcomes_by_grd, paste0(p_dir_out, "acad_outcomes_by_grd.csv"))
    ea_write(a_acad_outcomes_by_type, paste0(p_dir_out, "acad_outcomes_by_type.csv"))
    ea_write(a_acad_outcomes_by_region, paste0(p_dir_out, "acad_outcomes_by_region.csv"))
    
  }
  
