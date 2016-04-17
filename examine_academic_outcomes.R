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
  library(ggplot2)
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

  # load acad year info
  in_acad_year_data <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

##########################################
# examine acad outcomes - summary tables #
##########################################
  
  # copy acad year info
  acad_yr_data <- copy(in_acad_year_data)
  
  # calc acad outcomes, by ohc status
  a_acad_outcomes <- acad_yr_data[flag_dpi_yr == 1, list(n_obs = .N,
                                                         avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                                         avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                                         avg_incidents = round(mean(incidents_os, na.rm = TRUE), 3),
                                                         avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                                         sd_math_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3),
                                                         avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                                         sd_rdg_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3)),
                                  by = flag_ohc]
  
  # sort by grade
  setorder(acad_yr_data, grade_level_cd, flag_ohc)
  
  # calc acad outcomes, by ohc status, grade
  a_acad_outcomes_by_grd <- acad_yr_data[flag_dpi_yr == 1, list(n_obs = .N,
                                                               avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                                               avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                                               avg_incidents = round(mean(incidents_os, na.rm = TRUE), 3),
                                                               avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                                               sd_math_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3),
                                                               avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                                               sd_rdg_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3)),
                                          by = c("flag_ohc", "grade_level_cd")]
#####################
# format and export #
#####################

  # set height and width of plots
  p_height <- 28
  p_width <- 28

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(a_acad_outcomes, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/acad_outcomes_overall.csv")
    ea_write(a_acad_outcomes_by_grd, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/acad_outcomes_by_grd.csv")


  }
                          
