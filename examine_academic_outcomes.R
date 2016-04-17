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
  p_opt_exp <- 1

#############
# load data #
#############

  # load acad year info
  in_acad_year_data <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

###############
# format data #
###############

  # copy input sets
  acad_yr_data <- copy(in_acad_year_data)

  # change necessary vars to numeric
  acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad")] <- 
    lapply(acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad"), with = FALSE], as.numeric)
  
  # sort by academic year
  setorder(acad_yr_data, acad_year)
  
  # create avg days per placement vars
  acad_yr_data[, avg_days_plcmt_acad := tot_plcmt_days_acad / n_plcmt_acad]
  
###################################
# create flags for sample subsets #
###################################
  
  # create frl / non-frl flags for comparison groups
  acad_yr_data[, compare_frl := ifelse(flag_ohc == 0 & d_frl == 1, 1, 0)]

  # create flag if hs student
  acad_yr_data[, flag_hs := ifelse(grade_level_cd %in% c("08", "09", "10", "11", "12"), 1, 0)]
  
  # create hs subsets
  acad_yr_data_hs <- subset(acad_yr_data, flag_hs == 1 & flag_dpi_yr == 1)
  
##########################################
# examine acad outcomes - summary tables #
##########################################
  
  # sort by grade
  setorder(acad_yr_data, grade_level_cd, flag_ohc)
  setorder(acad_yr_data_hs, grade_level_cd, flag_ohc)

  # calc acad outcomes, by ohc status
  a_acad_outcomes <- acad_yr_data_hs[, list(n_obs = .N,
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
                                     by = flag_ohc]

  
  # calc acad outcomes, by ohc status, grade
  a_acad_outcomes_by_grd <- acad_yr_data[flag_dpi_yr == 1, list(n_obs = .N,
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
                                          by = c("flag_ohc", "grade_level_cd")]
#####################
# format and export #
#####################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/second_draft_exhibits/outcomes/"
  
  
  # set height and width of plots
  p_height <- 28
  p_width <- 28

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(a_acad_outcomes, paste0(p_dir_out, "acad_outcomes_overall_hs.csv"))
    ea_write(a_acad_outcomes_by_grd, paste0(p_dir_out, "acad_outcomes_by_grd.csv"))


  }
                          
