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
  library(stargazer)
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
  
  # transform number of placement var
  analysis_set[, n_plcmt_sq := lf_n_plcmt_acad * lf_n_plcmt_acad]
  analysis_set[, n_plcmt_sqrt := sqrt(lf_n_plcmt_acad)]
  analysis_set[, n_plcmt_log := log(lf_n_plcmt_acad)]

  # transform placement days var
  analysis_set[, plcmt_days_sq := tot_plcmt_days_acad * tot_plcmt_days_acad]
  analysis_set[, plcmt_days_sqrt := sqrt(tot_plcmt_days_acad)]
  analysis_set[, plcmt_days_log := log(tot_plcmt_days_acad)]
  
  # transform school enrollment var
  analysis_set[, sch_pup_ct_log := log(sch_pupil_count)]

  # create hs subset
  analysis_set_hs <- subset(analysis_set, flag_hs == 1)

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

#############################
# set regression parameters #
#############################
  
  # set student control vars
  reg_student_controls <- c("age_in_years_cd", "d_male", "d_elp", "d_sped", "d_frl", "d_race_black", "d_race_hispanic", "d_race_asian", 
                            "d_race_indian")
  
  # set school covariates
  reg_sch_controls <- c("sch_pupil_count", "sch_frl_scaled", "sch_sped_scaled", "sch_elp_scaled", "sch_non_white_scaled", "sch_removal_scaled",
                        "sch_mean_math_z_score", "sch_mean_rdg_z_score")
  
  # set year and grade dummies
  reg_dummies <- c("d_acad_year_2009", "d_acad_year_2010", "d_acad_year_2011", "d_acad_year_2012", "d_grade_09", "d_grade_10", "d_grade_11",
                   "d_grade_12")
  
  # combine for full set of controls
  reg_controls_full <- paste(c(reg_student_controls, reg_sch_controls, reg_dummies), collapse = " + ")
  
############################
# regressions - attendence #
############################

  # reg: attendance on ohc flag and controls
  reg_formula <- paste("att_rate_wi ~ flag_cur_plcmt + flag_prior_plcmt + ", reg_controls_full)
  reg_attend_ohc <- lm(reg_formula, data = analysis_set_hs)

  # reg: attendance on number of placements
  reg_formula <- paste("att_rate_wi ~ flag_cur_plcmt + flag_prior_plcmt + ", reg_controls_full)
  reg_attend_ohc_var <- lm(reg_formula, data = analysis_set_hs)

  # reg: attendance on number of placements (+ sqrt) and total days
  reg_attend_ohc_var_sqrt <- lm(reg_formula, data = analysis_set_hs)

##########################
# regressions - removals #
##########################
  
  # reg: removals on ohc flag and controls
  reg_remove_ohc <- lm(days_removed_os ~ flag_ohc + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + d_race_asian + d_race_indian +
                              age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + per_sch_removal + sch_mean_math_z_score +
                              sch_mean_rdg_z_score, data = regression_set)
  
  # reg: removals on number of placements and total days
  reg_remove_ohc_var <- lm(days_removed_os ~ n_plcmt_acad + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + 
                             d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + 
                             per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, data = regression_set)
  
  # reg: removals on number of placements (+ sqrt) and total days
  reg_remove_ohc_var_sqrt <- lm(days_removed_os ~ n_plcmt_acad + n_plcmt_acad_sqrt + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + 
                                  d_race_hispanic + d_race_black + d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + 
                                  per_sch_elp + per_sch_non_white + per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, 
                                data = regression_set)

###########################
# regressions - wkce math #
###########################
  
  # reg: wkce math on ohc flag and controls
  reg_kce_math_ohc <- lm(zscore_math_kce ~ flag_ohc + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + d_race_asian + 
                                 d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + per_sch_removal +
                                 sch_mean_math_z_score + sch_mean_rdg_z_score, data = regression_set)
  
  # reg: wkce math on number of placements and total days
  reg_kce_math_ohc_var <- lm(zscore_math_kce ~ n_plcmt_acad + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + 
                             d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + 
                             per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, data = regression_set)
  
  # reg: wkce math on number of placements (+ sqrt) and total days
  reg_kce_math_ohc_var_sqrt <- lm(zscore_math_kce ~ n_plcmt_acad + n_plcmt_acad_sqrt + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + 
                                  d_race_hispanic + d_race_black + d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + 
                                  per_sch_elp + per_sch_non_white + per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, 
                                data = regression_set)

##############################
# regressions - wkce reading #
##############################
  
  # reg: wkce reading on ohc flag and controls
  reg_kce_rdg_ohc <- lm(zscore_rdg_kce ~ flag_ohc + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + d_race_asian + 
                                 d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + per_sch_removal +
                                 sch_mean_math_z_score + sch_mean_rdg_z_score, data = regression_set)
  
  # reg: wkce reading on number of placements and total days
  reg_kce_rdg_ohc_var <- lm(zscore_rdg_kce ~ n_plcmt_acad + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + 
                             d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + 
                             per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, data = regression_set)
  
  # reg: wkce reading on number of placements (+ sqrt) and total days
  reg_kce_rdg_ohc_var_sqrt <- lm(zscore_rdg_kce ~ n_plcmt_acad + n_plcmt_acad_sqrt + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + 
                                  d_race_hispanic + d_race_black + d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + 
                                  per_sch_elp + per_sch_non_white + per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, 
                                data = regression_set)

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
    
    # output outcome descriptive tables
    ea_write(a_acad_outcomes, paste0(p_dir_out, "acad_outcomes_overall_hs.csv"))
    ea_write(a_acad_outcomes_by_grd, paste0(p_dir_out, "acad_outcomes_by_grd.csv"))
    ea_write(a_acad_outcomes_by_type, paste0(p_dir_out, "acad_outcomes_by_type.csv"))
    ea_write(a_acad_outcomes_by_region, paste0(p_dir_out, "acad_outcomes_by_region.csv"))
    
    # output attendance models, full
    stargazer(reg_attend_ohc, reg_attend_ohc_var, reg_attend_ohc_var_sqrt, type = "html",
              dep.var.labels = "Attendance Rate",
              covariate.labels = c("OHC", "Placements in Academic Year", "Placements in Academic Year (SQRT)", "Days in Placement in Academic Year",
                                   "Male", "ELP", "SPED", "FRL", "Hispanic", "Black", "Asian", "Indian", "Age", "School - Percent FRL", 
                                   "School - Percent SPED", "School - Percent ELP", "School - Percent Non-White", "School - Number of Removals", 
                                   "School - Avg. Math Score", "School - Avg. Reading Score"),
              report = "vc*s",
              out = paste0(p_dir_out, "reg_attendance.htm"))

    # output attendence models, simplified
    stargazer(reg_attend_ohc, reg_attend_ohc_var, reg_attend_ohc_var_sqrt, type = "html",
              dep.var.labels = "Attendance Rate",
              covariate.labels = c("OHC", "Placements in Academic Year", "Placements in Academic Year (SQRT)", "Days in Placement in Academic Year"),
              omit = c("d_male", "d_elp", "d_sped", "d_frl", "d_race_hispanic", "d_race_black", "d_race_asian", "d_race_indian", "age_in_years_cd", 
                       "per_sch_frl", "per_sch_sped", "per_sch_elp", "per_sch_non_white", "per_sch_removal", "sch_mean_math_z_score", 
                       "sch_mean_rdg_z_score"),
              report = "vc*s",
              out = paste0(p_dir_out, "reg_attendance_condense.htm"))

    # output removal models, full
    stargazer(reg_remove_ohc, reg_remove_ohc_var, reg_remove_ohc_var_sqrt, type = "html",
              dep.var.labels = "Number of Removals",
              covariate.labels = c("OHC", "Placements in Academic Year", "Placements in Academic Year (SQRT)", "Days in Placement in Academic Year",
                                   "Male", "ELP", "SPED", "FRL", "Hispanic", "Black", "Asian", "Indian", "Age", "School - Percent FRL", 
                                   "School - Percent SPED", "School - Percent ELP", "School - Percent Non-White", "School - Number of Removals", 
                                   "School - Avg. Math Score", "School - Avg. Reading Score"),
              report = "vc*s",
              out = paste0(p_dir_out, "reg_removal.htm"))

    # output removal models, simplified
    stargazer(reg_remove_ohc, reg_remove_ohc_var, reg_remove_ohc_var_sqrt, type = "html",
              dep.var.labels = "Number of Removals",
              covariate.labels = c("OHC", "Placements in Academic Year", "Placements in Academic Year (SQRT)", "Days in Placement in Academic Year"),
              omit = c("d_male", "d_elp", "d_sped", "d_frl", "d_race_hispanic", "d_race_black", "d_race_asian", "d_race_indian", "age_in_years_cd", 
                       "per_sch_frl", "per_sch_sped", "per_sch_elp", "per_sch_non_white", "per_sch_removal", "sch_mean_math_z_score", 
                       "sch_mean_rdg_z_score"),
              report = "vc*s",
              out = paste0(p_dir_out, "reg_removal_condense.htm"))
    
    # output wkce models, full
    stargazer(reg_kce_math_ohc, reg_kce_math_ohc_var, reg_kce_math_ohc_var_sqrt, reg_kce_rdg_ohc, reg_kce_rdg_ohc_var, reg_kce_rdg_ohc_var_sqrt,
              type = "html",
              dep.var.labels = c("WKCE Score - Math (Standardized)", "WKCE Score - Reading (Standardized)"),
              covariate.labels = c("OHC", "Placements in Academic Year", "Placements in Academic Year (SQRT)", "Days in Placement in Academic Year",
                                   "Male", "ELP", "SPED", "FRL", "Hispanic", "Black", "Asian", "Indian", "Age", "School - Percent FRL", 
                                   "School - Percent SPED", "School - Percent ELP", "School - Percent Non-White", "School - Number of Removals", 
                                   "School - Avg. Math Score", "School - Avg. Reading Score"),
              report = "vc*s",
              out = paste0(p_dir_out, "reg_wkce.htm"))
    
    # output wkce models, simplified
    stargazer(reg_kce_math_ohc, reg_kce_math_ohc_var, reg_kce_math_ohc_var_sqrt, reg_kce_rdg_ohc, reg_kce_rdg_ohc_var, reg_kce_rdg_ohc_var_sqrt,
              type = "html",
              dep.var.labels = c("WKCE Score - Math (Standardized)", "WKCE Score - Reading (Standardized)"),
              covariate.labels = c("OHC", "Placements in Academic Year", "Placements in Academic Year (SQRT)", "Days in Placement in Academic Year"),
              omit = c("d_male", "d_elp", "d_sped", "d_frl", "d_race_hispanic", "d_race_black", "d_race_asian", "d_race_indian", "age_in_years_cd", 
                       "per_sch_frl", "per_sch_sped", "per_sch_elp", "per_sch_non_white", "per_sch_removal", "sch_mean_math_z_score", 
                       "sch_mean_rdg_z_score"),
              report = "vc*s",
              out = paste0(p_dir_out, "reg_wkce_condense.htm"))
  }
  
