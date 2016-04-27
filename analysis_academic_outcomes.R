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
  library(sandwich)
  library(lmtest)
  library(plm)
  library(multiwayvcov)
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
  
#############################
# set regression parameters #
#############################
  
  # set student control vars
  lm_student_controls <- c("age_in_years_cd", "d_male", "d_elp", "d_sped", "d_frl", "d_race_black", "d_race_hispanic", "d_race_asian", 
                            "d_race_indian")
  
  # set school covariates
  lm_sch_controls <- c("sch_pupil_count", "sch_frl_scaled", "sch_sped_scaled", "sch_elp_scaled", "sch_non_white_scaled", "sch_removal_scaled",
                        "sch_mean_math_z_score", "sch_mean_rdg_z_score")
  
  # set year dummies
  lm_dummies_yr <- c("d_acad_year_2009", "d_acad_year_2010", "d_acad_year_2011", "d_acad_year_2012")
  
  # set grade dummies
  lm_dummies_grade <- c("d_grade_09", "d_grade_10", "d_grade_11", "d_grade_12")

  # combine for full set of controls
  lm_controls_full <- paste(c(lm_student_controls, lm_sch_controls, lm_dummies_yr, lm_dummies_grade), collapse = " + ")
  lm_controls_wkce <- paste(c(lm_student_controls, lm_sch_controls, lm_dummies_yr, "d_grade_09"), collapse = " + ")
  
#################################
# define SE clustering function #
#################################
  
  func_cluster <- function(dat, fm, cluster) {
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M / (M - 1)) * ((N - 1) / (N - K))
    uj  <- apply(estfun(fm), 2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc * sandwich(fm, meat = crossprod(uj) / N)
    coeftest(fm, vcovCL)
  }
  
############################
# regressions - attendence #
############################

  sub_attend <- subset(analysis_set_hs, !is.na(att_rate_wi))
  
  # reg: attendance on ohc flags and controls
  lm_formula <- paste("att_rate_wi ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_full)
  lm_attend_ohc <- lm(lm_formula, data = analysis_set_hs)

  # reg: attendance on number of placements
  lm_formula <- paste("att_rate_wi ~ lf_n_plcmt_acad + flag_prior_plcmt + ", lm_controls_full)
  lm_attend_n_plcmt <- lm(lm_formula, data = analysis_set_hs)

  # reg: attendance on total placement days
  lm_formula <- paste("att_rate_wi ~ tot_plcmt_days_acad + flag_prior_plcmt + ", lm_controls_full)
  lm_attend_plcmt_days <- lm(lm_formula, data = analysis_set_hs)

  
    pm1 <- plm(as.formula(lm_formula), data = analysis_set_hs, model = "pooling")


  
  ztest <- lm(lm_formula, data = sub_attend)
  lm_attend_ohc_cl <- func_cluster(sub_attend, ztest, sub_attend$sch_acctbl_code_cd)
  
##########################
# regressions - removals #
##########################
  
  # reg: removals on ohc flags and controls
  lm_formula <- paste("days_removed_os ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_full)
  lm_remove_ohc <- lm(lm_formula, data = analysis_set_hs)

  # reg: removals on number of placements
  lm_formula <- paste("days_removed_os ~ lf_n_plcmt_acad + flag_prior_plcmt + ", lm_controls_full)
  lm_remove_n_plcmt <- lm(lm_formula, data = analysis_set_hs)

  # reg: removals on total placement days
  lm_formula <- paste("days_removed_os ~ tot_plcmt_days_acad + flag_prior_plcmt + ", lm_controls_full)
  lm_remove_plcmt_days <- lm(lm_formula, data = analysis_set_hs)

###########################
# regressions - wkce math #
###########################
  
  # reg: wkce math on ohc flag and controls
  lm_formula <- paste("nxt_zscore_math_kce ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_wkce)
  lm_kce_math_ohc <- lm(lm_formula, data = analysis_set_wkce)
  
  # reg: wkce math on number of placements
  lm_formula <- paste("nxt_zscore_math_kce ~ lf_n_plcmt_acad + flag_prior_plcmt + ", lm_controls_wkce)
  lm_kce_math_n_plcmt <- lm(lm_formula, data = analysis_set_wkce)

  # reg: wkce math on total placement days
  lm_formula <- paste("nxt_zscore_math_kce ~ tot_plcmt_days_acad + flag_prior_plcmt + ", lm_controls_wkce)
  lm_kce_math_plcmt_days <- lm(lm_formula, data = analysis_set_wkce)

##############################
# regressions - wkce reading #
##############################
  
  # reg: wkce reading on ohc flag and controls
  lm_formula <- paste("nxt_zscore_rdg_kce ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_wkce)
  lm_kce_rdg_ohc <- lm(lm_formula, data = analysis_set_wkce)
  
  # reg: wkce reading on number of placements
  lm_formula <- paste("nxt_zscore_rdg_kce ~ lf_n_plcmt_acad + flag_prior_plcmt + ", lm_controls_wkce)
  lm_kce_rdg_n_plcmt <- lm(lm_formula, data = analysis_set_wkce)

  # reg: wkce reading on total placement days
  lm_formula <- paste("nxt_zscore_rdg_kce ~ tot_plcmt_days_acad + flag_prior_plcmt + ", lm_controls_wkce)
  lm_kce_rdg_plcmt_days <- lm(lm_formula, data = analysis_set_wkce)
  
#####################
# format and export #
#####################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/outcomes/"
  
  # create vector of OHC variable labels
  ohc_var_lables <- c("Current OHC Placement", "Days in Placement in Academic Year", "Placements in Academic Year", "Past OHC Placement")
  
  # create vector of control variable labels
  control_var_labels <- c("Age", "Male", "ELP", "SPED", "FRL", "Black", "Hispanic", "Asian", "Indian", "School - Total Enrollment", 
                      "School - FRL Students Per 1,000", "School - SPED Students Per 1,000", "School - ELP Students Per 1,000", 
                      "School - Non-White Students Per 1,000", "School - Removals Per 1,000 Students", "School - Avg. Math Score", 
                      "School - Avg. Reading Score", "Acad. Year: 2009", "Acad. Year: 2010", "Acad. Year: 2011", "Acad. Year: 2012")
  
  # create vector of grade dummy labels
  grade_dummy_labels <- c("Grade 9", "Grade 10", "Grade 11", "Grade 12")

  # create vector of omit variables
  omit_vars <- c("age_in_years_cd", "d_male", "d_elp", "d_sped", "d_frl", "d_race_black", "d_race_hispanic", "d_race_asian", "d_race_indian", 
                 "sch_pupil_count", "sch_frl_scaled", "sch_sped_scaled", "sch_elp_scaled", "sch_non_white_scaled", "sch_removal_scaled", 
                 "sch_mean_math_z_score", "sch_mean_rdg_z_score", "d_acad_year_2009", "d_acad_year_2010", "d_acad_year_2011", "d_acad_year_2012",
                 "d_grade_09", "d_grade_10", "d_grade_11", "d_grade_12")
  
  # export
  if (p_opt_exp == 1) { 
    
    # output outcome descriptive tables
    ea_write(a_acad_outcomes, paste0(p_dir_out, "acad_outcomes_overall_hs.csv"))
    ea_write(a_acad_outcomes_by_grd, paste0(p_dir_out, "acad_outcomes_by_grd.csv"))
    ea_write(a_acad_outcomes_by_type, paste0(p_dir_out, "acad_outcomes_by_type.csv"))
    ea_write(a_acad_outcomes_by_region, paste0(p_dir_out, "acad_outcomes_by_region.csv"))
    
    # output attendance models, full
    stargazer(lm_attend_ohc, lm_attend_n_plcmt, lm_attend_plcmt_days, 
              type = "html",
              dep.var.labels = "Attendance Rate",
              covariate.labels = c(ohc_var_lables, control_var_labels, grade_dummy_labels),
              report = "vc*s",
              out = paste0(p_dir_out, "lm_attendance.htm"))

    # output attendence models, simplified
    stargazer(lm_attend_ohc, lm_attend_n_plcmt, lm_attend_plcmt_days, 
              type = "html",
              dep.var.labels = "Attendance Rate",
              covariate.labels = ohc_var_lables,
              omit = omit_vars,
              report = "vc*s",
              out = paste0(p_dir_out, "lm_attendance_condense.htm"))

    # output removal models, full
    stargazer(lm_remove_ohc, lm_remove_n_plcmt, lm_remove_plcmt_days, 
              type = "html",
              dep.var.labels = "Number of Removals",
              covariate.labels = c(ohc_var_lables, control_var_labels, grade_dummy_labels),
              report = "vc*s",
              out = paste0(p_dir_out, "lm_removal.htm"))

    # output removal models, simplified
    stargazer(lm_remove_ohc, lm_remove_n_plcmt, lm_remove_plcmt_days, 
              type = "html",
              dep.var.labels = "Number of Removals",
              covariate.labels = ohc_var_lables,
              omit = omit_vars,
              report = "vc*s",
              out = paste0(p_dir_out, "lm_removal_condense.htm"))
    
    # output wkce math models, full
    stargazer(lm_kce_math_ohc, lm_kce_math_n_plcmt, lm_kce_math_plcmt_days,
              type = "html",
              dep.var.labels = "WKCE Score - Math (Standardized)",
              covariate.labels = c(ohc_var_lables, control_var_labels, "Grade 9"),
              report = "vc*s",
              out = paste0(p_dir_out, "lm_wkce_math.htm"))
    
    # output wkce math models, simplified
    stargazer(lm_kce_math_ohc, lm_kce_math_n_plcmt, lm_kce_math_plcmt_days,
              type = "html",
              dep.var.labels = "WKCE Score - Math (Standardized)",
              covariate.labels = ohc_var_lables,
              omit = omit_vars,
              report = "vc*s",
              out = paste0(p_dir_out, "lm_wkce_math_condense.htm"))
    
    # output wkce reading models, full
    stargazer(lm_kce_rdg_ohc, lm_kce_rdg_n_plcmt, lm_kce_rdg_plcmt_days,
              type = "html",
              dep.var.labels = "WKCE Score - Reading (Standardized)",
              covariate.labels = c(ohc_var_lables, control_var_labels, "Grade 9"),
              report = "vc*s",
              out = paste0(p_dir_out, "lm_wkce_rdg.htm"))
    
    # output wkce reading models, simplified
    stargazer(lm_kce_rdg_ohc, lm_kce_rdg_n_plcmt, lm_kce_rdg_plcmt_days,
              type = "html",
              dep.var.labels = "WKCE Score - Reading (Standardized)",
              covariate.labels = ohc_var_lables,
              omit = omit_vars,
              report = "vc*s",
              out = paste0(p_dir_out, "lm_wkce_rdg_condense.htm"))
  }
  
