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

  # load acad year info
  in_acad_year_data <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")
  
  # load school attributes
  in_school_att <- fread("X:/LFS-Education Outcomes/data/raw_data/DPI_SCHOOL_ATTRIBUTES.csv")

########################
# format analysis data #
########################

  # copy input sets
  acad_yr_data <- copy(in_acad_year_data)

  # change necessary vars to numeric
  acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad")] <- 
    lapply(acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad"), with = FALSE], as.numeric)
  
  # sort by academic year
  setorder(acad_yr_data, acad_year)
  
  # create avg days per placement vars
  acad_yr_data[, avg_days_plcmt_acad := tot_plcmt_days_acad / n_plcmt_acad]
  
##############################
# create vars for regression #
##############################
  
  # fill in 0 for ohc vars for comparison group
  acad_yr_data[flag_ohc == 0, c("n_plcmt_acad", "tot_plcmt_days_acad", "avg_days_plcmt_acad") := 0]
  
  # create squared and square root term from number of placements
  acad_yr_data[, n_plcmt_acad_sq := n_plcmt_acad * n_plcmt_acad]
  acad_yr_data[, n_plcmt_acad_sqrt := sqrt(n_plcmt_acad)]

##############################
# subset sample for analysis #
##############################
  
  # create frl / non-frl flags for comparison groups
  acad_yr_data[, compare_frl := ifelse(flag_ohc == 0 & d_frl == 1, 1, 0)]

  # create flag if hs student
  acad_yr_data[, flag_hs := ifelse(grade_level_cd %in% c("08", "09", "10", "11", "12"), 1, 0)]
  
  # create hs subset
  acad_yr_data_hs <- subset(acad_yr_data, flag_hs == 1 & flag_dpi_yr == 1)
  
  # remove 2013 and 2014 academic years #brule
  acad_yr_data_hs <- subset(acad_yr_data_hs, acad_year != "2013" & acad_year != "2014")
  
  # sort by student id and academic year
  setorder(acad_yr_data_hs, lf_child_id, -flag_ohc_yr, -acad_year)
  
  # remove duplicate ids, keeping latest academic year (with placement for ohc student) #brule
  latest_acad_yr_hs <- ea_no_dups(acad_yr_data_hs, "lf_child_id")
  
  # remove ohc students with no placement in hs
  latest_acad_yr_hs <- subset(latest_acad_yr_hs, !(flag_ohc == 1 & flag_ohc_yr == 0))

############################
# format school covariates #
############################
  
  # copy raw data
  school_att <- copy(in_school_att)
  
  # change var names to lowercase
  setnames(school_att, tolower(colnames(school_att)))
  
  # create school level averages
  school_att[, ":="(per_sch_frl = frl_count / pupil_count, per_sch_sped = swd_count / pupil_count,  
                    per_sch_elp = (pupil_count - noelp_count) / pupil_count, per_sch_non_white = (pupil_count - race_w_count) / pupil_count, 
                    per_sch_removal = totalremovals_total / pupil_count, dist_acctbl_code_cd = as.numeric(distid), 
                    sch_acctbl_code_cd = as.numeric(schid), acad_year = as.character(year))]
  
  # subset to covariates
  school_covar <- subset(school_att, select = c(acad_year, dist_acctbl_code_cd, sch_acctbl_code_cd, per_sch_frl, per_sch_sped, per_sch_elp, 
                                                 per_sch_non_white, per_sch_removal, mean_math_z_score, mean_rdg_z_score))
  
  # rename vars for merge
  setnames(school_covar, c("mean_math_z_score", "mean_rdg_z_score"), c("sch_mean_math_z_score", "sch_mean_rdg_z_score"))

  # melt school covariates long to summarize
  school_covar_long <- melt.data.table(school_covar, id.vars = c("acad_year", "dist_acctbl_code_cd", "sch_acctbl_code_cd"))

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
  regression_set <- ea_merge(latest_acad_yr_hs, school_covar, c("acad_year", "dist_acctbl_code_cd", "sch_acctbl_code_cd"), "x")
  
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

  # calc acad outcomes, by ohc status, placement type
  a_acad_outcomes_by_type <- acad_yr_data_hs[flag_dpi_yr == 1, list(n_obs = .N,
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
                                          by = c("flag_ohc", "dcf_plcmt_type")]
  
  # calc acad outcomes, by ohc status, region
  a_acad_outcomes_by_region <- acad_yr_data_hs[flag_dpi_yr == 1, list(n_obs = .N,
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
                                            by = c("flag_ohc", "region")]
  
############################
# regressions - attendence #
############################
  
# reg: attendance on ohc flag and controls
reg_attend_ohc <- lm(att_rate_wi ~ flag_ohc + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + d_race_asian + d_race_indian +
                            age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + per_sch_removal + sch_mean_math_z_score +
                            sch_mean_rdg_z_score, data = regression_set)

# reg: attendance on number of placements and total days
reg_attend_ohc_var <- lm(att_rate_wi ~ n_plcmt_acad + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + d_race_hispanic + d_race_black + 
                           d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + per_sch_elp + per_sch_non_white + 
                           per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, data = regression_set)

# reg: attendance on number of placements (+ sqrt) and total days
reg_attend_ohc_var_sqrt <- lm(att_rate_wi ~ n_plcmt_acad + n_plcmt_acad_sqrt + tot_plcmt_days_acad + d_male + d_elp + d_sped + d_frl + 
                                d_race_hispanic + d_race_black + d_race_asian + d_race_indian + age_in_years_cd + per_sch_frl + per_sch_sped + 
                                per_sch_elp + per_sch_non_white + per_sch_removal + sch_mean_math_z_score + sch_mean_rdg_z_score, 
                              data = regression_set)

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
  
