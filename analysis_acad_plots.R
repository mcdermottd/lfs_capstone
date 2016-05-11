######################################################################
# notes:
# - purpose: create ohc mean plots for presentation (adjusted and unadjusted)
# - inputs: formatted analysis set
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
  library(apsrtable)
  library(sandwich)
  library(lmtest)
  library(ggplot2)
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

###################################
# create variable transformations #
###################################

  # copy input sets
  full_outcomes_set <- copy(in_outcomes_set)
  
  # transform number of placement var (log)
  full_outcomes_set[, n_plcmt_log := ifelse(lf_n_plcmt_acad != 0, log(lf_n_plcmt_acad), 0)]

  # transform placement days var (log)
  full_outcomes_set[, plcmt_days_log := ifelse(tot_plcmt_days_acad != 0, log(tot_plcmt_days_acad), 0)]
  
  # transform school enrollment var
  full_outcomes_set[, sch_pup_ct_log := ifelse(sch_pupil_count != 0, log(sch_pupil_count), 0)]

########################
# subset analysis data #
########################
  
  # sort by academic year
  setorder(full_outcomes_set, acad_year)
  
  # create set with only analysis grades (7 - 12) #brule
  analysis_sample <- subset(full_outcomes_set, flag_analysis_grd == 1)
  
  # remove observations for ohc students occuring prior to a placement #brule
  analysis_sample <- subset(analysis_sample, !(flag_ohc == 1 & flag_cur_plcmt == 0 & flag_prior_plcmt == 0))
  
  # create set with only 7th and 9th graders with next year test score for wkce analysis #brule
  analysis_wkce <- subset(analysis_sample, (grade %in% c("07", "09") & !is.na(nxt_zscore_math_kce)))
  
#############################
# set regression parameters #
#############################
  
  # create list of student control vars
  lm_student_controls <- c("age_in_years_cd", "d_male", "d_frl", "d_sped", "d_elp", "d_race_black", "d_race_hispanic", "d_race_asian", 
                            "d_race_indian")
  
  # create list of grade dummies
  lm_dummies_grade <- c("d_grade_08", "d_grade_09", "d_grade_10", "d_grade_11", "d_grade_12")
  
  # create list of school covariates
  lm_sch_controls <- c("sch_pup_ct_log", "sch_frl_scaled", "sch_sped_scaled", "sch_elp_scaled", "sch_non_white_scaled", "sch_removal_scaled",
                       "sch_mean_math_z_score", "sch_mean_rdg_z_score")
  
  # create list of year dummies
  lm_dummies_yr <- c("d_acad_year_2009", "d_acad_year_2010", "d_acad_year_2011", "d_acad_year_2012")
  
  # combine for full set of controls
  lm_controls_full <- paste(c(lm_student_controls, lm_dummies_grade, lm_sch_controls, lm_dummies_yr), collapse = " + ")
  lm_controls_wkce <- paste(c(lm_student_controls, "d_grade_09", lm_sch_controls, lm_dummies_yr), collapse = " + ")

  # create list of vars for subset
  lm_var_list <- c("lf_sch_id", "flag_ohc", "flag_cur_plcmt", "flag_prior_plcmt", "lf_n_plcmt_acad", "n_plcmt_log", "tot_plcmt_days_acad", 
                   "plcmt_days_log", "d_p_type_fhome_rel", "d_p_type_fhome_nonrel", "d_p_type_group_home", "d_p_type_rcc", "d_p_type_other", 
                   lm_student_controls, lm_sch_controls, lm_dummies_yr, lm_dummies_grade)
  
#####################################################
# create set without missings to use in regressions #
#####################################################
  
  # create dataset with no missings
  attend_set <- subset(analysis_sample, select = c("att_rate_wi", lm_var_list))
  removal_set <- subset(analysis_sample, select = c("days_removed_os", lm_var_list))
  wkce_math_set <- subset(analysis_sample, select = c("nxt_zscore_math_kce", lm_var_list))
  wkce_rdg_set <- subset(analysis_sample, select = c("nxt_zscore_rdg_kce", lm_var_list))

  # remove all missings to run clustering function #brule
  attend_set <- na.omit(attend_set)
  removal_set <- na.omit(removal_set)
  wkce_math_set <- na.omit(wkce_math_set)
  wkce_rdg_set <- na.omit(wkce_rdg_set)

#############################
# set regression parameters #
#############################
  
  # create list of student control vars
  lm_student_controls <- c("age_in_years_cd", "d_male", "d_frl", "d_sped", "d_elp", "d_race_black", "d_race_hispanic", "d_race_asian", 
                           "d_race_indian")
  
  # create list of grade dummies
  lm_dummies_grade <- c("d_grade_08", "d_grade_09", "d_grade_10", "d_grade_11", "d_grade_12")
  
  # create list of school covariates
  lm_sch_controls <- c("sch_pup_ct_log", "sch_frl_scaled", "sch_sped_scaled", "sch_elp_scaled", "sch_non_white_scaled", "sch_removal_scaled",
                       "sch_mean_math_z_score", "sch_mean_rdg_z_score")
  
  # create list of year dummies
  lm_dummies_yr <- c("d_acad_year_2009", "d_acad_year_2010", "d_acad_year_2011", "d_acad_year_2012")
  
  # combine for full set of controls
  lm_controls_full <- paste(c(lm_student_controls, lm_dummies_grade, lm_sch_controls, lm_dummies_yr), collapse = " + ")
  lm_controls_wkce <- paste(c(lm_student_controls, "d_grade_09", lm_sch_controls, lm_dummies_yr), collapse = " + ")

  # create list of region variables
  lm_region_vars <- c("int_reg_nc_ohc", "int_reg_ne_ohc", "int_reg_mke_ohc", "int_reg_se_ohc", "int_reg_s_ohc", "int_reg_w_ohc", "int_reg_nw_ohc", 
                      "d_lf_region_nc", "d_lf_region_ne", "d_lf_region_mke", "d_lf_region_se", "d_lf_region_s", "d_lf_region_w", "d_lf_region_nw")
  
  # create list of vars for subset
  lm_var_list <- c("lf_sch_id", "flag_ohc", "flag_cur_plcmt", "flag_prior_plcmt", "lf_n_plcmt_acad", "n_plcmt_log", "tot_plcmt_days_acad", 
                   lm_student_controls, lm_sch_controls, lm_dummies_yr, lm_dummies_grade, lm_region_vars)
  
############################
# math wkce (same process) #
############################
    
  # reg: math wkce on OHC flags
  lm_formula <- paste("nxt_zscore_math_kce ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_wkce)
  m3a_wkce_math <- lm(lm_formula, data = wkce_math_set)

  # math wkce: compute predicted values and combine with orginal data
  pred_math_wkce_set <- data.table(pred_nxt_math_kce = fitted(m3a_wkce_math))
  wkce_math_set <- cbind(wkce_math_set, pred_math_wkce_set)
  
  # subset to ohc and outcome vars to summarize
  melt_vars <- subset(wkce_math_set, select = c(flag_cur_plcmt, flag_prior_plcmt, nxt_zscore_math_kce, pred_nxt_math_kce))
  
  # melt ohc data long to summarize
  summ_vars_long <- melt.data.table(melt_vars, id.vars = c("flag_cur_plcmt", "flag_prior_plcmt"))
  
  # table - summarize vars overall
  a_math_wkce_vars <- summ_vars_long[, list(n_obs = length(value),
                                           mean = mean(value),
                                           sd = sd(value),
                                           se = sd(value) / sqrt(length(value))),
                                     by = c("flag_cur_plcmt", "flag_prior_plcmt", "variable")]
  
  ztest <- wkce_math_set[, list(act_mean = mean(nxt_zscore_math_kce),
                                pred_mean = mean(pred_nxt_math_kce))?]
                         by = flag_cur_plcmt]
  
  
###############################
# reading wkce (same process) #
###############################
    
  # reg: reading wkce on OHC flags
  lm_formula <- paste("nxt_zscore_rdg_kce ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_wkce)
  m4a_wkce_rdg_ohc <- lm(lm_formula, data = wkce_rdg_set)
  
################################################
# attend: run reg and compute predicted values #
################################################
  
  # reg: attendance on OHC flags
  lm_formula <- paste("att_rate_wi ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_full)
  m1a_attend <- lm(lm_formula, data = attend_set)

##########################
# removal (same process) #
##########################
  
  # reg: removals on OHC flags
  lm_formula <- paste("days_removed_os ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_full)
  m2a_remove <- lm(lm_formula, data = removal_set)

  
  
ggplot(effect_df, aes(x = age_treatment, y = fit)) + 
  geom_bar(stat = "identity", fill = "#619CFF")  + 
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se,), width = .1, col = "black") +
  geom_point(aes(x = age_treatment ,y = lower), size = 5, shape = 21) +
  geom_point(aes(x = age_treatment, y = upper), size = 5, shape = 21)