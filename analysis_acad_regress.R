######################################################################
# notes:
# - purpose: regress ohc characteristics on academic outcomes (attendence, removals, wkce scores)
# - inputs: formatted analysis set
# - outputs: formatted and grouped .tex files (Latex format) for each type of academic outcome
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
  
  # transform number of placement var
  full_outcomes_set[, n_plcmt_sq := ifelse(lf_n_plcmt_acad != 0, lf_n_plcmt_acad * lf_n_plcmt_acad, 0)]
  full_outcomes_set[, n_plcmt_log := ifelse(lf_n_plcmt_acad != 0, log(lf_n_plcmt_acad), 0)]

  # transform placement days var
  full_outcomes_set[, plcmt_days_sq := ifelse(tot_plcmt_days_acad != 0, tot_plcmt_days_acad * tot_plcmt_days_acad, 0)]
  full_outcomes_set[, plcmt_days_log := ifelse(tot_plcmt_days_acad != 0, log(tot_plcmt_days_acad), 0)]
  
  # transform school enrollment var
  full_outcomes_set[, sch_pup_ct_log := ifelse(sch_pupil_count != 0, log(sch_pupil_count), 0)]

##################################
# create region interaction vars #
##################################
  
  full_outcomes_set[, int_reg_s_ohc := d_lf_region_s * flag_ohc]
  full_outcomes_set[, int_reg_ne_ohc := d_lf_region_ne * flag_ohc]
  full_outcomes_set[, int_reg_w_ohc := d_lf_region_w * flag_ohc]
  full_outcomes_set[, int_reg_nc_ohc := d_lf_region_nc * flag_ohc]
  full_outcomes_set[, int_reg_mke_ohc := d_lf_region_mke * flag_ohc]
  full_outcomes_set[, int_reg_se_ohc := d_lf_region_se * flag_ohc]
  full_outcomes_set[, int_reg_nw_ohc := d_lf_region_nw * flag_ohc]

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
  
#################################
# define SE clustering function #
#################################
  
  func_cl_vcov <- function(model, cluster){
    # cluster is an actual vector of clusters from data passed to model
    # from: http://rforpublichealth.blogspot.com/2014/10/easy-clustered-standard-errors-in-r.html
    
    # convert group variable to character
    cluster <- as.character(cluster)
   
    # calculate degree of freedom adjustment
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M / (M - 1)) * ((N - 1) / (N - K))
   
    # calculate the uj's
    uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
   
    # use sandwich to get the var-covar matrix
    vcov_cl <- dfc * sandwich(model, meat = crossprod(uj) / N)
    return(vcov_cl)
  }
  
#############################
# set regression parameters #
#############################
  
  # create list of student control vars
  lm_student_controls <- c("age_in_years_cd", "d_male", "d_elp", "d_sped", "d_frl", "d_race_black", "d_race_hispanic", "d_race_asian", 
                            "d_race_indian")
  
  # create list of school covariates
  lm_sch_controls <- c("sch_pup_ct_log", "sch_frl_scaled", "sch_sped_scaled", "sch_elp_scaled", "sch_non_white_scaled", "sch_removal_scaled",
                       "sch_mean_math_z_score", "sch_mean_rdg_z_score")
  
  # create list of year dummies
  lm_dummies_yr <- c("d_acad_year_2009", "d_acad_year_2010", "d_acad_year_2011", "d_acad_year_2012")
  
  # create list of grade dummies
  lm_dummies_grade <- c("d_grade_09", "d_grade_10", "d_grade_11", "d_grade_12")

  # create list of region interaction dummies
  lm_region_interactions <- c("int_reg_mke_ohc", "int_reg_se_ohc", "int_reg_s_ohc", "int_reg_w_ohc", "int_reg_nw_ohc", "int_reg_nc_ohc", 
                              "int_reg_ne_ohc")
  
  # create list of vars for subset
  lm_var_list <- c("lf_sch_id", "flag_cur_plcmt", "flag_prior_plcmt", "lf_n_plcmt_acad", "n_plcmt_sq", "n_plcmt_log", "tot_plcmt_days_acad", 
                   "plcmt_days_sq", "plcmt_days_log", "d_p_type_fhome_rel", "d_p_type_fhome_nonrel", "d_p_type_group_home", "d_p_type_rcc", 
                   "d_p_type_other", lm_student_controls, lm_sch_controls, lm_dummies_yr, lm_dummies_grade, lm_region_interactions)
  
  # combine for full set of controls
  lm_controls_full <- paste(c(lm_student_controls, lm_sch_controls, lm_dummies_yr, lm_dummies_grade), collapse = " + ")
  lm_controls_wkce <- paste(c(lm_student_controls, lm_sch_controls, lm_dummies_yr, "d_grade_09"), collapse = " + ")

############################
# regressions - attendence #
############################

  # create dataset with no missings
  attend_set <- subset(analysis_sample, select = c("att_rate_wi", lm_var_list))
  
  # remove all missings to run clustering function #brule
  attend_set <- na.omit(attend_set)
  
  # reg: attendance on OHC flags
  lm_formula <- paste("att_rate_wi ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_full)
  m1a_attend_ohc <- lm(lm_formula, data = attend_set)

  # reg: attendance on log number of placements
  lm_formula <- paste("att_rate_wi ~ n_plcmt_log + flag_prior_plcmt + ", lm_controls_full)
  m1b_attend_plcmts_log <- lm(lm_formula, data = attend_set)

  # reg: attendance on log total placement days
  lm_formula <- paste("att_rate_wi ~ plcmt_days_log + flag_prior_plcmt + ", lm_controls_full)
  m1c_attend_pdays_log <- lm(lm_formula, data = attend_set)

  # reg: attendance on type of OHC
  lm_formula <- paste("att_rate_wi ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + d_p_type_other + 
                      flag_prior_plcmt + ", lm_controls_full)
  m1d_attend_ptype <- lm(lm_formula, data = attend_set)
  
  # reg: attendance on region
  lm_formula <- paste("att_rate_wi ~ int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + int_reg_w_ohc + int_reg_nw_ohc + int_reg_nc_ohc + 
                      int_reg_ne_ohc + flag_prior_plcmt + ", lm_controls_full)
  m1e_attend_reg <- lm(lm_formula, data = attend_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m1a_attend_ohc$se <- func_cl_vcov(m1a_attend_ohc, attend_set$lf_sch_id)
  m1b_attend_plcmts_log$se <- func_cl_vcov(m1b_attend_plcmts_log, attend_set$lf_sch_id)
  m1c_attend_pdays_log$se <- func_cl_vcov(m1c_attend_pdays_log, attend_set$lf_sch_id)
  m1d_attend_ptype$se <- func_cl_vcov(m1d_attend_ptype, attend_set$lf_sch_id)
  m1e_attend_reg$se <- func_cl_vcov(m1e_attend_reg, attend_set$lf_sch_id)

##########################
# regressions - removals #
##########################
  
  # create dataset with no missings
  removal_set <- subset(analysis_sample, select = c("days_removed_os", lm_var_list))
  
  # remove all missings to run clustering function #brule
  removal_set <- na.omit(removal_set)
  
  # reg: removals on OHC flags
  lm_formula <- paste("days_removed_os ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_full)
  m2a_remove_ohc <- lm(lm_formula, data = removal_set)
  
  # reg: removals on log number of placements
  lm_formula <- paste("days_removed_os ~ n_plcmt_log + flag_prior_plcmt + ", lm_controls_full)
  m2b_remove_plcmts_log <- lm(lm_formula, data = removal_set)
  
  # reg: removals on log total placement days
  lm_formula <- paste("days_removed_os ~ plcmt_days_log + flag_prior_plcmt + ", lm_controls_full)
  m2c_remove_pdays_log <- lm(lm_formula, data = removal_set)

  # reg: removals on type of OHC
  lm_formula <- paste("days_removed_os ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + d_p_type_other + 
                      flag_prior_plcmt + ", lm_controls_full)
  m2d_remove_ptype <- lm(lm_formula, data = removal_set)
  
  # reg: removals on region
  lm_formula <- paste("days_removed_os ~ int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + int_reg_w_ohc + int_reg_nw_ohc + int_reg_nc_ohc + 
                      int_reg_ne_ohc + flag_prior_plcmt + ", lm_controls_full)
  m2e_remove_reg <- lm(lm_formula, data = removal_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m2a_remove_ohc$se <- func_cl_vcov(m2a_remove_ohc, removal_set$lf_sch_id)
  m2b_remove_plcmts_log$se <- func_cl_vcov(m2b_remove_plcmts_log, removal_set$lf_sch_id)
  m2c_remove_pdays_log$se <- func_cl_vcov(m2c_remove_pdays_log, removal_set$lf_sch_id)
  m2d_remove_ptype$se <- func_cl_vcov(m2d_remove_ptype, removal_set$lf_sch_id)
  m2e_remove_reg$se <- func_cl_vcov(m2e_remove_reg, removal_set$lf_sch_id)

###########################
# regressions - wkce math #
###########################
  
  # create dataset with no missings
  wkce_math_set <- subset(analysis_sample, select = c("nxt_zscore_math_kce", lm_var_list))
  
  # remove all missings to run clustering function #brule
  wkce_math_set <- na.omit(wkce_math_set)
  
  # reg: math wkce on OHC flags
  lm_formula <- paste("nxt_zscore_math_kce ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_wkce)
  m3a_wkce_math_ohc <- lm(lm_formula, data = wkce_math_set)
  
  # reg: math wkce on log number of placements
  lm_formula <- paste("nxt_zscore_math_kce ~ n_plcmt_log + flag_prior_plcmt + ", lm_controls_wkce)
  m3b_wkce_math_plcmts_log <- lm(lm_formula, data = wkce_math_set)
  
  # reg: math wkce on log total placement days
  lm_formula <- paste("nxt_zscore_math_kce ~ plcmt_days_log + flag_prior_plcmt + ", lm_controls_wkce)
  m3c_wkce_math_pdays_log <- lm(lm_formula, data = wkce_math_set)

  # reg: math wkce on type of OHC
  lm_formula <- paste("nxt_zscore_math_kce ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + d_p_type_other + 
                      flag_prior_plcmt + ", lm_controls_wkce)
  m3d_wkce_math_ptype <- lm(lm_formula, data = wkce_math_set)
  
  # reg: math wkce on region
  lm_formula <- paste("nxt_zscore_math_kce ~ int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + int_reg_w_ohc + int_reg_nw_ohc + int_reg_nc_ohc + 
                      int_reg_ne_ohc + flag_prior_plcmt + ", lm_controls_wkce)
  m3e_wkce_math_reg <- lm(lm_formula, data = wkce_math_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m3a_wkce_math_ohc$se <- func_cl_vcov(m3a_wkce_math_ohc, wkce_math_set$lf_sch_id)
  m3b_wkce_math_plcmts_log$se <- func_cl_vcov(m3b_wkce_math_plcmts_log, wkce_math_set$lf_sch_id)
  m3c_wkce_math_pdays_log$se <- func_cl_vcov(m3c_wkce_math_pdays_log, wkce_math_set$lf_sch_id)
  m3d_wkce_math_ptype$se <- func_cl_vcov(m3d_wkce_math_ptype, wkce_math_set$lf_sch_id)
  m3e_wkce_math_reg$se <- func_cl_vcov(m3e_wkce_math_reg, wkce_math_set$lf_sch_id)

##############################
# regressions - wkce reading #
##############################
  
  # create dataset with no missings
  wkce_rdg_set <- subset(analysis_sample, select = c("nxt_zscore_rdg_kce", lm_var_list))
  
  # remove all missings to run clustering function #brule
  wkce_rdg_set <- na.omit(wkce_rdg_set)
  
  # reg: reading wkce on OHC flags
  lm_formula <- paste("nxt_zscore_rdg_kce ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_wkce)
  m4a_wkce_rdg_ohc <- lm(lm_formula, data = wkce_rdg_set)
  
  # reg: reading wkce on log number of placements
  lm_formula <- paste("nxt_zscore_rdg_kce ~ n_plcmt_log + flag_prior_plcmt + ", lm_controls_wkce)
  m4b_wkce_rdg_plcmts_log <- lm(lm_formula, data = wkce_rdg_set)
  
  # reg: reading wkce on log total placement days
  lm_formula <- paste("nxt_zscore_rdg_kce ~ plcmt_days_log + flag_prior_plcmt + ", lm_controls_wkce)
  m4c_wkce_rdg_pdays_log <- lm(lm_formula, data = wkce_rdg_set)

  # reg: reading wkce on type of OHC
  lm_formula <- paste("nxt_zscore_rdg_kce ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + d_p_type_other + 
                      flag_prior_plcmt + ", lm_controls_wkce)
  m4d_wkce_rdg_ptype <- lm(lm_formula, data = wkce_rdg_set)
  
  # reg: reading wkce on region
  lm_formula <- paste("nxt_zscore_rdg_kce ~ int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + int_reg_w_ohc + int_reg_nw_ohc + int_reg_nc_ohc + 
                      int_reg_ne_ohc + flag_prior_plcmt + ", lm_controls_wkce)
  m4e_wkce_rdg_reg <- lm(lm_formula, data = wkce_rdg_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m4a_wkce_rdg_ohc$se <- func_cl_vcov(m4a_wkce_rdg_ohc, wkce_rdg_set$lf_sch_id)
  m4b_wkce_rdg_plcmts_log$se <- func_cl_vcov(m4b_wkce_rdg_plcmts_log, wkce_rdg_set$lf_sch_id)
  m4c_wkce_rdg_pdays_log$se <- func_cl_vcov(m4c_wkce_rdg_pdays_log, wkce_rdg_set$lf_sch_id)
  m4d_wkce_rdg_ptype$se <- func_cl_vcov(m4d_wkce_rdg_ptype, wkce_rdg_set$lf_sch_id)
  m4e_wkce_rdg_reg$se <- func_cl_vcov(m4e_wkce_rdg_reg, wkce_rdg_set$lf_sch_id)

######################
# format export vars #
######################

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

############################
# export regression output #
############################
   
  # export
  if (p_opt_exp == 1) { 

    # output attendance models
    cat(apsrtable(m1a_attend_ohc, m1b_attend_plcmts_log, m1c_attend_pdays_log,
                  se = "robust",
                  Sweave = FALSE,
                  float = "longtable",
                  label = "attendance_models",
                  caption = "Attendance Rate"),
        file = paste0(p_dir_out, "models_attendance.tex"))
    
    # output removal models
    cat(apsrtable(m2a_remove_ohc, m2b_remove_plcmts_log, m2c_remove_pdays_log,
                  se = "robust",
                  Sweave = FALSE,
                  float = "longtable",
                  label = "removal_models",
                  caption = "Removals"),
        file = paste0(p_dir_out, "models_removals.tex"))
    
    # output wkce math models
    cat(apsrtable(m3a_wkce_math_ohc, m3b_wkce_math_plcmts_log, m3c_wkce_math_pdays_log,
                  se = "robust",
                  Sweave = FALSE,
                  float = "longtable",
                  label = "wkce_math_models",
                  caption = "WKCE Math Score"),
        file = paste0(p_dir_out, "models_wkce_math.tex"))
    
    # output wkce reading models
    cat(apsrtable(m4a_wkce_rdg_ohc, m4b_wkce_rdg_plcmts_log, m4c_wkce_rdg_pdays_log,
                  se = "robust",
                  Sweave = FALSE,
                  float = "longtable",
                  label = "wkce_rdg_models",
                  caption = "WKCE Reading Score"),
        file = paste0(p_dir_out, "models_wkce_rdg.tex"))
    
    # output placement type models
    cat(apsrtable(m1d_attend_ptype, m2d_remove_ptype, m3d_wkce_math_ptype, m4d_wkce_rdg_ptype,
                  se = "robust",
                  model.names = c("Attendance Rate", "Removals", "WKCE Math Score", "WKCE Reading Score"),
                  Sweave = FALSE,
                  float = "longtable",
                  label = "ptype_models",
                  caption = "Academic Outcomes by Placement Type"),
        file = paste0(p_dir_out, "models_ptype.tex"))
    
    # output region models
    cat(apsrtable(m1e_attend_reg, m2e_remove_reg, m3e_wkce_math_reg, m4e_wkce_rdg_reg,
                  se = "robust",
                  model.names = c("Attendance Rate", "Removals", "WKCE Math Score", "WKCE Reading Score"),
                  Sweave = FALSE,
                  float = "longtable",
                  lable = "region_models",
                  caption = "Academic Outcomes by Region"),
        file = paste0(p_dir_out, "models_region.tex"))
    
  }
    
    
    
  
  stargazer(lm_attend_ohc, lm_attend_n_plcmt, lm_attend_plcmt_days, lm_attend_ptype, 
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
    
    
    