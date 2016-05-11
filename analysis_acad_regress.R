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
  
  # transform number of placement var (log)
  full_outcomes_set[, n_plcmt_log := ifelse(lf_n_plcmt_acad != 0, log(lf_n_plcmt_acad), 0)]

  # transform placement days var (log)
  full_outcomes_set[, plcmt_days_log := ifelse(tot_plcmt_days_acad != 0, log(tot_plcmt_days_acad), 0)]
  
  # transform school enrollment var
  full_outcomes_set[, sch_pup_ct_log := ifelse(sch_pupil_count != 0, log(sch_pupil_count), 0)]

##################################
# create region interaction vars #
##################################
  
  full_outcomes_set[, int_reg_nc_ohc := d_lf_region_nc * flag_ohc]
  full_outcomes_set[, int_reg_ne_ohc := d_lf_region_ne * flag_ohc]
  full_outcomes_set[, int_reg_mke_ohc := d_lf_region_mke * flag_ohc]
  full_outcomes_set[, int_reg_se_ohc := d_lf_region_se * flag_ohc]
  full_outcomes_set[, int_reg_s_ohc := d_lf_region_s * flag_ohc]
  full_outcomes_set[, int_reg_w_ohc := d_lf_region_w * flag_ohc]
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
                   "plcmt_days_log", "d_p_type_fhome_rel", "d_p_type_fhome_nonrel", "d_p_type_group_home", "d_p_type_rcc", "d_p_type_other", 
                   lm_student_controls, lm_sch_controls, lm_dummies_yr, lm_dummies_grade, lm_region_vars)
  
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
  
  # remove ptype "other" for ptype regressions
  attend_set_ptype <- subset(attend_set, d_p_type_other != 1)
  removal_set_ptype <- subset(removal_set, d_p_type_other != 1)
  wkce_math_set_ptype <- subset(wkce_math_set, d_p_type_other != 1)
  wkce_rdg_set_ptype <- subset(wkce_rdg_set, d_p_type_other != 1)

############################
# regressions - attendence #
############################

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
  lm_formula <- paste("att_rate_wi ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + flag_prior_plcmt + ", 
                      lm_controls_full)
  m1d_attend_ptype <- lm(lm_formula, data = attend_set_ptype)
  
  # reg: attendance on region
  lm_formula <- paste("att_rate_wi ~ flag_ohc + int_reg_nc_ohc + int_reg_ne_ohc + int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + 
                      int_reg_w_ohc + d_lf_region_nc + d_lf_region_ne + d_lf_region_mke + d_lf_region_se + d_lf_region_s + d_lf_region_w + ",
                      lm_controls_full)
  m1e_attend_reg <- lm(lm_formula, data = attend_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m1a_attend_ohc$se <- func_cl_vcov(m1a_attend_ohc, attend_set$lf_sch_id)
  m1b_attend_plcmts_log$se <- func_cl_vcov(m1b_attend_plcmts_log, attend_set$lf_sch_id)
  m1c_attend_pdays_log$se <- func_cl_vcov(m1c_attend_pdays_log, attend_set$lf_sch_id)
  m1d_attend_ptype$se <- func_cl_vcov(m1d_attend_ptype, attend_set_ptype$lf_sch_id)
  m1e_attend_reg$se <- func_cl_vcov(m1e_attend_reg, attend_set$lf_sch_id)

##########################
# regressions - removals #
##########################

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
  lm_formula <- paste("days_removed_os ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + flag_prior_plcmt + ",
                      lm_controls_full)
  m2d_remove_ptype <- lm(lm_formula, data = removal_set_ptype)
  
  # reg: removals on region
  lm_formula <- paste("days_removed_os ~ flag_ohc + int_reg_nc_ohc + int_reg_ne_ohc + int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + 
                      int_reg_w_ohc + d_lf_region_nc + d_lf_region_ne + d_lf_region_mke + d_lf_region_se + d_lf_region_s + d_lf_region_w + ",
                      lm_controls_full)
  m2e_remove_reg <- lm(lm_formula, data = removal_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m2a_remove_ohc$se <- func_cl_vcov(m2a_remove_ohc, removal_set$lf_sch_id)
  m2b_remove_plcmts_log$se <- func_cl_vcov(m2b_remove_plcmts_log, removal_set$lf_sch_id)
  m2c_remove_pdays_log$se <- func_cl_vcov(m2c_remove_pdays_log, removal_set$lf_sch_id)
  m2d_remove_ptype$se <- func_cl_vcov(m2d_remove_ptype, removal_set_ptype$lf_sch_id)
  m2e_remove_reg$se <- func_cl_vcov(m2e_remove_reg, removal_set$lf_sch_id)

###########################
# regressions - wkce math #
###########################

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
  lm_formula <- paste("nxt_zscore_math_kce ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + flag_prior_plcmt + ",
                      lm_controls_wkce)
  m3d_wkce_math_ptype <- lm(lm_formula, data = wkce_math_set_ptype)
  
  # reg: math wkce on region
  lm_formula <- paste("nxt_zscore_math_kce ~ flag_ohc + int_reg_nc_ohc + int_reg_ne_ohc + int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + 
                      int_reg_w_ohc + d_lf_region_nc + d_lf_region_ne + d_lf_region_mke + d_lf_region_se + d_lf_region_s + d_lf_region_w + ",
                      lm_controls_wkce)
  m3e_wkce_math_reg <- lm(lm_formula, data = wkce_math_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m3a_wkce_math_ohc$se <- func_cl_vcov(m3a_wkce_math_ohc, wkce_math_set$lf_sch_id)
  m3b_wkce_math_plcmts_log$se <- func_cl_vcov(m3b_wkce_math_plcmts_log, wkce_math_set$lf_sch_id)
  m3c_wkce_math_pdays_log$se <- func_cl_vcov(m3c_wkce_math_pdays_log, wkce_math_set$lf_sch_id)
  m3d_wkce_math_ptype$se <- func_cl_vcov(m3d_wkce_math_ptype, wkce_math_set_ptype$lf_sch_id)
  m3e_wkce_math_reg$se <- func_cl_vcov(m3e_wkce_math_reg, wkce_math_set$lf_sch_id)
  
##############################
# regressions - wkce reading #
##############################

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
  lm_formula <- paste("nxt_zscore_rdg_kce ~ d_p_type_fhome_rel + d_p_type_fhome_nonrel + d_p_type_group_home + d_p_type_rcc + flag_prior_plcmt + ",
                      lm_controls_wkce)
  m4d_wkce_rdg_ptype <- lm(lm_formula, data = wkce_rdg_set_ptype)
  
  # reg: reading wkce on region
  lm_formula <- paste("nxt_zscore_rdg_kce ~ flag_ohc + int_reg_nc_ohc + int_reg_ne_ohc + int_reg_mke_ohc + int_reg_se_ohc + int_reg_s_ohc + 
                      int_reg_w_ohc + d_lf_region_nc + d_lf_region_ne + d_lf_region_mke + d_lf_region_se + d_lf_region_s + d_lf_region_w + ",
                      lm_controls_wkce)
  m4e_wkce_rdg_reg <- lm(lm_formula, data = wkce_rdg_set)
  
  # append var-cov matrix with clustered standard errors as additional model argumented 
  m4a_wkce_rdg_ohc$se <- func_cl_vcov(m4a_wkce_rdg_ohc, wkce_rdg_set$lf_sch_id)
  m4b_wkce_rdg_plcmts_log$se <- func_cl_vcov(m4b_wkce_rdg_plcmts_log, wkce_rdg_set$lf_sch_id)
  m4c_wkce_rdg_pdays_log$se <- func_cl_vcov(m4c_wkce_rdg_pdays_log, wkce_rdg_set$lf_sch_id)
  m4d_wkce_rdg_ptype$se <- func_cl_vcov(m4d_wkce_rdg_ptype, wkce_rdg_set_ptype$lf_sch_id)
  m4e_wkce_rdg_reg$se <- func_cl_vcov(m4e_wkce_rdg_reg, wkce_rdg_set$lf_sch_id)

######################
# format export vars #
######################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/outcomes/"
  
  # create vector of student control labels
  student_control_labels <- c("Age", "Male", "Eligible for Free or Reduced Price Lunch (FRL)", "Disability (SPED)", 
                              "Limited English Proficiency (ELP)", "Race - Black", "Race - Hispanic", "Race - Asian", "Race - Indian")
                    
  # create vector of grade dummy labels
  grade_dummy_labels <- c("Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12")
                              
  # create vector of school control lables          
  school_control_labels <- paste0("School: ", c("Total Enrollment (Log)", "Number of FRL Students", "SPED Students", "ELP Students",
                                                 "Non-White Students", "Removals", "Avg. WKCE Math Score", "Avg. WKCE Reading Score"))
                                  
  # create vector of academic year lables
  acad_year_labels <- paste0("Acad. Year: ", c("2009", "2010", "2011", "2012"))

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
                  coef.names = c("(Intercept)", "Placement in Academic Year", "Placement in Prior Academic Year", student_control_labels,
                                 grade_dummy_labels, school_control_labels, acad_year_labels, "Number of Placements in Academic Year (Log)",
                                 "Days in Placement in Academic Year (Log)"),
                  label = "attendance_models",
                  caption = "Attendance Rate"),
        file = paste0(p_dir_out, "models_attendance.tex"))
    
    # output removal models
    cat(apsrtable(m2a_remove_ohc, m2b_remove_plcmts_log, m2c_remove_pdays_log,
                  se = "robust",
                  Sweave = FALSE,
                  float = "longtable",
                  coef.names = c("(Intercept)", "Placement in Academic Year", "Placement in Prior Academic Year", student_control_labels,
                                 grade_dummy_labels, school_control_labels, acad_year_labels, "Number of Placements in Academic Year (Log)",
                                 "Days in Placement in Academic Year (Log)"),
                  label = "discipline_models",
                  caption = "Disciplinary Action"),
        file = paste0(p_dir_out, "models_removals.tex"))
    
    # output wkce math models
    cat(apsrtable(m3a_wkce_math_ohc, m3b_wkce_math_plcmts_log, m3c_wkce_math_pdays_log,
                  se = "robust",
                  Sweave = FALSE,
                  float = "longtable",
                  coef.names = c("(Intercept)", "Placement in Academic Year", "Placement in Prior Academic Year", student_control_labels,
                                 "Grade 9", school_control_labels, acad_year_labels, "Number of Placements in Academic Year (Log)",
                                 "Days in Placement in Academic Year (Log)"),
                  label = "wkce_math_models",
                  caption = "WKCE Math Score"),
        file = paste0(p_dir_out, "models_wkce_math.tex"))
    
    # output wkce reading models
    cat(apsrtable(m4a_wkce_rdg_ohc, m4b_wkce_rdg_plcmts_log, m4c_wkce_rdg_pdays_log,
                  se = "robust",
                  Sweave = FALSE,
                  float = "longtable",
                  coef.names = c("(Intercept)", "Placement in Academic Year", "Placement in Prior Academic Year", student_control_labels,
                                 "Grade 9", school_control_labels, acad_year_labels, "Number of Placements in Academic Year (Log)",
                                 "Days in Placement in Academic Year (Log)"),
                  label = "wkce_rdg_models",
                  caption = "WKCE Reading Score"),
        file = paste0(p_dir_out, "models_wkce_rdg.tex"))
    
    # output placement type models
    cat(apsrtable(m1d_attend_ptype, m2d_remove_ptype, m3d_wkce_math_ptype, m4d_wkce_rdg_ptype,
                  se = "robust",
                  model.names = c("Attendance Rate", "Disciplinary Action", "WKCE Math Score", "WKCE Reading Score"),
                  Sweave = FALSE,
                  float = "longtable",
                  coef.names = c("(Intercept)", paste0("Placement Type: ", c("Foster Home - Relative", "Foster Home - Non-Relative", "Group Home",
                                                                             "RCC")), 
                                 "Placement in Prior Academic Year", student_control_labels, grade_dummy_labels, school_control_labels, 
                                 acad_year_labels),
                  label = "ptype_models",
                  caption = "Academic Outcomes by Placement Type"),
        file = paste0(p_dir_out, "models_ptype.tex"))
    
    # output region models
    cat(apsrtable(m1e_attend_reg, m2e_remove_reg, m3e_wkce_math_reg, m4e_wkce_rdg_reg,
                  se = "robust",
                  model.names = c("Attendance Rate", "Disciplinary Action", "WKCE Math Score", "WKCE Reading Score"),
                  Sweave = FALSE,
                  float = "longtable",
                  coef.names = c("(Intercept)", "Student Experiencing OHC", paste0("Region ", c("1 (Northcentral)","2 (Northeast)", "3 (Milwaukee)",
                                                                                                  "4 (Southeast)", "5 (South)", "6 (West)")," * OHC"),
                                 paste0("Region ", c("1", "2", "3", "4", "5", "6")), student_control_labels, grade_dummy_labels,
                                 school_control_labels, acad_year_labels),
                  label = "region_models",
                  caption = "Academic Outcomes by Region"),
        file = paste0(p_dir_out, "models_region.tex"))
    
  }
  
    