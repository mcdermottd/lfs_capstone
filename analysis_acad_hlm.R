######################################################################
# notes:
# - purpose:
# - inputs: formatted analysis set
# - outputs:
# - keywords: #brule
# - general:
######################################################################

#######################################
# load packages and clear objects/log #
#######################################

  # clear objects log
  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")

  # load packages
  library(lme4)
  library(arm)
  library(merTools)
  library(shiny)
  library(apsrtable)
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
  in_outcomes_set <- data.table(read.csv("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata", stringsAsFactors = FALSE))

###############################
# structure vars for analysis #
###############################

  # copy input sets
  full_outcomes_set <- copy(in_outcomes_set)
  
  # transform school enrollment var (log)
  full_outcomes_set[, sch_pup_ct_log := ifelse(sch_pupil_count != 0, log(sch_pupil_count), 0)]
  
  # grand center outcome vars
  full_outcomes_set[, ':=' (nxt_math_ctr = nxt_zscore_math_kce - mean(nxt_zscore_math_kce, na.rm = TRUE),
                            nxt_rdg_ctr = nxt_zscore_rdg_kce - mean(nxt_zscore_rdg_kce, na.rm = TRUE))]
  
  # create scaler to re-convert school vars
  full_outcomes_set[, scaler := sch_pupil_count / 1000]
  
  # unscale school variables
  full_outcomes_set[, ":="(sch_frl_count = sch_frl_scaled * scaler, sch_sped_count = sch_sped_scaled * scaler, 
                           sch_elp_count = sch_elp_scaled * scaler, sch_non_white_count = sch_non_white_scaled * scaler)]
  
  # create school percentage vars
  full_outcomes_set[, ":="(sch_per_frl = sch_frl_count / sch_pupil_count, sch_per_sped = sch_sped_count / sch_pupil_count, 
                           sch_per_elp = sch_elp_count / sch_pupil_count, sch_per_non_white = sch_non_white_count / sch_pupil_count)]
  
########################
# subset analysis data #
########################
  
  # sort by academic year
  setorder(full_outcomes_set, acad_year)
  
  # remove entries with missing regions and counties #brule
  analysis_sample <- subset(full_outcomes_set, lf_region != "missing")
  analysis_sample <- subset(analysis_sample, lf_county != "missing")

  # create set with only analysis grades (7 - 12) #brule
  analysis_sample <- subset(analysis_sample, flag_analysis_grd == 1)

  # remove observations for ohc students occuring prior to a placement #brule
  analysis_sample <- subset(analysis_sample, !(flag_ohc == 1 & flag_cur_plcmt == 0 & flag_prior_plcmt == 0))
  
  # create set with only 7th and 9th graders with next year test score for wkce analysis #brule
  analysis_wkce <- subset(analysis_sample, (grade %in% c("07", "09") & !is.na(nxt_zscore_math_kce)))

###################################
# summarize variables of interest #
###################################
  
  # subset to ohc and outcome vars to summarize
  melt_vars <- subset(analysis_wkce, select = c(lf_child_id, lf_sch_id, lf_region, lf_county, acad_year, flag_cur_plcmt, flag_prior_plcmt, 
                                                  nxt_math_ctr, nxt_rdg_ctr, age_in_years_cd, d_male, d_female, d_elp, d_sped, d_frl, d_race_white,
                                                  d_race_black, d_race_hispanic, d_race_asian, d_race_indian, sch_pup_ct_log, sch_per_frl, 
                                                  sch_per_sped, sch_per_elp, sch_per_non_white, sch_mean_math_z_score, sch_mean_rdg_z_score))
  
  # melt ohc data long to summarize
  summ_vars_long <- melt.data.table(melt_vars, id.vars = c("lf_child_id", "lf_sch_id", "lf_region", "lf_county", "acad_year"))
  
  # remove NA values
  summ_vars_long <- subset(summ_vars_long, !is.na(value))
  
  # table - summarize vars overall
  a_summ_vars <- summ_vars_long[, list(n_obs = length(value),
                                       mean = round(mean(value), 3),
                                       sd = round(sd(value), 3), 
                                       min = round(min(value), 3),
                                       max = round(max(value), 3)),
                                by = variable]
  
  # table - number of students by county
  a_cty_counts <- melt_vars[, .N, by = c("lf_county", "flag_cur_plcmt", "flag_prior_plcmt")]
  
#############################
# set regression parameters #
#############################
  
  # create list of student control vars
  lm_student_controls <- c("age_in_years_cd", "d_male", "d_frl", "d_sped", "d_elp", "d_race_black", "d_race_hispanic", "d_race_asian", 
                            "d_race_indian")

  # create list of school covariates
  lm_sch_controls <- c("sch_pup_ct_log", "sch_pup_ct_log", "sch_per_frl", "sch_per_sped", "sch_per_elp", "sch_per_non_white", "sch_mean_math_z_score",
                       "sch_mean_rdg_z_score")
  
  # create list of year dummies
  lm_dummies_yr <- c("d_acad_year_2009", "d_acad_year_2010", "d_acad_year_2011", "d_acad_year_2012")
  
  # combine for full set of controls
  lm_controls_wkce <- paste(c(lm_student_controls, "d_grade_09", lm_sch_controls, lm_dummies_yr), collapse = " + ")

###############################################################
# investigate outcome with different model forms (attendance) #
###############################################################

  # lm: attendance on OHC flags, pooled across counties
  lm_formula <- paste("nxt_math_ctr ~ flag_cur_plcmt + flag_prior_plcmt + ", lm_controls_wkce)
  m1a_m_kce_pooled <- lm(lm_formula, data = analysis_sample)

  # glm: attendance on OHC flags, grouped by county
  glm_formula <- paste("nxt_math_ctr ~ flag_cur_plcmt + flag_prior_plcmt + lf_county + ", lm_controls_wkce)
  m1b_m_kce_grp <- glm(glm_formula, data = analysis_sample)

  # lmer: attendance on empty model, grouped by county
  m1c_m_kce_hlm_empty <- lmer(nxt_math_ctr ~ (1 | lf_county), data = analysis_sample, REML = FALSE)

################################################
# main hlm models - attendance and wkce scores #
################################################
  
  # lmer: wkce math, random region intercepts with current placement at level-2
  lmer_formula <- paste("nxt_math_ctr ~ flag_cur_plcmt + flag_prior_plcmt + (1 + flag_cur_plcmt | lf_county) + ", lm_controls_wkce)
  m2a_m_kce_hlm <- lmer(lmer_formula, data = analysis_sample, REML = FALSE)
  
  # save tau matrix
  m2a_tau_mat <- as.data.table(VarCorr(m2a_m_kce_hlm))
  
  # lmer: wkce reading, random region intercepts with current placement at level-2
  lmer_formula <- paste("nxt_rdg_ctr ~ flag_cur_plcmt + flag_prior_plcmt + (1 + flag_cur_plcmt | lf_county) + ", lm_controls_wkce)
  m2b_r_kce_hlm <- lmer(lmer_formula, data = analysis_sample, REML = FALSE)
  
  # save tau matrix
  m2b_tau_mat <- as.data.table(VarCorr(m2b_r_kce_hlm))
  
################################################
# compute county ranks (expectedRank function) #
################################################
  
  # compute county ranks
  ranks_kce_math <- data.table(expectedRank(m2a_m_kce_hlm, groupFctr = "lf_county", term = "flag_cur_plcmt"))
  ranks_kce_rdg  <- data.table(expectedRank(m2b_r_kce_hlm, groupFctr = "lf_county", term = "flag_cur_plcmt"))
  
  # create shiny app with diagnostic info
  # shinyMer(m1e_attend_hlm_cur, simData = analysis_sample[1:100, ])
  
######################
# format export vars #
######################

  # set output director
  p_dir_out <- "C:/Users/Drew/Dropbox/course_data/edpsych_964/final_paper/" 
  
  # create vector of student control labels
  student_control_labels <- c("Age", "Male", "Eligible for Free or Reduced Price Lunch (FRL)", "Disability (SPED)", 
                              "Limited English Proficiency (ELP)", "Race - Black", "Race - Hispanic", "Race - Asian", "Race - Indian")
                    
  # create vector of school control lables          
  school_control_labels <- paste0("School: ", c("Total Enrollment (Log)", "Number of FRL Students", "SPED Students", "ELP Students", 
                                                "Non-White Students", "Removals", "Avg. WKCE Math Score", "Avg. WKCE Reading Score"))
                                  
  # create vector of academic year lables
  acad_year_labels <- paste0("Acad. Year: ", c("2009", "2010", "2011", "2012"))

##########
# export #
##########
  
  # export
  if (p_opt_exp == 1) { 
    
    # output summary stats
    fwrite(a_summ_vars, paste0(p_dir_out, "outcomes_summary_overall.csv")) } 
    

    # output attendance model
    # cat(apsrtable(m1e_attend_hlm_cur, 
    #               lev = 0,
    #               Sweave = FALSE,
    #               float = "longtable",
    #               coef.names = c("(Intercept)", "Placement in Prior Academic Year", student_control_labels, grade_dummy_labels, school_control_labels,
    #                              acad_year_labels),
    #               label = "attendance_model",
    #               caption = "Attendance Rate"),
    #     file = paste0(p_dir_out, "models_attendance.tex"))
    
  }
  
    