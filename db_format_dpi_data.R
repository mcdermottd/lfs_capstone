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
  library(eaanalysis)

#############
# set parms #
#############

  # output toggle
  p_opt_exp <- 0

#############
# load data #
#############

  # load ed outcomes data
  in_stacked_dpi <- fread("X:/LFS-Education Outcomes/data/raw_data/DCFmatchedSample03012016.csv")
  
  # load school attributes
  in_school_att <- fread("X:/LFS-Education Outcomes/data/raw_data/DPI_SCHOOL_ATTRIBUTES.csv")
  
  # load wkce summary score info
  wkce_avgs <- fread("X:/LFS-Education Outcomes/documents/wkce_summary_stats.csv")
  
############################
# format dpi data to merge #
############################
  
  # copy dpi file
  format_stacked_dpi <- copy(in_stacked_dpi)
  
  # remove exact duplicates #brule
  format_stacked_dpi <- ea_no_dups(format_stacked_dpi, opt_key_all = 1)
  
  # convert variable names to lowercase
  setnames(format_stacked_dpi, colnames(format_stacked_dpi), tolower(colnames(format_stacked_dpi)))
  
  # create academic year var
  format_stacked_dpi[, acad_year := paste0("20", ea_scan(school_year, 2, "-"))]
  
#######################################
# create dummies for dpi demographics #
#######################################
  
  # create race dummies
  format_stacked_dpi[!is.na(gender_code_cd), d_male := ifelse(gender_code_cd == "M", 1, 0)]
  format_stacked_dpi[!is.na(gender_code_cd), d_female := ifelse(gender_code_cd == "F", 1, 0)]

  # convert elp scale to dummy variable
  format_stacked_dpi[!is.na(elp_code_cd), d_elp := ifelse(elp_code_cd <= 5, 1, 0)]
  
  # convert disability code to dummy variable
  format_stacked_dpi[!is.na(disab_ye), d_sped := ifelse(disab_ye == "N", 0, 1)]
  
  # convert frl code to dummy variable
  format_stacked_dpi[!is.na(frl_ye), d_frl := ifelse(frl_ye == "N", 0, 1)]
  format_stacked_dpi[!is.na(frl_ye), d_fpl := ifelse(frl_ye == "F", 1, 0)]
  format_stacked_dpi[!is.na(frl_ye), d_rpl := ifelse(frl_ye %in% c("A", "R"), 1, 0)]

  # dummy out race variable
  format_stacked_dpi <- db_dummy(format_stacked_dpi, "race_eth_code_cd", opt_data_frequency = 0)
  
  # rename dummied race variables
  setnames(format_stacked_dpi, c("d_race_eth_code_cd_W", "d_race_eth_code_cd_missing", "d_race_eth_code_cd_I", "d_race_eth_code_cd_B", 
                                 "d_race_eth_code_cd_H", "d_race_eth_code_cd_A"), c("d_race_white", "d_race_missing", "d_race_indian", "d_race_black",
                                                                                 "d_race_hispanic", "d_race_asian"))
  
  # set all race variables to missing, if race_missing == 1
  format_stacked_dpi[d_race_missing == 1, c("d_race_white", "d_race_missing", "d_race_indian", "d_race_black", "d_race_hispanic", 
                                            "d_race_asian") := NA]

###############################################
# subset to one row per student per acad year #
###############################################
  
  # sort based on child id, school year, and test scores (to remove rows with missing scores first) #brule
  setorder(format_stacked_dpi, lds_student_key, acad_year, math_kce_scale_score, rdg_kce_scale_score, na.last = TRUE)
   
  # remove duplicates based on child_id and school_year #brule
  stacked_dpi_no_dups <- ea_no_dups(format_stacked_dpi, c("lds_student_key", "acad_year"))  

###########################
# standardize test scores #
###########################

  # copy wkce avg file
  wkce_avgs_merge <- copy(wkce_avgs)
  
  # create vars for merge
  wkce_avgs_merge[, ":="(grade_level_cd = as.character(grade), acad_year = as.character(acad_year))]
  
  # add additional 0 to grade var for merge
  wkce_avgs_merge[nchar(grade_level_cd) == 1, grade_level_cd := paste0("0", grade_level_cd)]
  
  # merge avgs with main data set
  stacked_dpi_standard <- ea_merge(stacked_dpi_no_dups, wkce_avgs_merge, c("acad_year", "grade_level_cd"))
  
  # create z-scored scores
  stacked_dpi_standard[, zscore_math_kce := (math_kce_scale_score - math_score_mean) / math_score_sd]
  stacked_dpi_standard[, zscore_rdg_kce := (rdg_kce_scale_score - rdg_score_mean) / rdg_score_sd]
  
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
  
  
  
