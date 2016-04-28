######################################################################
# notes:
# - purpose: create summary tables of demographics and OHC characteristics
# - inputs: formatted analysis set
# - outputs: stacked tables summarizing data characteristics
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

###############
# format data #
###############

  # make copy of analysis set
  full_outcomes_set <- copy(in_outcomes_set)
  
  # sort by child id, placements, demographics, and academic year
  setorder(full_outcomes_set, lf_child_id, -flag_cur_plcmt, d_male, acad_year, na.last = TRUE)
  
  # create set with only hs students
  analysis_sample <- subset(full_outcomes_set, flag_analysis_grd == 1)
  
  # create set with one row per child, keeping rows with acad year info and demographics first #brule
  full_demo_set <- ea_no_dups(full_outcomes_set, "lf_child_id")
  analysis_demo_set <- ea_no_dups(analysis_sample, "lf_child_id")
  
  # sort sets by academic year
  setorder(full_outcomes_set, acad_year) 
  setorder(analysis_sample, acad_year) 
  setorder(full_demo_set, acad_year) 
  setorder(analysis_demo_set, acad_year) 

###############################
# examine schools in data set #
###############################
  
    # count the number of schools overall
    sch_count <- full_outcomes_set[, list(n_schools = uniqueN(lf_sch_id))]
    sch_count_hs <- analysis_sample[, list(n_schools = uniqueN(lf_sch_id))]

    # count the number of schools by region
    sch_count_reg <- full_outcomes_set[, list(n_schools = uniqueN(lf_sch_id)), by = lf_region]
    sch_count_reg_hs <- analysis_sample[, list(n_schools = uniqueN(lf_sch_id)), by = lf_region]

    # add vars to stack
    sch_count[, ":="(sch_type = "all", lf_region = "all")]
    sch_count_hs[, ":="(sch_type = "hs", lf_region = "all")]
    sch_count_reg[, sch_type := "all"]
    sch_count_reg_hs[, sch_type := "hs"]
    
    # stack together
    stacked_sch_count <- rbind(sch_count, sch_count_hs)
    stacked_sch_count <- rbind(stacked_sch_count, sch_count_reg)
    stacked_sch_count <- rbind(stacked_sch_count, sch_count_reg_hs)

################################
# examine overall demographics #
################################

  # calc overall demo summary
  a_demo_overall <- full_demo_set[, list(n_obs = .N,
                                     per_male = round(mean(d_male, na.rm = TRUE), 3),
                                     per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                     per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                     per_frl = round(mean(d_frl, na.rm = TRUE), 3),
                                     per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                     per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                     per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                     per_black = round(mean(d_race_black, na.rm = TRUE), 3),
                                     per_hispanic = round(mean(d_race_hispanic, na.rm = TRUE), 3),
                                     per_asian = round(mean(d_race_asian, na.rm = TRUE), 3),
                                     per_indian = round(mean(d_race_indian, na.rm = TRUE), 3))]

  # calc overall demo, ohc status
  a_demo_compare <- full_demo_set[, list(n_obs = .N,
                                     per_male = round(mean(d_male, na.rm = TRUE), 3),
                                     per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                     per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                     per_frl = round(mean(d_frl, na.rm = TRUE), 3),
                                     per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                     per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                     per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                     per_black = round(mean(d_race_black, na.rm = TRUE), 3),
                                     per_hispanic = round(mean(d_race_hispanic, na.rm = TRUE), 3),
                                     per_asian = round(mean(d_race_asian, na.rm = TRUE), 3),
                                     per_indian = round(mean(d_race_indian, na.rm = TRUE), 3)),
                              by = c("flag_ohc")]
  
  
  # calc overall demo, ohc status and frl 
  a_demo_compare_frl <- full_demo_set[, list(n_obs = .N,
                                         per_male = round(mean(d_male, na.rm = TRUE), 3),
                                         per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                         per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                         per_frl = round(mean(d_frl, na.rm = TRUE), 3),
                                         per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                         per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                         per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                         per_black = round(mean(d_race_black, na.rm = TRUE), 3),
                                         per_hispanic = round(mean(d_race_hispanic, na.rm = TRUE), 3),
                                         per_asian = round(mean(d_race_asian, na.rm = TRUE), 3),
                                         per_indian = round(mean(d_race_indian, na.rm = TRUE), 3)),
                              by = c("flag_ohc", "flag_compare_frl")]

  # add vars to stack
  a_demo_overall[, set := "overall"]
  a_demo_compare[, set := "compare"]
  a_demo_compare_frl[, set := "frl"]

  # stack together
  stacked_demo_compare <- rbind(a_demo_overall, a_demo_compare, fill = TRUE)
  stacked_demo_compare <- rbind(stacked_demo_compare, a_demo_compare_frl, fill = TRUE)
  
  # reorder vars
  ea_colorder(stacked_demo_compare, c("set", "flag_ohc", "flag_compare_frl"))

##########################################################
# examine demographics for analysis years (grade 7 - 12) #
##########################################################
  
  # calc overall demo, hs subset, ohc status
  a_demo_compare_hs <- analysis_demo_set[, list(n_obs = .N,
                                           per_male = round(mean(d_male, na.rm = TRUE), 3),
                                           per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                           per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                           per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                           per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                           per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                           per_black = round(mean(d_race_black, na.rm = TRUE), 3),
                                           per_hispanic = round(mean(d_race_hispanic, na.rm = TRUE), 3),
                                           per_asian = round(mean(d_race_asian, na.rm = TRUE), 3),
                                           per_indian = round(mean(d_race_indian, na.rm = TRUE), 3)),
                                    by = c("flag_ohc")]
  
  # calc overall demo, hs subset, ohc status and frl
  a_demo_compare_hs_frl <- analysis_demo_set[, list(n_obs = .N,
                                               per_male = round(mean(d_male, na.rm = TRUE), 3),
                                               per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                               per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                               per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                               per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                               per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                               per_black = round(mean(d_race_black, na.rm = TRUE), 3),
                                               per_hispanic = round(mean(d_race_hispanic, na.rm = TRUE), 3),
                                               per_asian = round(mean(d_race_asian, na.rm = TRUE), 3),
                                               per_indian = round(mean(d_race_indian, na.rm = TRUE), 3)),
                                  by = c("flag_ohc", "flag_compare_frl")]

  
  # add vars to stack
  a_demo_compare_hs[, set := "compare"]
  a_demo_compare_hs_frl[, set := "frl"]

  # stack together
  stacked_demo_compare_hs <- rbind(a_demo_compare_hs, a_demo_compare_hs_frl, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_demo_compare_hs, c("set", "flag_ohc", "flag_compare_frl"))
  
##########################################
# create long ohc demo data to summarize #
##########################################
  
  # subset to data for melt
  sub_full_demo <- subset(full_demo_set, flag_ohc == 1, select = c(lf_child_id, n_ohc_tot, tot_ohc_days, lf_n_plcmt_tot, tot_plcmt_days, d_male,
                                                                   d_female, d_elp, d_sped, d_frl, d_fpl, d_rpl, d_race_white, d_race_black, 
                                                                   d_race_hispanic, d_race_asian, d_race_indian))
  sub_analysis_demo <- subset(analysis_demo_set, flag_ohc == 1, select = c(lf_child_id, n_ohc_tot, tot_ohc_days, lf_n_plcmt_tot, tot_plcmt_days,
                                                                           d_male, d_female, d_elp, d_sped, d_frl, d_fpl, d_rpl, d_race_white,
                                                                           d_race_black, d_race_hispanic, d_race_asian, d_race_indian))
  
  # create var for avg. length of placement
  sub_full_demo[, plcmt_length := tot_plcmt_days / lf_n_plcmt_tot]
  sub_analysis_demo[, plcmt_length := tot_plcmt_days / lf_n_plcmt_tot]

  # melt ohc data long to summarize
  ohc_full_long <- melt.data.table(sub_full_demo, id.vars = c("lf_child_id", "d_male", "d_female", "d_elp", "d_sped", "d_frl", "d_fpl", "d_rpl",
                                                             "d_race_white", "d_race_black", "d_race_hispanic", "d_race_asian", "d_race_indian"))
  ohc_analysis_long <- melt.data.table(sub_analysis_demo, id.vars = c("lf_child_id", "d_male", "d_female", "d_elp", "d_sped", "d_frl", "d_fpl", "d_rpl",
                                                                   "d_race_white", "d_race_black", "d_race_hispanic", "d_race_asian", 
                                                                   "d_race_indian"))
  # remove NA values
  ohc_full_long <- subset(ohc_full_long, !is.na(value))
  ohc_analysis_long <- subset(ohc_analysis_long, !is.na(value))

##################################
# examine overall ohc statistics #
##################################
  
  # calc ohc stats overall
  a_ohc_stats_overall <- ohc_full_long[, list(n_obs = length(value),
                                               min = min(value),
                                               q25 = quantile(value, .25),
                                               q50 = quantile(value, .5),
                                               q75 = quantile(value, .75),
                                               max = max(value),
                                               mean = round(mean(value), 3),
                                               var = round(var(value), 3),
                                               sd = round(sd(value), 3)), 
                                       by = c("variable")]
  
  # calc ohc stats overall
  a_ohc_stats_hs_overall <- ohc_analysis_long[, list(n_obs = length(value),
                                                     min = min(value),
                                                     q25 = quantile(value, .25),
                                                     q50 = quantile(value, .5),
                                                     q75 = quantile(value, .75),
                                                     max = max(value),
                                                     mean = round(mean(value), 3),
                                                     var = round(var(value), 3),
                                                     sd = round(sd(value), 3)), 
                                             by = c("variable")]
  
  # calc ohc stats by gender
  a_ohc_by_gender_hs <- ohc_analysis_long[, list(n_obs = length(value),
                                                min = min(value),
                                                q25 = quantile(value, .25),
                                                q50 = quantile(value, .5),
                                                q75 = quantile(value, .75),
                                                max = max(value),
                                                mean = round(mean(value), 2),
                                                var = round(var(value), 2),
                                                sd = round(sd(value), 2)),
                                         by = c("variable", "d_male")]

  # calc ohc stats by race (white vs non-white)
  a_ohc_by_race_hs <- ohc_analysis_long[, list(n_obs = length(value),
                                              min = min(value),
                                              q25 = quantile(value, .25),
                                              q50 = quantile(value, .5),
                                              q75 = quantile(value, .75),
                                              max = max(value),
                                              mean = round(mean(value), 2),
                                              var = round(var(value), 2),
                                              sd = round(sd(value), 2)),
                                       by = c("variable", "d_race_white")]

  # add vars to stack
  a_ohc_stats_overall[, set := "overall"]
  a_ohc_stats_hs_overall[, set := "hs"]
  a_ohc_by_gender_hs[, set := "hs_gender"]
  a_ohc_by_race_hs[, set := "hs_race"]

  # stack together
  stacked_ohc_compare <- rbind(a_ohc_stats_overall, a_ohc_stats_hs_overall, fill = TRUE)
  stacked_ohc_compare <- rbind(stacked_ohc_compare, a_ohc_by_gender_hs, fill = TRUE)
  stacked_ohc_compare <- rbind(stacked_ohc_compare, a_ohc_by_race_hs, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_ohc_compare, c("set", "d_male", "d_race_white"))

#########################################
# summarize ohc flags overall and in hs #
#########################################
  
  # count placement flags
  a_plcmt_flags <- full_outcomes_set[flag_ohc == 1, list(n_ohc_obs = .N,
                                                    n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                    n_prior_plcmt = sum(flag_prior_plcmt == 1))]
  
  # count placement flags
  a_plcmt_flags_hs <- analysis_sample[flag_ohc == 1, list(n_ohc_obs = .N,
                                                          n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                          n_prior_plcmt = sum(flag_prior_plcmt == 1))]
  
  # count placement flags by acad yr
  a_plcmt_flags_yr <- analysis_sample[flag_ohc == 1, list(n_ohc_obs = .N,
                                                          n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                          n_prior_plcmt = sum(flag_prior_plcmt == 1)),
                                      by = acad_year]
  
  # count placement flags by region
  a_plcmt_flags_reg <- analysis_sample[flag_ohc == 1, list(n_ohc_obs = .N,
                                                           n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                           n_prior_plcmt = sum(flag_prior_plcmt == 1)),
                                       by = lf_region]
  
  # create pre placement var
  a_plcmt_flags[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]
  a_plcmt_flags_hs[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]
  a_plcmt_flags_yr[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]
  a_plcmt_flags_reg[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]

  # add vars to stack
  a_plcmt_flags[, set := "overall"]
  a_plcmt_flags_hs[, set := "hs"]
  a_plcmt_flags_yr[, set := "hs_year"]
  a_plcmt_flags_reg[, set := "hs_region"]

  # stack together
  stacked_plcmt_flags <- rbind(a_plcmt_flags, a_plcmt_flags_hs, fill = TRUE)
  stacked_plcmt_flags <- rbind(stacked_plcmt_flags, a_plcmt_flags_yr, fill = TRUE)
  stacked_plcmt_flags <- rbind(stacked_plcmt_flags, a_plcmt_flags_reg, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_plcmt_flags, c("set", "acad_year", "lf_region"))

########################################################
# examine number of placements - by year, type, region #
########################################################
  
  # create set with only placement years
  plcmt_data <- subset(analysis_sample, flag_cur_plcmt == 1)
  
  # calc number of placement types
  a_num_plcmts <- plcmt_data[, list(n_plcmts = .N,
                                       n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                       n_fhome_rel = sum(p_type == "fhome_rel"),
                                       n_group_home = sum(p_type == "group_home"),
                                       n_rcc = sum(p_type == "rcc"),
                                       n_other = sum(p_type == "other"))]
  
  # calc number of placement types by acad yr
  a_num_plcmts_yr <- plcmt_data[, list(n_plcmts = .N,
                                           n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                           n_fhome_rel = sum(p_type == "fhome_rel"),
                                           n_group_home = sum(p_type == "group_home"),
                                           n_rcc = sum(p_type == "rcc"),
                                           n_other = sum(p_type == "other")),
                                   by = acad_year]
  
  # calc number of placement types by region
  a_num_plcmts_reg <- plcmt_data[, list(n_plcmts = .N,
                                           n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                           n_fhome_rel = sum(p_type == "fhome_rel"),
                                           n_group_home = sum(p_type == "group_home"),
                                           n_rcc = sum(p_type == "rcc"),
                                           n_other = sum(p_type == "other")),
                                    by = lf_region]

  # calc number of placement types by acad yr and region
  a_num_plcmts_yr_reg <- plcmt_data[, list(n_plcmts = .N,
                                             n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                             n_fhome_rel = sum(p_type == "fhome_rel"),
                                             n_group_home = sum(p_type == "group_home"),
                                             n_rcc = sum(p_type == "rcc"),
                                             n_other = sum(p_type == "other")),
                                       by = c("acad_year", "lf_region")]
      
  # add vars to stack
  a_num_plcmts[, set := "overall"]
  a_num_plcmts_yr[, set := "acad_year"]
  a_num_plcmts_reg[, set := "region"]
  a_num_plcmts_yr_reg[, set := "acad_yr_reg"]

  # stack together
  stacked_num_plcmts <- rbind(a_num_plcmts, a_num_plcmts_yr, fill = TRUE)
  stacked_num_plcmts <- rbind(stacked_num_plcmts, a_num_plcmts_reg, fill = TRUE)
  stacked_num_plcmts <- rbind(stacked_num_plcmts, a_num_plcmts_yr_reg, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_num_plcmts, c("set", "acad_year", "lf_region"))
  
#######################################################
# examine demographics and placements characteristics #
#######################################################

  # create var for avg. length of placement
  plcmt_data[, plcmt_length_acad := tot_plcmt_days_acad / lf_n_plcmt_acad]

  # calc stats of ohc placements by year
  a_plcmt_by_yr_hs <- plcmt_data[, list(n_obs = .N,
                                           avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                           per_male = round(mean(d_male, na.rm = TRUE), 3),
                                           per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                           per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                           per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                           per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                           per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                           avg_plcmts = round(mean(lf_n_plcmt_acad), 3),
                                           avg_plcmt_days = round(mean(tot_plcmt_days_acad), 3),
                                           avg_plcmt_length = round(mean(plcmt_length_acad), 3)),
                                       by = acad_year]

  # calc stats of ohc placements by plcmt type
  a_plcmt_by_type_hs <- plcmt_data[, list(n_obs = .N,
                                             avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                             per_male = round(mean(d_male, na.rm = TRUE), 3),
                                             per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                             per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                             per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                             per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                             per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                             avg_plcmts = round(mean(lf_n_plcmt_acad), 3),
                                             avg_plcmt_days = round(mean(tot_plcmt_days_acad), 3),
                                             avg_plcmt_length = round(mean(plcmt_length_acad), 3)),
                                      by = p_type]
  
  # calc stats of ohc placements by region
  a_plcmt_by_region_hs <- plcmt_data[, list(n_obs = .N,
                                               avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                               per_male = round(mean(d_male, na.rm = TRUE), 3),
                                               per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                               per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                               per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                               per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                               per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                               avg_plcmts = round(mean(lf_n_plcmt_acad), 3),
                                               avg_plcmt_days = round(mean(tot_plcmt_days_acad), 3),
                                               avg_plcmt_length = round(mean(plcmt_length_acad), 3)),
                                        by = lf_region]

  # add vars to stack
  a_plcmt_by_yr_hs[, set := "acad_year"]
  a_plcmt_by_type_hs[, set := "ptype"]
  a_plcmt_by_region_hs[, set := "region"]

  # stack together
  stacked_plcmt_info <- rbind(a_plcmt_by_yr_hs, a_plcmt_by_type_hs, fill = TRUE)
  stacked_plcmt_info <- rbind(stacked_plcmt_info, a_plcmt_by_region_hs, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_plcmt_info, c("set", "acad_year", "p_type", "lf_region"))
  
#####################
# format and export #
#####################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/descriptive/"

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(stacked_sch_count, paste0(p_dir_out, "school_counts.csv"))
    ea_write(stacked_demo_compare, paste0(p_dir_out, "demo_compare_overall.csv"))
    ea_write(stacked_demo_compare_hs, paste0(p_dir_out, "demo_compare_hs.csv"))
    ea_write(stacked_ohc_compare, paste0(p_dir_out, "ohc_compare.csv"))
    ea_write(stacked_plcmt_flags, paste0(p_dir_out, "num_plcmts_flags.csv"))
    ea_write(stacked_num_plcmts, paste0(p_dir_out, "num_plcmts_by_type.csv"))
    ea_write(stacked_plcmt_info, paste0(p_dir_out, "plcmt_compare.csv"))

  }
  
  
  
