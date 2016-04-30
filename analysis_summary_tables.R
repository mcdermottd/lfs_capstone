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
    sch_count_sub <- analysis_sample[, list(n_schools = uniqueN(lf_sch_id))]

    # count the number of schools by region
    sch_count_reg <- full_outcomes_set[, list(n_schools = uniqueN(lf_sch_id)), by = lf_region]
    sch_count_reg_sub <- analysis_sample[, list(n_schools = uniqueN(lf_sch_id)), by = lf_region]

    # add vars for stacking
    sch_count[, ":="(sch_type = "all", lf_region = "all")]
    sch_count_sub[, ":="(sch_type = "analysis_sample", lf_region = "all")]
    sch_count_reg[, sch_type := "all"]
    sch_count_reg_sub[, sch_type := "analysis_sample"]
    
    # stack together
    stacked_sch_count <- rbind(sch_count, sch_count_sub)
    stacked_sch_count <- rbind(stacked_sch_count, sch_count_reg)
    stacked_sch_count <- rbind(stacked_sch_count, sch_count_reg_sub)

##############################################
# examine overall demographics - full sample #
##############################################

  # table - overall demographics
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

  # table - overall demographics by ohc status (OHC or comparison group)
  a_demo_ohc_status <- full_demo_set[, list(n_obs = .N,
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
                                     by = flag_ohc]
  
  
  # table - overall demographics by ohc and frl statuses 
  a_demo_ohc_frl <- full_demo_set[, list(n_obs = .N,
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
                                  by = list(flag_ohc, flag_compare_frl)]

  # add vars for stacking
  a_demo_overall[, ":="(set = "full", type = "overall")]
  a_demo_ohc_status[, ":="(set = "full", type = "ohc_compare")]
  a_demo_ohc_frl[, ":="(set = "full", type = "frl_compare")]

  # stack together
  stacked_demo_overall <- rbind(a_demo_overall, a_demo_ohc_status, fill = TRUE)
  stacked_demo_overall <- rbind(stacked_demo_overall, a_demo_ohc_frl, fill = TRUE)

##########################################################
# examine demographics for analysis years (grade 7 - 12) #
##########################################################
  
  # table - demographics of analysis sample
  a_demo_sub <- analysis_demo_set[, list(n_obs = .N,
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
  
  # table - demographics of analysis sample by ohc status
  a_demo_sub_ohc <- analysis_demo_set[, list(n_obs = .N,
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
                                      by = flag_ohc]
  
  # table - demographics of analysis sample by ohc and frl statuses
  a_demo_sub_ohc_frl <- analysis_demo_set[, list(n_obs = .N,
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
                                          by = list(flag_ohc, flag_compare_frl)]

  
  # add vars for stacking
  a_demo_sub[, ":="(set = "analysis", type = "overall")]
  a_demo_sub_ohc[, ":="(set = "analysis", type = "ohc_compare")]
  a_demo_sub_ohc_frl[, ":="(set = "analysis", type = "frl_compare")]

  # stack together
  stacked_demo_analysis <- rbind(a_demo_sub, a_demo_sub_ohc, fill = TRUE)
  stacked_demo_analysis <- rbind(stacked_demo_analysis, a_demo_sub_ohc_frl, fill = TRUE)

  # stack with overall demographics
  stacked_full_demo <- rbind(stacked_demo_overall, stacked_demo_analysis)
  
  # reorder vars
  ea_colorder(stacked_full_demo, c("set", "type", "flag_ohc", "flag_compare_frl"))
  
##########################################
# create long ohc demo data to summarize #
##########################################
  
  # subset to data for melt
  full_demo_ohc <- subset(full_demo_set, flag_ohc == 1, select = c(lf_child_id, n_ohc_tot, tot_ohc_days, lf_n_plcmt_tot, tot_plcmt_days, d_male, 
                                                                   d_elp, d_sped, d_frl, d_race_white, d_race_black, d_race_hispanic))
  analysis_demo_ohc <- subset(analysis_demo_set, flag_ohc == 1, select = c(lf_child_id, n_ohc_tot, tot_ohc_days, lf_n_plcmt_tot, tot_plcmt_days,
                                                                           d_male, d_elp, d_sped, d_frl, d_race_white, d_race_black, d_race_hispanic))
  
  # create var for avg. length of placement
  full_demo_ohc[, plcmt_length := tot_plcmt_days / lf_n_plcmt_tot]
  analysis_demo_ohc[, plcmt_length := tot_plcmt_days / lf_n_plcmt_tot]

  # melt ohc data long to summarize
  full_demo_ohc_long <- melt.data.table(full_demo_ohc, id.vars = c("lf_child_id", "d_male", "d_elp", "d_sped", "d_frl", "d_race_white", 
                                                                   "d_race_black", "d_race_hispanic"))
  analysis_demo_ohc_long <- melt.data.table(analysis_demo_ohc, id.vars = c("lf_child_id", "d_male", "d_elp", "d_sped", "d_frl", "d_race_white", 
                                                                           "d_race_black", "d_race_hispanic"))
  
  # remove NA values
  full_demo_ohc_long <- subset(full_demo_ohc_long, !is.na(value))
  analysis_demo_ohc_long <- subset(analysis_demo_ohc_long, !is.na(value))

##################################
# examine overall ohc statistics #
##################################
  
  # table - ohc placement stats overall
  a_ohc_stats <- full_demo_ohc_long[, list(n_obs = length(value),
                                           min = min(value),
                                           q25 = quantile(value, .25),
                                           q50 = quantile(value, .5),
                                           q75 = quantile(value, .75),
                                           max = max(value),
                                           mean = round(mean(value), 3),
                                           var = round(var(value), 3),
                                           sd = round(sd(value), 3)), 
                                    by = variable]
  
  # table - ohc placement stats for analysis sample
  a_ohc_sub <- analysis_demo_ohc_long[, list(n_obs = length(value),
                                                   min = min(value),
                                                   q25 = quantile(value, .25),
                                                   q50 = quantile(value, .5),
                                                   q75 = quantile(value, .75),
                                                   max = max(value),
                                                   mean = round(mean(value), 3),
                                                   var = round(var(value), 3),
                                                   sd = round(sd(value), 3)), 
                                            by = variable]
  
  # table - ohc placement stats for analysis sample by gender
  a_ohc_sub_gender <- analysis_demo_ohc_long[, list(n_obs = length(value),
                                                min = min(value),
                                                q25 = quantile(value, .25),
                                                q50 = quantile(value, .5),
                                                q75 = quantile(value, .75),
                                                max = max(value),
                                                mean = round(mean(value), 2),
                                                var = round(var(value), 2),
                                                sd = round(sd(value), 2)),
                                         by = list(variable, d_male)]

  # table - ohc placement stats for analysis sample by gender by race
  a_ohc_sub_race <- analysis_demo_ohc_long[, list(n_obs = length(value),
                                              min = min(value),
                                              q25 = quantile(value, .25),
                                              q50 = quantile(value, .5),
                                              q75 = quantile(value, .75),
                                              max = max(value),
                                              mean = round(mean(value), 2),
                                              var = round(var(value), 2),
                                              sd = round(sd(value), 2)),
                                       by = list(variable, d_race_white)]

  # add vars for stacking
  a_ohc_stats[, type := "overall"]
  a_ohc_sub[, type := "analysis_sample"]
  a_ohc_sub_gender[, type := "analysis_gender"]
  a_ohc_sub_race[, type := "analysis_race"]

  # stack together
  stacked_ohc_stats <- rbind(a_ohc_stats, a_ohc_sub, fill = TRUE)
  stacked_ohc_stats <- rbind(stacked_ohc_stats, a_ohc_sub_gender, fill = TRUE)
  stacked_ohc_stats <- rbind(stacked_ohc_stats, a_ohc_sub_race, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_ohc_stats, c("type", "d_male", "d_race_white"))

#########################################
# summarize ohc flags overall and in hs #
#########################################
  
  # freq - placement flags overall
  a_plcmt_flags <- full_outcomes_set[flag_ohc == 1, list(n_ohc_obs = .N,
                                                          n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                          n_prior_plcmt = sum(flag_prior_plcmt == 1))]
  
  # freq - placement flags in analysis sample
  a_plcmt_flags_sub <- analysis_sample[flag_ohc == 1, list(n_ohc_obs = .N,
                                                            n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                            n_prior_plcmt = sum(flag_prior_plcmt == 1))]
  
  # freq - placement flags by acad yr
  a_plcmt_flags_sub_yr <- analysis_sample[flag_ohc == 1, list(n_ohc_obs = .N,
                                                              n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                              n_prior_plcmt = sum(flag_prior_plcmt == 1)),
                                          by = acad_year]
  
  # freq - placement flags by region
  a_plcmt_flags_sub_reg <- analysis_sample[flag_ohc == 1, list(n_ohc_obs = .N,
                                                               n_cur_plcmt = sum(flag_cur_plcmt == 1),
                                                               n_prior_plcmt = sum(flag_prior_plcmt == 1)),
                                           by = lf_region]
  
  # create pre_placement counts
  a_plcmt_flags[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]
  a_plcmt_flags_sub[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]
  a_plcmt_flags_sub_yr[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]
  a_plcmt_flags_sub_reg[, n_pre_plcmt := n_ohc_obs - (n_cur_plcmt + n_prior_plcmt)]

  # add vars for stacking
  a_plcmt_flags[, type := "overall"]
  a_plcmt_flags_sub[, type := "analysis_sample"]
  a_plcmt_flags_sub_yr[, type := "analysis_sample_yr"]
  a_plcmt_flags_sub_reg[, type := "analysis_sample_reg"]

  # stack together
  stacked_plcmt_flags <- rbind(a_plcmt_flags, a_plcmt_flags_sub, fill = TRUE)
  stacked_plcmt_flags <- rbind(stacked_plcmt_flags, a_plcmt_flags_sub_yr, fill = TRUE)
  stacked_plcmt_flags <- rbind(stacked_plcmt_flags, a_plcmt_flags_sub_reg, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_plcmt_flags, c("type", "acad_year", "lf_region"))

##################################################
# examine placement type - by year, type, region #
##################################################
  
  # create set with only placement years
  plcmt_data <- subset(analysis_sample, flag_cur_plcmt == 1)
  
  # freq - placemtn types overall
  a_num_plcmts <- plcmt_data[, list(n_plcmts = .N,
                                     n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                     n_fhome_rel = sum(p_type == "fhome_rel"),
                                     n_group_home = sum(p_type == "group_home"),
                                     n_rcc = sum(p_type == "rcc"),
                                     n_other = sum(p_type == "other"))]
  
  # freq - placement types by acad yr
  a_num_plcmts_yr <- plcmt_data[, list(n_plcmts = .N,
                                       n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                       n_fhome_rel = sum(p_type == "fhome_rel"),
                                       n_group_home = sum(p_type == "group_home"),
                                       n_rcc = sum(p_type == "rcc"),
                                       n_other = sum(p_type == "other")),
                                by = acad_year]
  
  # freq - placement types by region
  a_num_plcmts_reg <- plcmt_data[, list(n_plcmts = .N,
                                         n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                         n_fhome_rel = sum(p_type == "fhome_rel"),
                                         n_group_home = sum(p_type == "group_home"),
                                         n_rcc = sum(p_type == "rcc"),
                                         n_other = sum(p_type == "other")),
                                 by = lf_region]

  # freq - placement types by acad yr and region
  a_num_plcmts_yr_reg <- plcmt_data[, list(n_plcmts = .N,
                                           n_fhome_nonrel = sum(p_type == "fhome_nonrel"),
                                           n_fhome_rel = sum(p_type == "fhome_rel"),
                                           n_group_home = sum(p_type == "group_home"),
                                           n_rcc = sum(p_type == "rcc"),
                                           n_other = sum(p_type == "other")),
                                    by = list(acad_year, lf_region)]
      
  # add vars for stacking
  a_num_plcmts[, type := "overall"]
  a_num_plcmts_yr[, type := "acad_year"]
  a_num_plcmts_reg[, type := "region"]
  a_num_plcmts_yr_reg[, type := "acad_yr_reg"]

  # stack together
  stacked_num_plcmts <- rbind(a_num_plcmts, a_num_plcmts_yr, fill = TRUE)
  stacked_num_plcmts <- rbind(stacked_num_plcmts, a_num_plcmts_reg, fill = TRUE)
  stacked_num_plcmts <- rbind(stacked_num_plcmts, a_num_plcmts_yr_reg, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_num_plcmts, c("type", "acad_year", "lf_region"))
  
#######################################################
# examine demographics and placements characteristics #
#######################################################

  # create var for avg. length of placement
  plcmt_data[, plcmt_length_acad := tot_plcmt_days_acad / lf_n_plcmt_acad]

  # table - acad year placement statistics overall
  a_plcmt <- plcmt_data[, list(n_obs = .N,
                               avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                               per_male = round(mean(d_male, na.rm = TRUE), 3),
                               per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                               per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                               per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                               per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                               per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                               n_plcmts_avg = round(mean(lf_n_plcmt_acad), 3),
                               n_plcmts_q50 = round(q50 = quantile(lf_n_plcmt_acad, .5)),
                               n_plcmts_sd = round(sd(lf_n_plcmt_acad), 3),
                               plcmt_days_avg = round(mean(tot_plcmt_days_acad), 3),
                               plcmt_days_q50 = round(q50 = quantile(tot_plcmt_days_acad, .5)),
                               plcmt_days_sd = round(sd(tot_plcmt_days_acad), 3))]

  # table - acad year placement statistics by year
  a_plcmt_by_yr <- plcmt_data[, list(n_obs = .N,
                                     avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                     per_male = round(mean(d_male, na.rm = TRUE), 3),
                                     per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                     per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                     per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                     per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                     per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                     n_plcmts_avg = round(mean(lf_n_plcmt_acad), 3),
                                     n_plcmts_q50 = round(q50 = quantile(lf_n_plcmt_acad, .5)),
                                     n_plcmts_sd = round(sd(lf_n_plcmt_acad), 3),
                                     plcmt_days_avg = round(mean(tot_plcmt_days_acad), 3),
                                     plcmt_days_q50 = round(q50 = quantile(tot_plcmt_days_acad, .5)),
                                     plcmt_days_sd = round(sd(tot_plcmt_days_acad), 3)),
                              by = acad_year]

  # table - acad year placement statistics by plcmt type
  a_plcmt_by_type <- plcmt_data[, list(n_obs = .N,
                                       avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                       per_male = round(mean(d_male, na.rm = TRUE), 3),
                                       per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                       per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                       per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                       per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                       per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                       n_plcmts_avg = round(mean(lf_n_plcmt_acad), 3),
                                       n_plcmts_q50 = round(q50 = quantile(lf_n_plcmt_acad, .5)),
                                       n_plcmts_sd = round(sd(lf_n_plcmt_acad), 3),
                                       plcmt_days_avg = round(mean(tot_plcmt_days_acad), 3),
                                       plcmt_days_q50 = round(q50 = quantile(tot_plcmt_days_acad, .5)),
                                       plcmt_days_sd = round(sd(tot_plcmt_days_acad), 3)),
                                by = p_type]
  
  # table - acad year placement statistics by region
  a_plcmt_by_region <- plcmt_data[, list(n_obs = .N,
                                         avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                         per_male = round(mean(d_male, na.rm = TRUE), 3),
                                         per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                         per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                         per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                         per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                         per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                         n_plcmts_avg = round(mean(lf_n_plcmt_acad), 3),
                                         n_plcmts_q50 = round(q50 = quantile(lf_n_plcmt_acad, .5)),
                                         n_plcmts_sd = round(sd(lf_n_plcmt_acad), 3),
                                         plcmt_days_avg = round(mean(tot_plcmt_days_acad), 3),
                                         plcmt_days_q50 = round(q50 = quantile(tot_plcmt_days_acad, .5)),
                                         plcmt_days_sd = round(sd(tot_plcmt_days_acad), 3)),
                                  by = lf_region]

  # add vars for stacking
  a_plcmt[, type := "overall"]
  a_plcmt_by_yr[, type := "acad_year"]
  a_plcmt_by_type[, type := "ptype"]
  a_plcmt_by_region[, type := "region"]

  # stack together
  stacked_plcmt_stats <- rbind(a_plcmt, a_plcmt_by_yr, fill = TRUE)
  stacked_plcmt_stats <- rbind(stacked_plcmt_stats, a_plcmt_by_type, fill = TRUE)
  stacked_plcmt_stats <- rbind(stacked_plcmt_stats, a_plcmt_by_region, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_plcmt_stats, c("type", "acad_year", "p_type", "lf_region"))
  
#####################
# format and export #
#####################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/descriptive/"

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(stacked_sch_count, paste0(p_dir_out, "freq_schools.csv"))
    ea_write(stacked_plcmt_flags, paste0(p_dir_out, "freq_type_ohc_obs_acad_yr.csv"))
    ea_write(stacked_num_plcmts, paste0(p_dir_out, "freq_plcmts_by_type_acad_yr.csv"))
    
    ea_write(stacked_full_demo, paste0(p_dir_out, "stats_student_demographics.csv"))
    ea_write(stacked_ohc_stats, paste0(p_dir_out, "stats_ohc.csv"))
    ea_write(stacked_plcmt_stats, paste0(p_dir_out, "stats_plcmt_acad_yr.csv"))

  }
  
  
  
