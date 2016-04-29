

###############################################
# initial format and remove duplicate entries #
###############################################
  
  # create list of vars to check on duplicates
  vars_dup <- colnames(sub_ohc_data)
  vars_nocheck <- c("site_region", "case_type", "child_person_type", "child_level_of_need", "provider_loc", "plcmt_with_sibling",
                    "days_plcmt_in_rpt_period")
  vars_dup <- setdiff(vars_dup, vars_nocheck)

  # subset based on duplicate vars list #brule
  sub_ohc_data <- ea_no_dups(sub_ohc_data, vars_dup)

  # sort based on child id, placement end date, and discharge variables (reverse sort discharge so "yes" appears first)
  setorder(sub_ohc_data, child_id, plcmt_end_date, -discharge, discharge_reason, end_reason)

  # remove discharge vars from no dup check
  vars_dup <- setdiff(vars_dup, c("discharge", "discharge_reason", "end_reason"))

  # remove additional duplicates ignoring discharge vars #brule
  sub_ohc_data <- ea_no_dups(sub_ohc_data, vars_dup)
   
################################
# create additional child vars #
################################

  # create gender female flag
  stacked_set[, flag_female := 0]
  stacked_set[child_gender == "Female", flag_female := 1]

  # create frequency of race / hispanic combinations
  a_freq_race <- ea_table(stacked_set, c("child_race", "child_hispanic"), opt_percent = 1)

  # create race flags #brule
  stacked_set[, c("flag_amer_indian", "flag_asian", "flag_black", "flag_pac_island", "flag_white", "flag_other") := 0]
  stacked_set[child_race == "American Indian/Alaskan Native", flag_amer_indian := 1]
  stacked_set[child_race == "Asian", flag_asian := 1]
  stacked_set[child_race == "Black/African American", flag_black := 1]
  stacked_set[child_race == "Native Hawaiian/Other Pacific Islande", flag_pac_island := 1]
  stacked_set[child_race == "White", flag_white := 1]
  stacked_set[child_race == "Unable to Determine" | is.na(child_race), flag_other := 1]

  # create hispanic flag
  stacked_set[, flag_hispanic := 0]
  stacked_set[child_hispanic == "Y", flag_hispanic := 1]

  # create disability flag
  stacked_set[, flag_disability := 0]
  stacked_set[child_disability == "Y", flag_disability := 1]

###########################
# standardize test scores #
###########################
  
  # subset to necessary vars for melt
  score_data_to_melt <- subset(dpi_acad_info, select = c(lf_child_id, acad_year, grade_level_cd, math_kce_scale_score, rdg_kce_scale_score))
  
  # melt test score data long to summarize
  score_data_long <- melt.data.table(score_data_to_melt, id.vars = c("lf_child_id", "acad_year", "grade_level_cd"))
  
  # sort data
  setorder(score_data_long, grade_level_cd, acad_year)
  
  # calc test stats by grade and acad yr
  a_kce_stats <- score_data_long[!is.na(value), list(n_obs = length(value),
                                                     min = min(value),
                                                     q25 = quantile(value, .25),
                                                     q50 = quantile(value, .5),
                                                     q75 = quantile(value, .75),
                                                     max = max(value),
                                                     mean = round(mean(value), 3),
                                                     var = round(var(value), 3),
                                                     sd = round(sd(value), 3)), 
                                 by = c("grade_level_cd", "acad_year", "variable")]
  
  # subset to vars for standardization
  sub_kce_stats <- subset(a_kce_stats, select = c(grade_level_cd, acad_year, variable, mean, sd))
  
  # cast wide by academic year
  kce_standardize <- data.table::dcast(sub_kce_stats, grade_level_cd + acad_year ~ variable, value.var = c("mean", "sd"))
  
#########
# plots #
#########
  
  # histogram - avg plcmt length in acad year, by academic year (15 day bins, <= 750 days)
  plot_hist_avg_days_acad_byr <- ggplot(data = subset(sub_ohc_acad_yr, acad_year > 2007 & acad_year < 2013), aes(x = avg_days_per_plcmt)) + 
                                         geom_histogram(binwidth = 10, colour = "black", fill = "dodgerblue4") +
                                         labs(x = "Average Days Per Placement", y = "Number of Children", 
                                              title = "Average Days Per Out of Home Care Placement Per Year \n - by Academic Year") + 
                                         plot_attributes +
                                         facet_wrap(~acad_year, ncol = 2)
  
#########################
# plot ohc info overall #
#########################
  
  # histogram - total ohc days (30 day bins, <= 1825 days)
  plot_hist_ohc_days_hs <- ggplot(data = plcmt_data_hs, aes(x = tot_ohc_days)) + 
                                   geom_histogram(binwidth = 30, colour = "black", fill = "dodgerblue4") +
                                   labs(x = "Days in OHC", y = "Number of Children", 
                                        title = "Total Days in Out-of-Home Care") + 
                                   plot_attributes
  
  # histogram - total ohc placements (1 day bins, <= 20 placements)
  plot_hist_ohc_plcmts_hs <- ggplot(data = subset(plcmt_data_hs, n_plcmt_tot <= 20), aes(x = n_plcmt_tot)) + 
                                     geom_histogram(binwidth = 1, colour = "black", fill = "dodgerblue4") +
                                     labs(x = "Number of Placements", y = "Number of Children", 
                                          title = "Total Placements in Out-of-Home Care") + 
                                     plot_attributes
  
  # histogram - avg plcmt length (15 day bins, <= 750 days)
  plot_hist_avg_days_plcmt_hs <- ggplot(data = subset(plcmt_data_hs, avg_days_plcmt <= 750), aes(x = avg_days_plcmt)) + 
                                         geom_histogram(binwidth = 15, colour = "black", fill = "dodgerblue4") +
                                         labs(x = "Days Per Placement", y = "Number of Children", 
                                              title = "Average Length of Out-of-Home Care Placement - Overall") + 
                                         plot_attributes

