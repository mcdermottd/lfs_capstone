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
  library(ggplot2)
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

  # load child info
  in_child_info <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set_child_info.rdata")
  
  # load acad year info
  in_acad_year_data <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set_full.rdata")

#########################
# format acad year data #
#########################
  
  # copy acad year info
  acad_yr_data <- copy(in_acad_year_data)
  
  # set missing ohc flags to 1
  acad_yr_data[is.na(flag_ohc), flag_ohc := 1]
  
  # create additional flag when acad outcomes are missing
  acad_yr_data[, flag_dpi_yr := ifelse(!is.na(lds_student_key), 1, 0)]
  
  # reorder vars
  ea_colorder(acad_yr_data, c("lf_child_id", "lds_student_key", "child_id", "flag_ohc", "flag_ohc_yr", "flag_dpi_yr"))
  
###########################
# standardize test scores #
###########################
  
  # subset to necessary vars for melt
  score_data_to_melt <- subset(acad_yr_data, flag_dpi_yr == 1, select = c(lf_child_id, flag_ohc, acad_year, grade_level_cd, math_kce_scale_score,
                                                                          rdg_kce_scale_score))
  
  # melt test score data long to summarize
  score_data_long <- melt.data.table(score_data_to_melt, id.vars = c("lf_child_id", "flag_ohc", "acad_year", "grade_level_cd"))
  
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
  
  # merge back with main set
  acad_yr_data <- ea_merge(acad_yr_data, kce_standardize, c("grade_level_cd", "acad_year"))
  
  # create z-scored scores
  acad_yr_data[, zscore_math_kce := (math_kce_scale_score - mean_math_kce_scale_score) / sd_math_kce_scale_score]
  acad_yr_data[, zscore_rdg_kce := (rdg_kce_scale_score - mean_rdg_kce_scale_score) / sd_rdg_kce_scale_score]

##################################
# calculate overall demographics #
##################################
  
  # copy child demo data set
  child_demo_data <- copy(in_child_info)
  
  # calc overall demo summary
  a_demo_overall <- child_demo_data[, list(n_obs = .N,
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

  # calc overall demo, ohc and not
  a_demo_compare <- child_demo_data[, list(n_obs = .N,
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
  
  # calc overall demo, by ohc and frl statues
  a_demo_compare_frl <- child_demo_data[!is.na(d_frl), list(n_obs = .N,
                                                             per_male = round(mean(d_male, na.rm = TRUE), 3),
                                                             per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                                             per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                                             per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                                             per_black = round(mean(d_race_black, na.rm = TRUE), 3),
                                                             per_hispanic = round(mean(d_race_hispanic, na.rm = TRUE), 3),
                                                             per_asian = round(mean(d_race_asian, na.rm = TRUE), 3),
                                                             per_indian = round(mean(d_race_indian, na.rm = TRUE), 3)),
                                        by = c("flag_ohc", "d_frl")]
  
######################
# summarize ohc data #
######################
  
  # subset to data for melt
  sub_ohc_data <- subset(child_demo_data, flag_ohc == 1, select = c(lf_child_id, n_ohc_tot, tot_ohc_days, n_plcmt_tot, tot_plcmt_days, d_male,
                                                                    d_female, d_elp, d_sped, d_frl, d_fpl, d_rpl, d_race_white, d_race_indian, 
                                                                    d_race_black, d_race_hispanic, d_race_asian))
  
  # create avg days per placement var
  sub_ohc_data[, avg_days_per_plcmt := tot_plcmt_days / n_plcmt_tot]
  
  # melt ohc data long to summarize
  ohc_data_long <- melt.data.table(sub_ohc_data, id.vars = c("lf_child_id", "d_male", "d_female", "d_elp", "d_sped", "d_frl", "d_fpl", "d_rpl",
                                                             "d_race_white", "d_race_indian", "d_race_black", "d_race_hispanic", "d_race_asian"))

  # calc ohc stats overall
  a_ohc_overall <- ohc_data_long[!is.na(value), list(n_obs = length(value),
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
  a_ohc_by_gender <- ohc_data_long[!is.na(value) & !is.na(d_male), list(n_obs = length(value),
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
  a_ohc_by_race <- ohc_data_long[!is.na(value) & !is.na(d_male), list(n_obs = length(value),
                                                                      min = min(value),
                                                                      q25 = quantile(value, .25),
                                                                      q50 = quantile(value, .5),
                                                                      q75 = quantile(value, .75),
                                                                      max = max(value),
                                                                      mean = round(mean(value), 2),
                                                                      var = round(var(value), 2),
                                                                      sd = round(sd(value), 2)),
                                   by = c("variable", "d_race_white")]

##########################################
# produce demo summary tables by acad yr #
##########################################
  
  # change necessary vars to numeric
  acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad")] <- 
    lapply(acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad"), with = FALSE], as.numeric)

  # create avg days per placement vars
  acad_yr_data[, avg_days_per_plcmt := tot_plcmt_days / n_plcmt_tot]
  acad_yr_data[, avg_days_plcmt_acad := tot_plcmt_days_acad / n_plcmt_acad]
  
  # sort by academic year
  setorder(acad_yr_data, acad_year)
  
  # calc stats of ohc students per year
  a_ohc_by_yr <- acad_yr_data[flag_ohc == 1, list(n_obs = .N,
                                                   avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                                   per_male = round(mean(d_male, na.rm = TRUE), 3),
                                                   per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                                   per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                                   per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                                   per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                                   per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                                   avg_plcmt = round(mean(n_plcmt_tot), 3),
                                                   avg_plcmt_days = round(mean(tot_plcmt_days), 3),
                                                   avg_plcmt_length = round(mean(avg_days_per_plcmt), 3)), 
                              by = acad_year]
  
  # calc stats of ohc placements by year
  a_plcmt_by_yr <- acad_yr_data[flag_ohc_yr == 1, list(n_obs = .N,
                                                       avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                                       per_male = round(mean(d_male, na.rm = TRUE), 3),
                                                       per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                                       per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                                       per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                                       per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                                       per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                                       avg_plcmt = round(mean(n_plcmt_acad), 3),
                                                       avg_plcmt_days = round(mean(tot_plcmt_days_acad), 3),
                                                       avg_plcmt_length = round(mean(avg_days_plcmt_acad), 3)), 
                                by = acad_year]
  
  
#######################################
# produce acad outcome summary tables #
####################################### 
  
  # calc acad outcomes, ohc and not
  a_acad_outcomes_compare <- acad_yr_data[flag_dpi_yr == 1, list(n_obs = .N,
                                                                 avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                                                 avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                                                 avg_incidents = round(mean(incidents_os, na.rm = TRUE), 3),
                                                                 avg_math_kce = round(mean(math_kce_scale_score, na.rm = TRUE), 3),
                                                                 avg_rdg_kce = round(mean(rdg_kce_scale_score, na.rm = TRUE), 3)),
                                          by = flag_ohc]
                                           
                                           
                                           
                                           per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                           per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                           per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                           per_black = round(mean(d_race_black, na.rm = TRUE), 3),
                                           per_hispanic = round(mean(d_race_hispanic, na.rm = TRUE), 3),
                                           per_asian = round(mean(d_race_asian, na.rm = TRUE), 3),
                                           per_indian = round(mean(d_race_indian, na.rm = TRUE), 3)),
                                    by = flag_ohc]


#####################
# plot summary info #
#####################
  
  # set up base plot attributes / theme 
  plot_attributes <- theme( plot.background = element_rect(fill = "lightgrey"),
                            panel.grid.major.x = element_line(color = "gray90"), 
                            panel.grid.minor  = element_blank(),
                            panel.background = element_rect(fill = "white", colour = "black") , 
                            panel.grid.major.y = element_line(color = "gray90"),
                            text = element_text(size = 20),
                            plot.title = element_text(vjust = 0, colour = "black", face = "bold", size = 25))
  
  # histogram - total ohc days (30 day bins, <= 1825 days)
  plot_hist_ohc_days <- ggplot(data = subset(sub_ohc_data_unique, ohc_days_tot <= 1825), aes(x = ohc_days_tot)) + 
                               geom_histogram(binwidth = 30, colour = "black", fill = "dodgerblue4") +
                               labs(x = "Total Number of Days in OHC", y = "Number of Children", 
                                    title = "Total Number of Days in Out of Home Care") + 
                               plot_attributes
  
  # histogram - total ohc placements (1 day bins, <= 20 placements)
  plot_hist_ohc_plcmts <- ggplot(data = subset(sub_ohc_data_unique, num_plcmt_tot <= 20), aes(x = num_plcmt_tot)) + 
                                 geom_histogram(binwidth = 1, colour = "black", fill = "dodgerblue4") +
                                 labs(x = "Total Number of Placements", y = "Number of Children", 
                                      title = "Total Placements in Out of Home Care") + 
                                 plot_attributes
  
  # histogram - avg plcmt length (15 day bins, <= 750 days)
  plot_hist_avg_days_plcmt <- ggplot(data = subset(sub_ohc_data_unique, avg_days_per_plcmt <= 750), aes(x = avg_days_per_plcmt)) + 
                                     geom_histogram(binwidth = 15, colour = "black", fill = "dodgerblue4") +
                                     labs(x = "Average Days Per Placement", y = "Number of Children", 
                                          title = "Average Days Per Out of Home Care Placement") + 
                                     plot_attributes
                               
  # histogram - total plcmt days in acad year (20 day bins)
  plot_hist_plcmt_days_acad <- ggplot(data = subset(sub_ohc_acad_yr, acad_year > 2007 & acad_year < 2013), aes(x = plcmt_days_acad_year)) + 
                               geom_histogram(binwidth = 20, colour = "black", fill = "dodgerblue4") +
                               labs(x = "Number of Out of Home Placement Days Per Year", y = "Number of Children", 
                                    title = "Number of Out of Home Care Placements Per Year") + 
                               plot_attributes
  
  # histogram - total plcmt days in acad year, by academic year (20 day bins)
  plot_hist_plcmt_days_acad_byr <- ggplot(data = subset(sub_ohc_acad_yr, acad_year > 2007 & acad_year < 2013), aes(x = plcmt_days_acad_year)) + 
                               geom_histogram(binwidth = 20, colour = "black", fill = "dodgerblue4") +
                               labs(x = "Number of Out of Home Placement Days Per Year", y = "Number of Children", 
                                    title = "Number of Out of Home Care Placements Per Year \n - by Academic Year") + 
                               plot_attributes +
                               facet_wrap(~acad_year, ncol = 2)

  # histogram - avg plcmt length in acad year (15 day bins, <= 750 days)
  plot_hist_avg_days_acad <- ggplot(data = subset(sub_ohc_acad_yr, acad_year > 2007 & acad_year < 2013), aes(x = avg_days_per_plcmt)) + 
                                     geom_histogram(binwidth = 10, colour = "black", fill = "dodgerblue4") +
                                     labs(x = "Average Days Per Placement", y = "Number of Children", 
                                          title = "Average Days Per Out of Home Care Placement Per Year") + 
                                     plot_attributes
  
  # histogram - avg plcmt length in acad year, by academic year (15 day bins, <= 750 days)
  plot_hist_avg_days_acad_byr <- ggplot(data = subset(sub_ohc_acad_yr, acad_year > 2007 & acad_year < 2013), aes(x = avg_days_per_plcmt)) + 
                                         geom_histogram(binwidth = 10, colour = "black", fill = "dodgerblue4") +
                                         labs(x = "Average Days Per Placement", y = "Number of Children", 
                                              title = "Average Days Per Out of Home Care Placement Per Year \n - by Academic Year") + 
                                         plot_attributes +
                                         facet_wrap(~acad_year, ncol = 2)
  
#####################
# format and export #
#####################
  
  # sort output set
  setorder(a_summ_acad_by_yr, acad_year, variable)
  
  # set height and width of plots
  p_height <- 28
  p_width <- 28

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(a_demo_overall, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/overall_demo.csv")
    ea_write(a_demo_compare, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/overall_demo_compare.csv")
    ea_write(a_demo_compare_frl, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/overall_demo_compare_frl.csv")

    ea_write(a_ohc_overall, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/ohc_stats_overall.csv")
    ea_write(a_ohc_by_yr, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/ohc_stats_overall_by_yr.csv")
    ea_write(a_plcmt_by_yr, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/acad_plcmt_stats_by_yr.csv")

    

    ggsave("X:/LFS-Education Outcomes/qc/hist_ohc_days_overall.png", plot = plot_hist_ohc_days, width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_plcmts_overall.png", plot = plot_hist_ohc_plcmts, width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_avg_plcmt_days_overall.png", plot = plot_hist_avg_days_plcmt, 
           width = p_width, height = p_height, units = "cm")
    
    ggsave("X:/LFS-Education Outcomes/qc/hist_plcmt_days_acad.png", plot = plot_hist_plcmt_days_acad, 
           width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_plcmt_days_acad_by_yr.png", plot = plot_hist_plcmt_days_acad_byr, 
           width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_avg_plcmt_days_acad.png", plot = plot_hist_avg_days_acad, 
           width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_avg_plcmt_days_acad_by_yr.png", plot = plot_hist_avg_days_acad_byr, 
           width = p_width, height = p_height, units = "cm")

  }
  
  
  
  
  
  
  
  
