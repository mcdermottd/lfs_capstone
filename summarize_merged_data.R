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
  in_acad_year_data <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

################################
# examine overall demographics #
################################
  
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
  
############################
# examine overall ohc info #
############################
  
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

####################################################
# examine demographics and placements by acad year #
####################################################
  
  # copy acad year info
  acad_yr_data <- copy(in_acad_year_data)
  
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

  # calc stats of ohc placements by plcmt type
  a_plcmt_by_type <- acad_yr_data[flag_ohc_yr == 1, list(n_obs = .N,
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
                                     by = dcf_plcmt_type]
  
  # calc stats of ohc placements by region
  a_plcmt_by_region <- acad_yr_data[flag_ohc_yr == 1, list(n_obs = .N,
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
                                    by = region]
  
#######################################
# examine placement type and location #
#######################################
  
  # freq of placement type 
  a_plcmt_type <- ea_table(subset(acad_yr_data, flag_ohc_yr == 1), c("dcf_plcmt_type"), opt_percent = 1)
  
  # freq of placement type by year
  a_plcmt_type_yr <- ea_table(subset(acad_yr_data, flag_ohc_yr == 1), c("dcf_plcmt_type", "acad_year"))

  # freq of placement region
  a_plcmt_region <- ea_table(subset(acad_yr_data, flag_ohc_yr == 1), c("region"), opt_percent = 1)

  # freq of placement region by year
  a_plcmt_region_yr <- ea_table(subset(acad_yr_data, flag_ohc_yr == 1), c("region", "acad_year"))
  
#######################################
# produce acad outcome summary tables #
####################################### 
  
  # calc acad outcomes, by ohc status
  a_acad_outcomes <- acad_yr_data[flag_dpi_yr == 1, list(n_obs = .N,
                                                         avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                                         avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                                         avg_incidents = round(mean(incidents_os, na.rm = TRUE), 3),
                                                         avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                                         sd_math_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3),
                                                         avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                                         sd_rdg_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3)),
                                  by = flag_ohc]
  
  # sort by grade
  setorder(acad_yr_data, grade_level_cd, flag_ohc)
  
  # calc acad outcomes, by ohc status, grade
  a_acad_outcomes_by_grd <- acad_yr_data[flag_dpi_yr == 1, list(n_obs = .N,
                                                               avg_atten = round(mean(att_rate_wi, na.rm = TRUE), 3),
                                                               avg_days_remove = round(mean(days_removed_os, na.rm = TRUE), 3),
                                                               avg_incidents = round(mean(incidents_os, na.rm = TRUE), 3),
                                                               avg_math_kce = round(mean(zscore_math_kce, na.rm = TRUE), 3),
                                                               sd_math_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3),
                                                               avg_rdg_kce = round(mean(zscore_rdg_kce, na.rm = TRUE), 3),
                                                               sd_rdg_kce = round(sd(zscore_math_kce, na.rm = TRUE), 3)),
                                          by = c("flag_ohc", "grade_level_cd")]
                                           



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
    
    ea_write(a_demo_overall, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/demo_overall.csv")
    ea_write(a_demo_compare, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/demo_by_ohc.csv")
    ea_write(a_demo_compare_frl, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/demo_by_ohc_frl.csv")

    ea_write(a_ohc_overall, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/ohc_overall.csv")
    ea_write(a_ohc_by_gender, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/ohc_by_gender.csv")
    ea_write(a_ohc_by_race, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/ohc_by_race.csv")

    ea_write(a_ohc_by_yr, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/ohc_overall_by_yr.csv")
    ea_write(a_plcmt_by_yr, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/plcmts_by_yr.csv")
    ea_write(a_plcmt_by_type, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/plcmts_by_type.csv")
    ea_write(a_plcmt_by_region, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/plcmts_by_region.csv")

    ea_write(a_plcmt_type, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/freq_plcmt_type_overall.csv")
    ea_write(a_plcmt_type_yr, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/freq_plcmt_type_by_yr.csv")
    ea_write(a_plcmt_region, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/freq_plcmt_region_overall.csv")
    ea_write(a_plcmt_region_yr, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/freq_plcmt_region_by_yr.csv")
        
    ea_write(a_acad_outcomes, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/acad_outcomes_overall.csv")
    ea_write(a_acad_outcomes_by_grd, "X:/LFS-Education Outcomes/qc/second_draft_exhibits/acad_outcomes_by_grd.csv")


    

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
  
  
  
  
  
  
  
  
