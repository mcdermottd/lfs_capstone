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
  in_child_info <- ea_load("X:/LFS-Education Outcomes/data/lfs_data/analysis_set_child_info.rdata")
    
#########################################
# structure child demo set to summarize #
#########################################
  
  # copy child demo data set
  child_demo_data <- copy(in_child_info)
  
  # create race dummies
  child_demo_data[!is.na(gender_code_cd), d_male := ifelse(gender_code_cd == "M", 1, 0)]
  child_demo_data[!is.na(gender_code_cd), d_female := ifelse(gender_code_cd == "F", 1, 0)]

  # convert elp scale to dummy variable
  child_demo_data[!is.na(elp_code_cd), d_elp := ifelse(elp_code_cd <= 5, 1, 0)]
  
  # convert disability code to dummy variable
  child_demo_data[!is.na(disab_ye), d_sped := ifelse(disab_ye == "N", 0, 1)]
  
  # convert frl code to dummy variable
  child_demo_data[!is.na(frl_ye), d_frl := ifelse(frl_ye == "N", 0, 1)]
  child_demo_data[!is.na(frl_ye), d_fpl := ifelse(frl_ye == "F", 1, 0)]
  child_demo_data[!is.na(frl_ye), d_rpl := ifelse(frl_ye %in% c("A", "R"), 1, 0)]

  # dummy out race variable
  child_demo_data <- db_dummy(child_demo_data, "race_eth_code_cd", opt_data_frequency = 0)
  
  # rename dummied race variables
  setnames(child_demo_data, c("d_race_eth_code_cd_W", "d_race_eth_code_cd_missing", "d_race_eth_code_cd_I", "d_race_eth_code_cd_B", 
                              "d_race_eth_code_cd_H", "d_race_eth_code_cd_A"), c("d_race_white", "d_race_missing", "d_race_indian", "d_race_black",
                                                                                 "d_race_hispanic", "d_race_asian"))
  
  
  # set all race variables to missing, if race_missing == 1
  child_demo_data[d_race_missing == 1, c("d_race_white", "d_race_missing", "d_race_indian", "d_race_black", "d_race_hispanic", "d_race_asian") := NA]
  
  # create non-white dummy var
  child_demo_data[d_race_missing != 1, d_race_nonwhite := ifelse(d_race_white != 1, 1, 0)]

##################################
# calculate overall demographics #
##################################

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
  
#####################################
# produce summary tables by acad yr #
#####################################
  
  # subset to data for melt
  sub_ohc_acad_yr <- subset(sub_ohc_data, acad_year > 2007 & acad_year < 2013, select = c(child_id, acad_year, removal_date, ohc_days_tot, 
                                                                                          num_plcmt_tot, plcmt_days_tot, num_plcmt_acad_yr, 
                                                                                          plcmt_days_acad_year))
  
  # create avg days per placement var
  sub_ohc_acad_yr[, avg_days_per_plcmt := plcmt_days_acad_year / num_plcmt_acad_yr]
  
  # change removal date to removal year
  sub_ohc_acad_yr[, removal_date := as.numeric(year(removal_date))]

  # melt ohc data long to summarize
  long_data_acad <- melt.data.table(sub_ohc_acad_yr, id.vars = c("child_id", "acad_year"))
  
  # subset to only acad year vars
  sub_long_data_acad <- subset(long_data_acad, 
                               variable == "num_plcmt_acad_yr" | variable == "plcmt_days_acad_year" | variable == "avg_days_per_plcmt")
  
  # calc acad yr stats overall
  a_summ_acad <- sub_long_data_acad[!is.na(value), list(n_obs = length(value),
                                                       min = min(value),
                                                       q25 = quantile(value, .25),
                                                       q50 = quantile(value, .5),
                                                       q75 = quantile(value, .75),
                                                       max = max(value),
                                                       mean = round(mean(value), 2),
                                                       var = round(var(value), 2),
                                                       sd = round(sd(value), 2)), 
                                    by = c("variable")]
  
  # sort by variable, removal year
  setorder(long_data_acad, variable, acad_year)

  # calc ohc stats by acad year
  a_summ_acad_by_yr <- long_data_acad[!is.na(value), list(n_obs = length(value),
                                                           min = min(value),
                                                           q25 = quantile(value, .25),
                                                           q50 = quantile(value, .5),
                                                           q75 = quantile(value, .75),
                                                           max = max(value),
                                                           mean = round(mean(value), 2),
                                                           var = round(var(value), 2),
                                                           sd = round(sd(value), 2)), 
                                      by = c("variable", "acad_year")]

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
    
    ea_write(a_summ_overall, "X:/LFS-Education Outcomes/qc/summary_stats_ohc_overall.csv")
    ea_write(a_summ_overall, "X:/LFS-Education Outcomes/qc/summary_stats_ohc_overall_by_yr.csv")
    ea_write(a_summ_acad, "X:/LFS-Education Outcomes/qc/summary_stats_acad.csv")
    ea_write(a_summ_acad_by_yr, "X:/LFS-Education Outcomes/qc/summary_stats_acad_by_yr.csv")

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
  
  
  
  
  
  
  
  
