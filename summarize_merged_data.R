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

#############
# set parms #
#############

  # output toggle
  p_opt_exp <- 0

#############
# load data #
#############

  in_stacked_ohc <- fread("X:/LFS-Education Outcomes/data/lfs_data/stacked_acad_yr_set.csv", colClasses = "character")

############################
# format data to summarize #
############################
  
  # copy raw data
  sub_ohc_data <- copy(in_stacked_ohc)
  
  # set placement vars to numeric
  sub_ohc_data[, 11:15] <- lapply(sub_ohc_data[, 11:15, with = FALSE], as.numeric)

  # remove entries with 0 for placement days in acad year #brule
  sub_ohc_data <- subset(sub_ohc_data, plcmt_days_acad_year != 0)
  
  # set placement days longer than 365 to 365 #brule
  sub_ohc_data[plcmt_days_acad_year > 365, plcmt_days_acad_year := 365]

  # create set that has one row per student to calc overall numbers
  setorder(sub_ohc_data, child_id, -acad_year)
  sub_ohc_data_unique <- ea_no_dups(sub_ohc_data, "child_id")
  
##################################
# produce overall summary tables #
##################################
  
  # subset to data for melt
  sub_ohc_data_unique <- subset(sub_ohc_data_unique, select = c(child_id, removal_date, ohc_days_tot, num_plcmt_tot, plcmt_days_tot))

  # create avg days per placement var
  sub_ohc_data_unique[, avg_days_per_plcmt := plcmt_days_tot / num_plcmt_tot]
  
  # melt ohc data long to summarize
  long_data_overall <- melt.data.table(sub_ohc_data_unique, id.vars = c("child_id", "removal_date"))
  
  # create removal year var
  long_data_overall[, removal_year := year(removal_date)]
  
  # sort by variable, removal year
  setorder(long_data_overall, variable, removal_year)
  
  # calc ohc stats overall
  a_summ_overall <- long_data_overall[!is.na(value), list(n_obs = length(value),
                                                           min = min(value),
                                                           q25 = quantile(value, .25),
                                                           q50 = quantile(value, .5),
                                                           q75 = quantile(value, .75),
                                                           max = max(value),
                                                           mean = round(mean(value), 2),
                                                           var = round(var(value), 2),
                                                           sd = round(sd(value), 2)), 
                                      by = c("variable")]
  
  # calc ohc stats overall, by removal year
  a_summ_overall_by_yr <- long_data_overall[!is.na(value), list(n_obs = length(value),
                                                                min = min(value),
                                                                q25 = quantile(value, .25),
                                                                q50 = quantile(value, .5),
                                                                q75 = quantile(value, .75),
                                                                max = max(value),
                                                                mean = round(mean(value), 2),
                                                                var = round(var(value), 2),
                                                                sd = round(sd(value), 2)),
                                            by = c("variable", "removal_year")]

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
  
  
  
  
  
  
  
  
