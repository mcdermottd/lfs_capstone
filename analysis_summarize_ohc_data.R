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
  
  # remove entries with 0 for placement days in acad year #brule
  sub_ohc_data <- subset(sub_ohc_data, as.numeric(plcmt_days_acad_year) != 0)
  
  # create set that has one row per student to calc overall numbers
  setorder(sub_ohc_data, child_id, -acad_year)
  sub_ohc_data_unique <- ea_no_dups(sub_ohc_data, "child_id")
  
##################################
# produce overall summary tables #
##################################
  
  # subset to data for melt
  sub_ohc_data_unique <- subset(sub_ohc_data_unique, select = c(child_id, removal_date, ohc_days_tot, num_plcmt_tot, plcmt_days_tot))

  # melt ohc data long to summarize
  long_data_overall <- melt.data.table(sub_ohc_data_unique, id.vars = c("child_id", "removal_date"))
  
  # set value var to numeric
  long_data_overall[, value := as.numeric(value)]
  
  # create removal year var
  long_data_overall[, removal_year := year(removal_date)]
  
  # sort by variable, removal year
  setorder(long_data_overall, variable, removal_year)
  
  # calc ohc stats overall
  a_overall_stats <- long_data_overall[!is.na(value), list(n_obs = length(value),
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
  a_overall_stats_yr <- long_data_overall[!is.na(value), list(n_obs = length(value),
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
  sub_ohc_acad_yr <- subset(sub_ohc_data, select = c(child_id, acad_year, removal_date, ohc_days_tot, num_plcmt_tot, plcmt_days_tot, 
                                                     num_plcmt_acad_yr, plcmt_days_acad_year))
  
  # change removal date to removal year
  sub_ohc_acad_yr[, removal_date := as.character(year(removal_date))]

  # melt ohc data long to summarize
  long_data_acad <- melt.data.table(sub_ohc_acad_yr, id.vars = c("child_id", "acad_year"))
  
  # set value var to numeric
  long_data_acad[, value := as.numeric(value)]
  
  # sort by variable, removal year
  setorder(long_data_acad, variable, acad_year)
  
  # calc summary stats on long file, with by group
  a_acad_yr_stats <- long_data_acad[!is.na(value), list(n_obs = length(value),
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
  
  # histogram - total ohc days
  plot_hist_ohc_days <- ggplot(data = subset(ohc_dpi_data_no_dups, ohc_days_tot <= 1825), aes(x = ohc_days_tot)) + 
                               geom_histogram(binwidth = 20, colour = "black", fill = "darkred") +
                               labs(x = "Total Number of Days in OHC", y = "Number of Students", 
                                    title = "Total Number of Days in Out of Home Care by Student") + 
                               plot_attributes
  
  # histogram - total ohc days, by academic year
  plot_hist_ohc_days_byr <- ggplot(data = subset(ohc_dpi_data_no_dups, ohc_days_tot <= 1825), aes(x = ohc_days_tot)) + 
                               geom_histogram(binwidth = 20, colour = "black", fill = "darkred") +
                               labs(x = "Total Number of Days in OHC", y = "Number of Students", 
                                    title = "Total Number of Days in Out of Home Care by Student, by Latest Academic Year") + 
                               plot_attributes +
                               facet_wrap(~latest_acad_yr, ncol = 2)
                               
  # histogram - total plcmt days in acad year
  plot_hist_plcmt_days_acad <- ggplot(data = ohc_dpi_data_no_dups, aes(x = plcmt_days_acad_year)) + 
                               geom_histogram(binwidth = 20, colour = "black", fill = "darkred") +
                               labs(x = "Total Number of Out of Home Placement Days in Academic Year", y = "Number of Students", 
                                    title = "Total Number of Out of Home Placement Days in Academic Year by Student") + 
                               plot_attributes
  
  # histogram - total plcmt days in acad year, by academic year
  plot_hist_plcmt_days_acad_byr <- ggplot(data = ohc_dpi_data_no_dups, aes(x = plcmt_days_acad_year)) + 
                               geom_histogram(binwidth = 20, colour = "black", fill = "darkred") +
                               labs(x = "Total Number of Out of Home Placement Days in Academic Year", y = "Number of Students", 
                                    title = "Total Number of Out of Home Placement Days in Academic Year by Student, by Latest Academic Year") + 
                               plot_attributes +
                               facet_wrap(~latest_acad_yr, ncol = 2)

  
  
  
  plot_freq_state20_comp_dist <- ggplot(data = wide_analysis_set[!is.na(state20_comp)], aes(x = state20_comp)) + 
                                        geom_histogram(binwidth = 2, colour = "black", fill = "#2D947C") +
                                        labs(x = "Growth Rating (0-20)", y = "Number of Teachers", 
                                             title = paste0("Growth on Comparable Measures by District: ", sy_label)) + 
                                        plot_attributes +
                                        facet_wrap(~district_code, ncol = 4)
##########
# export #
##########

  # set height and width of plots
  p_height <- 28
  p_width <- 28

  
  # export
  if (p_opt_exp == 1) { 
    
    ea_write( , ".csv")
    
    ggsave("X:/LFS-Education Outcomes/qc/hist_ohc_days_overall.png", plot = plot_hist_ohc_days, width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_ohc_days_by_acad_yr.png", plot = plot_hist_ohc_days_byr, 
           width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_plcmt_days_acad_yr_overall.png", plot = plot_hist_plcmt_days_acad, 
           width = p_width, height = p_height, units = "cm")
    ggsave("X:/LFS-Education Outcomes/qc/hist_plcmt_days_acad_yr_by_acad_yr.png", plot = plot_hist_plcmt_days_acad_byr, 
           width = p_width, height = p_height, units = "cm")
    

  }
  
  
  
  
  
  
  
  