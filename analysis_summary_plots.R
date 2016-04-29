######################################################################
# notes:
# - purpose: create summary plots of OHC characteristics
# - inputs: formatted analysis set
# - outputs: plots summarizing OHC characteristics
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

  # set up base plot attributes / theme 
  plot_attributes <- theme(plot.background = element_rect(fill = "lightgrey"),
                            panel.grid.major.x = element_line(color = "gray90"), 
                            panel.grid.minor  = element_blank(),
                            panel.background = element_rect(fill = "white", colour = "black") , 
                            panel.grid.major.y = element_line(color = "gray90"),
                            text = element_text(size = 20),
                            plot.title = element_text(vjust = 0, colour = "black", face = "bold", size = 25))

  # output toggle
  p_opt_exp <- 0
  
#############
# load data #
#############

  # load analysis set
  in_outcomes_set <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

########################
# format analysis data #
########################

  # copy input sets
  full_outcomes_set <- copy(in_outcomes_set)

  # sort by academic year
  setorder(full_outcomes_set, acad_year)
  
  # create analysis sample subset
  analysis_sample <- subset(full_outcomes_set, flag_analysis_grd == 1)

  # create set with only placement years
  plcmt_data <- subset(analysis_sample, flag_cur_plcmt == 1)

  # create var for avg. length of placement
  plcmt_data[, plcmt_length_acad := tot_plcmt_days_acad / lf_n_plcmt_acad]
  
################################
# create summary stats to plot #
################################
  
  # freq - plcmt type by acad year
  freq_type_yr <- ea_table(plcmt_data, c("acad_year", "dcf_plcmt_type"))
  
  # remove missing placement types
  freq_type_yr <- subset(freq_type_yr, !is.na(dcf_plcmt_type))
  
####################################
# plot placement info by acad year #
####################################

  # histogram - total placements in acad year
  plot_hist_n_plcmt <- ggplot(data = plcmt_data, aes(x = n_plcmt_acad)) + 
                              geom_histogram(binwidth = 1, colour = "black", fill = "dodgerblue4") +
                              labs(x = "Number of Placements", y = "Number of Children", 
                                   title = "Out-of-Home Care Placements in an Academic Year") + 
                              plot_attributes
                                       
  # histogram - total plcmt days in acad year (20 day bins)
  plot_hist_plcmt_days <- ggplot(data = plcmt_data, aes(x = tot_plcmt_days_acad)) + 
                                 geom_histogram(binwidth = 20, colour = "black", fill = "dodgerblue4") +
                                 labs(x = "Number of Placement Days", y = "Number of Children",
                                      title = "Days in Out-of-Home Care Placement in an Academic Year") + 
                                 plot_attributes

  # histogram - avg plcmt length in acad year (15 day bins)
  plot_hist_plcmt_length <- ggplot(data = plcmt_data, aes(x = plcmt_length_acad)) + 
                                   geom_histogram(binwidth = 10, colour = "black", fill = "dodgerblue4") +
                                   labs(x = "Days Per Placement", y = "Number of Children", 
                                        title = "Average Out-of-Home Care Placement Length \n in an Academic Year") + 
                                   plot_attributes

  # bar plot - total placements by year and placement type
  plot_bar_ptype_by_yr <- ggplot(freq_type_yr, aes(acad_year, count)) + 
                                 geom_bar(stat = "identity", position = "dodge", aes(fill = dcf_plcmt_type)) +
                                 labs(x = "Academic Year", y = "Number of Placements", 
                                     title = "Out-of-Home Care Placements by Type \n in an Academic Year") + 
                                 scale_fill_discrete(name = "Placement Type") +
                                 plot_attributes

#####################
# format and export #
#####################

  # set output directory
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/descriptive/"
  
  # set height and width of plots
  p_height <- 28
  p_width <- 28

  # export
  if (p_opt_exp == 1) { 
    
    ggsave(paste0(p_dir_out, "hist_acad_n_plcmts.png"), plot = plot_hist_n_plcmt,  width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_acad_plcmt_days.png"), plot = plot_hist_plcmt_days,  width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_acad_avg_plcmt_length.png"), plot = plot_hist_plcmt_length, width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "bar_ptype_by_acad_yr.png"), plot = plot_bar_ptype_by_yr, width = p_width, height = p_height, units = "cm")

  }
  