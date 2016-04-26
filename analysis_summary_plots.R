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

  # load analysis set
  in_analysis_set <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

###############
# format data #
###############


#########################
# plot ohc info overall #
#########################
  
  # set up base plot attributes / theme 
  plot_attributes <- theme( plot.background = element_rect(fill = "lightgrey"),
                            panel.grid.major.x = element_line(color = "gray90"), 
                            panel.grid.minor  = element_blank(),
                            panel.background = element_rect(fill = "white", colour = "black") , 
                            panel.grid.major.y = element_line(color = "gray90"),
                            text = element_text(size = 20),
                            plot.title = element_text(vjust = 0, colour = "black", face = "bold", size = 25))

  # create set with only placement years
  plcmt_data_hs <- subset(child_demo_data_hs, flag_ohc == 1)
  
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

####################################
# plot placement info by acad year #
####################################

  # histogram - total placements in acad year (<= 15 placements)
  plot_hist_plcmt_acad_hs <- ggplot(data = subset(plcmt_yr_data_hs, n_plcmt_tot <= 15), aes(x = n_plcmt_acad)) + 
                                     geom_histogram(binwidth = 1, colour = "black", fill = "dodgerblue4") +
                                     labs(x = "Number of Placements", y = "Number of Children", 
                                          title = "Placements in Out-of-Home Care Per Academic Year") + 
                                     plot_attributes
  
  # bar plot - total placements by year and placement type
  plot_bar_plcmts_by_yr_type_hs <- ggplot(subset(a_plcmt_type_yr_hs, !is.na(dcf_plcmt_type)), aes(acad_year, count)) + 
                                          geom_bar(stat = "identity", position = "dodge", aes(fill = dcf_plcmt_type)) +
                                       labs(x = "Academic Year", y = "Number of Placements", 
                                            title = "Out-of-Home Care Placements by Type \n Per Academic Year") + 
                                       scale_fill_discrete(name = "Placement Type") +
                                       plot_attributes
                                       
  # histogram - total plcmt days in acad year (20 day bins)
  plot_hist_plcmt_days_acad_hs <- ggplot(data = plcmt_yr_data_hs, aes(x = tot_plcmt_days_acad)) + 
                                       geom_histogram(binwidth = 20, colour = "black", fill = "dodgerblue4") +
                                       labs(x = "Number of Placement Days", y = "Number of Children",
                                            title = "Out-of-Home Care Placement Days Per Academic Year") + 
                                       plot_attributes

  # histogram - avg plcmt length in acad year (15 day bins)
  plot_hist_avg_days_acad_hs <- ggplot(data = plcmt_yr_data_hs, aes(x = avg_days_plcmt_acad)) + 
                                     geom_histogram(binwidth = 10, colour = "black", fill = "dodgerblue4") +
                                     labs(x = "Days Per Placement", y = "Number of Children", 
                                          title = "Average Days Per Out-of-Home Care Placement Per Academic Year") + 
                                     plot_attributes

#####################
# format and export #
#####################

  # set output director
  p_dir_out <- "X:/LFS-Education Outcomes/qc/final_draft_exhibits/descriptive/"
  
  # set height and width of plots
  p_height <- 28
  p_width <- 28

  # export
  if (p_opt_exp == 1) { 
    
    ggsave(paste0(p_dir_out, "hist_ohc_days_overall.png"), plot = plot_hist_ohc_days_hs, width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_plcmts_overall.png"), plot = plot_hist_ohc_plcmts_hs, width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_avg_plcmt_days_overall.png"), plot = plot_hist_avg_days_plcmt_hs, width = p_width, height = p_height, units = "cm")

    ggsave(paste0(p_dir_out, "hist_plcmts_acad.png"), plot = plot_hist_plcmt_acad_hs,  width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "bar_plcmts_acad_by_yr_type.png"), plot = plot_bar_plcmts_by_yr_type_hs, width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_plcmt_days_acad.png"), plot = plot_hist_plcmt_days_acad_hs,  width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_avg_plcmt_days_acad.png"), plot = plot_hist_avg_days_acad_hs, width = p_width, height = p_height, units = "cm")

  }
  