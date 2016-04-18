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

  # load child info
  in_child_info <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set_child_info.rdata")
  
  # load acad year info
  in_acad_year_data <- ea_load("X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

###############
# format data #
###############

  # copy input sets
  child_demo_data <- copy(in_child_info)
  acad_yr_data <- copy(in_acad_year_data)

  # change necessary vars to numeric
  acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad")] <- 
    lapply(acad_yr_data[, c("n_plcmt_tot", "tot_plcmt_days", "tot_plcmt_days_acad", "n_plcmt_acad"), with = FALSE], as.numeric)
  
  # sort by academic year
  setorder(acad_yr_data, acad_year)
  
  # create avg days per placement vars
  child_demo_data[, avg_days_ohc := tot_ohc_days / n_ohc_tot]
  child_demo_data[, avg_days_plcmt := tot_plcmt_days / n_plcmt_tot]
  acad_yr_data[, avg_days_plcmt_acad := tot_plcmt_days_acad / n_plcmt_acad]

###################################
# create flags for sample subsets #
###################################
  
  # create frl / non-frl flags for comparison groups
  child_demo_data[, compare_frl := ifelse(flag_ohc == 0 & d_frl == 1, 1, 0)]
  acad_yr_data[, compare_frl := ifelse(flag_ohc == 0 & d_frl == 1, 1, 0)]

  # create flag if hs student #brule
  acad_yr_data[, flag_hs := ifelse(grade_level_cd %in% c("08", "09", "10", "11", "12"), 1, 0)]
  
  # create hs subset
  acad_yr_data_hs <- subset(acad_yr_data, flag_hs == 1)
  
  # subset to ids of HS school students (8th - 12th grades)
  hs_student_ids <- subset(acad_yr_data_hs, select = c(lf_child_id, flag_hs))
  
  # remove dups based on id
  hs_student_ids <- ea_no_dups(hs_student_ids, "lf_child_id")
  
  # merge with child demo info
  child_demo_data <- ea_merge(child_demo_data, hs_student_ids, "lf_child_id", opt_print = 0)
  
  # fill in missing flags
  child_demo_data[is.na(flag_hs), flag_hs := 0]

  # hs subset of child demo set  
  child_demo_data_hs <- subset(child_demo_data, flag_hs == 1)

################################
# examine overall demographics #
################################

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

  # calc overall demo, ohc status
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
                                    by = c("flag_ohc")]
  
  
  # calc overall demo, ohc status and frl 
  a_demo_compare_frl <- child_demo_data[, list(n_obs = .N,
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
                                    by = c("flag_ohc", "compare_frl")]
  
  # calc overall demo, hs subset, ohc status
  a_demo_compare_hs <- child_demo_data_hs[, list(n_obs = .N,
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
  a_demo_compare_hs_frl <- child_demo_data_hs[, list(n_obs = .N,
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
                                          by = c("flag_ohc", "compare_frl")]

############################
# examine overall ohc info #
############################
  
  # subset to data for melt
  sub_ohc_data <- subset(child_demo_data, flag_ohc == 1, select = c(lf_child_id, flag_hs, n_ohc_tot, tot_ohc_days, avg_days_ohc, n_plcmt_tot, 
                                                                    tot_plcmt_days, avg_days_plcmt, d_male, d_female, d_elp, d_sped, d_frl, d_fpl, 
                                                                    d_rpl, d_race_white, d_race_indian, d_race_black, d_race_hispanic, d_race_asian))
  
  # melt ohc data long to summarize
  ohc_data_long <- melt.data.table(sub_ohc_data, id.vars = c("lf_child_id", "flag_hs", "d_male", "d_female", "d_elp", "d_sped", "d_frl", "d_fpl",
                                                             "d_rpl", "d_race_white", "d_race_indian", "d_race_black", "d_race_hispanic", 
                                                             "d_race_asian"))

  # remove NA values
  ohc_data_long <- subset(ohc_data_long, !is.na(value))
  
  # create hs subset
  ohc_data_long_hs <- subset(ohc_data_long, flag_hs == 1)
  
  # calc ohc stats overall
  a_ohc_overall <- ohc_data_long[, list(n_obs = length(value),
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
  a_ohc_overall_hs <- ohc_data_long_hs[, list(n_obs = length(value),
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
  a_ohc_by_gender_hs <- ohc_data_long_hs[!is.na(d_male), list(n_obs = length(value),
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
  a_ohc_by_race_hs <- ohc_data_long_hs[!is.na(d_male), list(n_obs = length(value),
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
  
  # create set with only placement years
  plcmt_yr_data_hs <- subset(acad_yr_data_hs, flag_ohc_yr == 1)

  # calc stats of ohc placements by year
  a_plcmt_by_yr_hs <- plcmt_yr_data_hs[, list(n_obs = .N,
                                               avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                               per_male = round(mean(d_male, na.rm = TRUE), 3),
                                               per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                               per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                               per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                               per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                               per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                               avg_plcmts = round(mean(n_plcmt_acad), 3),
                                               avg_plcmt_days = round(mean(tot_plcmt_days_acad), 3),
                                               avg_plcmt_length = round(mean(avg_days_plcmt_acad), 3)),
                                       by = acad_year]

#######################################
# examine placement type and location #
#######################################

  # calc stats of ohc placements by plcmt type
  a_plcmt_by_type_hs <- plcmt_yr_data_hs[, list(n_obs = .N,
                                                 avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                                 per_male = round(mean(d_male, na.rm = TRUE), 3),
                                                 per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                                 per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                                 per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                                 per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                                 per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                                 avg_plcmts = round(mean(n_plcmt_acad), 3),
                                                 avg_plcmt_days = round(mean(tot_plcmt_days_acad), 3),
                                                 avg_plcmt_length = round(mean(avg_days_plcmt_acad), 3)),
                                         by = dcf_plcmt_type]
  
  # calc stats of ohc placements by region
  a_plcmt_by_region_hs <- plcmt_yr_data_hs[, list(n_obs = .N,
                                                   avg_age = round(mean(age_in_years_cd, na.rm = TRUE), 3),
                                                   per_male = round(mean(d_male, na.rm = TRUE), 3),
                                                   per_elp = round(mean(d_elp, na.rm = TRUE), 3),
                                                   per_sped = round(mean(d_sped, na.rm = TRUE), 3),
                                                   per_fpl = round(mean(d_fpl, na.rm = TRUE), 3),
                                                   per_rpl = round(mean(d_rpl, na.rm = TRUE), 3),
                                                   per_white = round(mean(d_race_white, na.rm = TRUE), 3),
                                                   avg_plcmts = round(mean(n_plcmt_acad), 3),
                                                   avg_plcmt_days = round(mean(tot_plcmt_days_acad), 3),
                                                   avg_plcmt_length = round(mean(avg_days_plcmt_acad), 3)),
                                           by = region]
  
  # freq of placement type by year
  a_plcmt_type_yr_hs <- ea_table(plcmt_yr_data_hs, c("dcf_plcmt_type", "acad_year"))

  # freq of placement region by year
  a_plcmt_region_yr_hs <- ea_table(plcmt_yr_data_hs, c("region", "acad_year"))

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
  p_dir_out <- "X:/LFS-Education Outcomes/qc/second_draft_exhibits/descriptive/"
  
  # set height and width of plots
  p_height <- 28
  p_width <- 28

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(hs_student_ids, "X:/LFS-Education Outcomes/qc/hs_student_ids.csv")

    ea_write(a_demo_overall, paste0(p_dir_out, "demo_overall.csv"))
    ea_write(a_demo_compare, paste0(p_dir_out, "demo_by_ohc.csv"))
    ea_write(a_demo_compare_frl, paste0(p_dir_out, "demo_by_ohc_frl.csv"))
    ea_write(a_demo_compare_hs, paste0(p_dir_out, "demo_by_ohc_hs.csv"))
    ea_write(a_demo_compare_hs_frl, paste0(p_dir_out, "demo_by_ohc_hs_frl.csv"))

    ea_write(a_ohc_overall, paste0(p_dir_out, "ohc_overall.csv"))
    ea_write(a_ohc_overall_hs, paste0(p_dir_out, "ohc_overall_hs.csv"))
    ea_write(a_ohc_by_gender_hs, paste0(p_dir_out, "ohc_by_gender_hs.csv"))
    ea_write(a_ohc_by_race_hs, paste0(p_dir_out, "ohc_by_race_hs.csv"))

    ea_write(a_plcmt_by_yr_hs, paste0(p_dir_out, "plcmts_by_yr_hs.csv"))
    ea_write(a_plcmt_by_type_hs, paste0(p_dir_out, "plcmts_by_type_hs.csv"))
    ea_write(a_plcmt_by_region_hs, paste0(p_dir_out, "plcmts_by_region_hs.csv"))
    ea_write(a_plcmt_type_yr_hs, paste0(p_dir_out, "freq_plcmt_type_by_yr_hs.csv"))
    ea_write(a_plcmt_region_yr_hs, paste0(p_dir_out, "freq_plcmt_region_by_yr_hs.csv"))
    
    ggsave(paste0(p_dir_out, "hist_ohc_days_overall.png"), plot = plot_hist_ohc_days_hs, width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_plcmts_overall.png"), plot = plot_hist_ohc_plcmts_hs, width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_avg_plcmt_days_overall.png"), plot = plot_hist_avg_days_plcmt_hs, width = p_width, height = p_height, units = "cm")

    ggsave(paste0(p_dir_out, "hist_plcmts_acad.png"), plot = plot_hist_plcmt_acad_hs,  width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "bar_plcmts_acad_by_yr_type.png"), plot = plot_bar_plcmts_by_yr_type_hs, width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_plcmt_days_acad.png"), plot = plot_hist_plcmt_days_acad_hs,  width = p_width, height = p_height, units = "cm")
    ggsave(paste0(p_dir_out, "hist_avg_plcmt_days_acad.png"), plot = plot_hist_avg_days_acad_hs, width = p_width, height = p_height, units = "cm")

  }
  
  
  
