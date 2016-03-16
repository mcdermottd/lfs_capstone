######################################################################
# notes:
# - purpose:
# - inputs:
# - outputs:
# - keywords:
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

  in_merged_data <- fread("X:/LFS-Education Outcomes/data/lfs_data/combined_dpi_dcf_set.csv")

########################
# create analysis sets #
########################
  
  # copy raw data
  merged_data <- copy(in_merged_data)

  # create set without control data
  ohc_dpi_data <- subset(merged_data, !is.na(merge_id))
  
  # remove duplicates by child id
  ohc_dpi_data_no_dups <- ea_no_dups(ohc_dpi_data, "merge_id")
  
##############################
# produce ohc summary tables #
##############################
  
  # subset to data for melt
  data_to_melt <- subset(ohc_dpi_data_no_dups, select = c(merge_id, removal_date, latest_acad_yr, ohc_days_tot, num_plcmt_tot, plcmt_days_tot, 
                                                          num_plcmt_acad_yr, plcmt_days_acad_year))
  
  # melt ohc data long to summarize
  long_data <- melt.data.table(data_to_melt, id.vars = c("merge_id", "removal_date", "latest_acad_yr"))
  
  # create removal year var
  long_data[, removal_year := year(removal_date)]
  
  # sort by variable, removal year
  setorder(long_data, variable, removal_year)
  
  # calc ohc stats overall
  a_ohc_stats <- long_data[!is.na(value), list(n_obs = length(value),
                                               min = min(value),
                                               q25 = quantile(value, .25),
                                               q50 = quantile(value, .5),
                                               q75 = quantile(value, .75),
                                               max = max(value),
                                               mean = round(mean(value), 2),
                                               var = round(var(value), 2),
                                               sd = round(sd(value), 2)), by = c("variable")]

  
  # calc summary stats on long file, with by group
  a_ohc_stats_yr <- long_data[!is.na(value), list(n_obs = length(value),
                                                  min = min(value),
                                                  q25 = quantile(value, .25),
                                                  q50 = quantile(value, .5),
                                                  q75 = quantile(value, .75),
                                                  max = max(value),
                                                  mean = round(mean(value), 2),
                                                  var = round(var(value), 2),
                                                  sd = round(sd(value), 2)), by = c("variable", "removal_year")]

  # sort by variable, removal year
  setorder(long_data, variable, latest_acad_yr)
  
  # calc summary stats on long file, with by group
  a_ohc_stats_acad_yr <- long_data[!is.na(value), list(n_obs = length(value),
                                                 min = min(value),
                                                 q25 = quantile(value, .25),
                                                 q50 = quantile(value, .5),
                                                 q75 = quantile(value, .75),
                                                 max = max(value),
                                                 mean = round(mean(value), 2),
                                                 var = round(var(value), 2),
                                                 sd = round(sd(value), 2)), by = c("variable", "latest_acad_yr")]
  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write( , ".csv")
    
  }
  
  
  
  
  
  
  
  