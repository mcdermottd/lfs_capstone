######################################################################
# notes:
# - purpose: merge ohc and dpi set
# - inputs: ohc data aggregated to one row per academic year, formatted dpi data (also one row per year)
# - outputs: merged set, matched by academic year (+ control data), with unmatched ohc data removed
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
  library(data.table)

#############
# set parms #
#############

  # output toggle
  p_opt_exp <- 0

#############
# load data #
#############
  
  # load formatted dpi data
  in_stacked_dpi <- ea_load("X:/LFS-Education Outcomes/data/lfs_interim_sets/dpi_analysis_set.rdata")
  
  # load formatted ohc data
  in_stacked_ohc <- ea_load("X:/LFS-Education Outcomes/data/lfs_interim_sets/stacked_ohc_formatted.rdata")
  
#######################################
# create new ids for merge / analysis #
#######################################
  
  # copy input data
  format_dpi_set <- copy(in_stacked_dpi)
  format_ohc_set <- copy(in_stacked_ohc)

  # create lf id in dpi data
  format_dpi_set[, ":="(lf_dpi_id = paste0("dpi_", lds_student_key), lf_dcf_id = paste0("dcf_", child_id))]

  # combine ids, use dcf id if not NA, otherwise use dpi id #brule
  format_dpi_set[, lf_child_id := ifelse(lf_dcf_id != "dcf_NA", lf_dcf_id, lf_dpi_id)]
  
  # remove temporary lf ids
  format_dpi_set <- subset(format_dpi_set, select = -c(lf_dpi_id, lf_dcf_id))

  # create lf id in ohc data for merge
  setnames(format_ohc_set, "child_id", "lf_child_id")
  format_ohc_set[, lf_child_id := paste0("dcf_", lf_child_id)]

#######################################
# merge ids and determine merge rates #
#######################################
  
  # subset dpi data to merge id and remove duplicates
  dpi_data_ids <- subset(format_dpi_set, select = c(lf_child_id, lds_student_key, child_id))
  dpi_data_ids <- ea_no_dups(dpi_data_ids, "lf_child_id")

  # create dpi merge flag
  dpi_data_ids[, flag_dpi := 1]

  # subset ohc data to id and remove duplicates
  ohc_data_ids <- subset(format_ohc_set, select = lf_child_id)
  ohc_data_ids <- ea_no_dups(ohc_data_ids, "lf_child_id")
  
  # create ohc merge flag
  ohc_data_ids[, flag_ohc := 1]

  # merge ohc ids with dpi data
  combined_ids <- ea_merge(dpi_data_ids, ohc_data_ids, "lf_child_id")
  
  # fill in missing flags
  combined_ids[is.na(flag_dpi), flag_dpi := 0]
  combined_ids[is.na(flag_ohc), flag_ohc := 0]
  
  # create flags based on merge
  combined_ids[, flag_merge := ifelse(flag_dpi == 1 & flag_ohc == 1, 1, 0)]
  combined_ids[, flag_no_dpi := ifelse(ea_scan(lf_child_id, 1, "_") == "dcf" & flag_merge == 0 & flag_ohc == 1, 1, 0)]
  combined_ids[, flag_no_ohc := ifelse(ea_scan(lf_child_id, 1, "_") == "dcf" & flag_ohc == 0, 1, 0)]

  # subset to unduplicated ohc ids
  ohc_ids_merge <- subset(combined_ids, ea_scan(lf_child_id, 1, "_") == "dcf", select = c(lf_child_id, lds_student_key, child_id, flag_merge, 
                                                                                          flag_no_dpi, flag_no_ohc))
  
  # create frequency table of merge types
  a_merge_stats <- ea_table(ohc_ids_merge, c("flag_merge", "flag_no_dpi", "flag_no_ohc"), opt_percent = 1)
  
  # subset out unmerged ohc ids #brule
  merged_ids <- subset(combined_ids, flag_no_ohc != 1 & flag_no_dpi != 1)
  
  # remove unneeded merge flags
  merged_ids[, c("flag_dpi", "flag_merge", "flag_no_dpi", "flag_no_ohc") := NULL]

##############################################
# merge on dpi academic year info and format #
##############################################
  
  # merge on dpi acad year info to id list
  merged_dpi_info <- ea_merge(merged_ids, format_dpi_set, c("lf_child_id", "lds_student_key", "child_id"), "x", opt_print = 0)
  
  # create sets of ohc and non-ohc students
  merged_dpi_ohc <- subset(merged_dpi_info, flag_ohc == 1)
  merged_dpi_compare <- subset(merged_dpi_info, flag_ohc == 0)
  
############################
# subset ohc data to merge #
############################
  
  # create ohc set with only merged students
  sub_ohc_data <- ea_merge(format_ohc_set, subset(ohc_ids_merge, flag_merge == 1), "lf_child_id", "y", opt_print = 0)
  
  # create set of ohc child info
  ohc_child_info <- subset(sub_ohc_data, select = c(lf_child_id, n_ohc_tot, tot_ohc_days, first_ohc_start_date, last_ohc_end_date,
                                                     n_plcmt_tot, tot_plcmt_days, first_pstart_date, last_pend_date))
  
  # remove duplicates
  ohc_child_info <- ea_no_dups(ohc_child_info, "lf_child_id")
  
  # create set of acad. yr. ohc info
  ohc_acad_yr_info <- subset(sub_ohc_data, select = c(lf_child_id, acad_year, n_plcmt_acad, tot_plcmt_days_acad, first_pstart_date_acad, 
                                                       last_pend_date_acad, dcf_plcmt_type, region, provider_county, tpr_finalization_date, 
                                                       adoption_final_date))
  
  # merge on matching academic years from dpi data
  ohc_acad_yr_info <- ea_merge(subset(merged_dpi_ohc, select = c("lf_child_id", "acad_year", "flag_ohc")), ohc_acad_yr_info, 
                               c("lf_child_id", "acad_year"), "y")
  
  # subset ohc data with no matching dpi year #brule
  ohc_dpi_yr <- subset(ohc_acad_yr_info, !is.na(flag_ohc))
  ohc_no_dpi_yr <- subset(ohc_acad_yr_info, is.na(flag_ohc))
  
###########################################
# combine dpi data with matched ohc years #
###########################################
  
  # combine child info with dpi data
  merged_dpi_ohc <- ea_merge(merged_dpi_ohc, ohc_child_info, "lf_child_id", opt_print = 0)
  
  # merge full dpi info with matched years
  ohc_dpi_matched <- ea_merge(merged_dpi_ohc, ohc_dpi_yr, c("lf_child_id", "acad_year", "flag_ohc"))
  
###############################################
# add demographic info to unmatched ohc years #
###############################################
  
  # create set of dpi demographic info to merge with missing years
  dpi_child_info <- subset(merged_dpi_ohc, select = c(lf_child_id, lds_student_key, child_id, flag_ohc, d_male, d_female, d_elp, d_sped, d_frl, 
                                                      d_fpl, d_rpl, d_race_white, d_race_black, d_race_hispanic, d_race_indian, d_race_asian, 
                                                      d_race_missing, n_ohc_tot, tot_ohc_days, first_ohc_start_date, last_ohc_end_date, n_plcmt_tot,
                                                      tot_plcmt_days, first_pstart_date, last_pend_date)) 

  # sort based on child_id and characteristics (to remove rows with missing demographics first) #brule
  setorder(dpi_child_info, lf_child_id, d_male, na.last = TRUE)
  
  # remove duplicates on child id
  dpi_child_info <- ea_no_dups(dpi_child_info, "lf_child_id")
  
  # remove unneeded ohc flag
  ohc_no_dpi_yr[, flag_ohc := NULL]
  
  # merge with ohc data with no matching dpi year #brule
  ohc_dpi_unmatched <- ea_merge(dpi_child_info, ohc_no_dpi_yr, "lf_child_id", "y", opt_print = 0)
  
  # stack merged ohc sets
  ohc_dpi_full <- rbind(ohc_dpi_matched, ohc_dpi_unmatched, fill = TRUE)

#############################################
# create flags based on first ohc placement # 
#############################################
  
  # create flag for placement in current year
  ohc_dpi_full[, flag_ohc_plcmt := ifelse()]
  
  
  
  
######################
# format merged data #
######################
  
  # create avg days per placement vars
  child_demo_data[, avg_days_ohc := tot_ohc_days / n_ohc_tot]
  child_demo_data[, avg_days_plcmt := tot_plcmt_days / n_plcmt_tot]
  acad_yr_data[, avg_days_plcmt_acad := tot_plcmt_days_acad / n_plcmt_acad]
  
    # fill in 0 for ohc vars for comparison group
  acad_yr_data[flag_ohc == 0, c("n_plcmt_acad", "tot_plcmt_days_acad", "avg_days_plcmt_acad") := 0]
  
  # create frl / non-frl flags for comparison groups
  child_demo_data[, compare_frl := ifelse(flag_ohc == 0 & d_frl == 1, 1, 0)]
  acad_yr_data[, compare_frl := ifelse(flag_ohc == 0 & d_frl == 1, 1, 0)]

  


##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(analysis_set, "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.csv")
    save(analysis_set, file = "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")

    ea_write(dpi_ohc_child_info, "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set_child_info.csv")
    save(dpi_ohc_child_info, file = "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set_child_info.rdata")
    
    ea_write(ohc_ids_merge, "X:/LFS-Education Outcomes/qc/ohc_merged_ids.csv")
    ea_write(a_merge_stats, "X:/LFS-Education Outcomes/qc/ohc_merged_rates.csv")

  }

