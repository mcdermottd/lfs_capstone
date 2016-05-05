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
  library(lubridate)
  library(foreign)
  library(data.table)
  library(eaanalysis)

#############
# set parms #
#############

  # create region strings (for recoding)
  p_reg_nc <- c("Vilas", "Oneida", "Forest", "Lincoln", "Langlade", "Marathon", "Wood", "Portage", "Adams")
  p_reg_ne <- c("Florence", "Marinette", "Menominee", "Oconto", "Shawano", "Waupaca", "Outagamie", "Brown", "Kewaunee", "Door", "Waushara", 
                "Winnebago", "Calumet", "Manitowoc", "Green Lake", "Fond Du Lac", "Fond du Lac", "Sheboygan")
  p_reg_nw <- c("Bayfield", "Ashland", "Iron", "Sawyer", "Price", "Taylor", "Douglas", "Burnett", "Washburn", "Polk", "Barron", "Rusk")
  p_reg_se <- c("Walworth", "Racine", "Kenosha")
  p_reg_s  <- c("Marquette", "Richland", "Sauk", "Columbia", "Dodge", "Washington", "Ozaukee", "Grant", "Iowa", "Dane", "Jefferson", "Waukesha",
                "Lafayette", "Green", "Rock")
  p_reg_w  <- c("St Croix", "Saint Croix", "Dunn", "Chippewa", "Pierce", "Pepin", "Eau Claire", "Clark", "Buffalo", "Trempealeau", "Jackson", 
                "La Crosse", "Monroe", "Juneau", "Vernon", "Crawford")
  
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

#########################
# merge dpi and ohc ids #
#########################
  
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

  # remove unmerged ohc ids #brule
  merged_ids <- subset(combined_ids, flag_no_ohc != 1 & flag_no_dpi != 1)
  
  # remove unneeded merge flags
  merged_ids[, c("flag_dpi", "flag_merge", "flag_no_dpi", "flag_no_ohc") := NULL]
  
###########################
# investigate merge rates #
###########################
  
  # subset to unduplicated ohc ids
  ohc_ids_merge <- subset(combined_ids, ea_scan(lf_child_id, 1, "_") == "dcf", select = c(lf_child_id, lds_student_key, child_id, flag_merge, 
                                                                                          flag_no_dpi, flag_no_ohc))
  
  # create frequency table of merge types
  a_merge_stats <- ea_table(ohc_ids_merge, c("flag_merge", "flag_no_dpi", "flag_no_ohc"), opt_percent = 1)
  
  # create dpi and ohc sets to compare years and grades
  ohc_compare <- subset(format_ohc_set, select = c(lf_child_id, acad_year, child_age))
  dpi_compare <- subset(format_dpi_set, ea_scan(lf_child_id, 1, "_") == "dcf", select = c(lf_child_id, acad_year, grade_level_cd))

  # combine with merged ids
  ohc_compare <- ea_merge(ohc_compare, merged_ids, "lf_child_id", "x")
  dpi_compare <- ea_merge(dpi_compare, merged_ids, "lf_child_id", "x")

  # rename flag_ohf
  setnames(ohc_compare, "flag_ohc", "id_merge")
  setnames(dpi_compare, "flag_ohc", "id_merge")

  # set missings to 0
  ohc_compare[is.na(id_merge), id_merge := 0]
  dpi_compare[is.na(id_merge), id_merge := 0]

  # create frequency table of OHC merge rates
  a_merge_stats_ohc_yr <- ea_table(ohc_compare, c("acad_year", "id_merge"), opt_percent = 1)
  a_merge_stats_ohc_age <- ea_table(ohc_compare, c("child_age", "id_merge"), opt_percent = 1)

  # create frequency table of DPI merge rates
  a_merge_stats_dpi_yr <- ea_table(dpi_compare, c("acad_year", "id_merge"), opt_percent = 1)
  a_merge_stats_dpi_grd <- ea_table(dpi_compare, c("grade_level_cd", "id_merge"), opt_percent = 1)

############################
# investigate unmerged ids #
############################
  
  # subset to separate sets of unmerged ids
  ohc_unmerged_ids <- subset(ohc_ids_merge, flag_no_dpi == 1, select = c(lf_child_id, flag_no_dpi))
  dpi_unmerged_ids <- subset(ohc_ids_merge, flag_no_ohc == 1, select = c(lf_child_id, flag_no_ohc))

  # create full set of records for unmerged students
  ohc_unmerged_set <- ea_merge(ohc_unmerged_ids, format_ohc_set, "lf_child_id", "x", opt_print = 0)
  dpi_unmerged_set <- ea_merge(dpi_unmerged_ids, format_dpi_set, "lf_child_id", "x", opt_print = 0)

  # sort based on age
  ohc_unmerged_set[, child_age := as.numeric(child_age)]
  setorder(ohc_unmerged_set, lf_child_id, child_age)

  # take frequence of chid age in the DCF data
  a_unmerged_age <- ea_table(ohc_unmerged_set, "child_age", opt_percent = 1)
  
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
  
  # merge with ohc data with no matching dpi year (to use dpi demographics) #brule
  ohc_dpi_unmatched <- ea_merge(dpi_child_info, ohc_no_dpi_yr, "lf_child_id", "y", opt_print = 0)
  
  # stack merged ohc sets
  ohc_dpi_full <- rbind(ohc_dpi_matched, ohc_dpi_unmatched, fill = TRUE)

###############################
# combine all data and format #
###############################
  
  # stack ohc and comparison group data
  stacked_data <- rbind(ohc_dpi_full, merged_dpi_compare, fill = TRUE)
  
  # create flag for placement in current year
  stacked_data[, flag_cur_plcmt := ifelse(!is.na(n_plcmt_acad), 1, 0)]
  
  # create flag if prior placement
  stacked_data[, flag_prior_plcmt := ifelse(is.na(n_plcmt_acad) & first_pstart_date < ymd(paste0((as.numeric(acad_year) - 1), "-06-01")), 1, 0)]
  stacked_data[flag_ohc == 0, flag_prior_plcmt := 0]

  # create frl / non-frl flags for comparison groups
  stacked_data[, flag_compare_frl := ifelse(flag_ohc == 0 & d_frl == 1, 1, 0)]

####################################
# create additional placement vars #
####################################

  # change necessary vars to numeric
  change_vars <- c("n_ohc_tot", "tot_ohc_days", "n_plcmt_tot", "tot_plcmt_days", "n_plcmt_acad", "tot_plcmt_days_acad")
  stacked_data[, change_vars] <- lapply(stacked_data[, change_vars, with = FALSE], as.numeric)

  # create truncated placement vars #brule
  stacked_data[, ":="(lf_n_plcmt_tot = n_plcmt_tot, lf_n_plcmt_acad = n_plcmt_acad)]
  stacked_data[lf_n_plcmt_tot > 20, lf_n_plcmt_tot := 20]
  stacked_data[lf_n_plcmt_acad > 10, lf_n_plcmt_acad := 10]
  
  # fill in 0 for missing ohc vars
  stacked_data[flag_ohc == 0, c(change_vars) := 0]
  stacked_data[flag_cur_plcmt == 0, c("n_plcmt_acad", "tot_plcmt_days_acad", "lf_n_plcmt_acad") := 0]

###########################
# fill in missing regions #
###########################
  
  # combine county variables
  stacked_data[, lf_county := provider_county]
  stacked_data[is.na(lf_county), lf_county := sch_county_name]

  # create regions for missings #brule
  stacked_data[, lf_region := region]
  stacked_data[is.na(lf_region) & lf_county %in% p_reg_nc, lf_region := "Northcentral"]
  stacked_data[is.na(lf_region) & lf_county %in% p_reg_ne, lf_region := "Northeast"]
  stacked_data[is.na(lf_region) & lf_county %in% p_reg_nw, lf_region := "Northwest"]
  stacked_data[is.na(lf_region) & lf_county %in% p_reg_se, lf_region := "Southeast"]
  stacked_data[is.na(lf_region) & lf_county %in% p_reg_s, lf_region := "South"]
  stacked_data[is.na(lf_region) & lf_county %in% p_reg_w, lf_region := "West"]
  stacked_data[is.na(lf_region) & lf_county == "Milwaukee", lf_region := "Milwaukee"]

################################################
# merge leading scores for 7th and 9th graders #
################################################
  
  # create list of vars to subset, rename
  change_vars <- c("acad_year", "grade_level_cd", "test_date", "zscore_math_kce", "perf_level_math", "zscore_rdg_kce", "perf_level_rdg")
  
  # create set of 8th and 10th grade test scores
  leading_scores <- subset(stacked_data, grade_level_cd %in% c("08", "10"), select = c("lf_child_id", change_vars))
  
  # update colnames for merge
  setnames(leading_scores, change_vars, paste0("nxt_", change_vars))
  
  # create acad_year var for merge
  leading_scores[, acad_year := as.character(as.numeric(nxt_acad_year) - 1)]
  
  # merge scores with data set
  stacked_data_lscores <- ea_merge(stacked_data, leading_scores, c("lf_child_id", "acad_year"), "x")

#################################
# create college-ready variable #
#################################
  
  # create college-ready flag based on math score #brule
  stacked_data_lscores[, flag_col_rdy := ifelse(perf_level_math == 4, 1, 0)]
  stacked_data_lscores[, flag_col_rdy_nxt := ifelse(nxt_perf_level_math == 4, 1, 0)]

###################################
# add additional analysis dummies #
###################################

  # rename grade var to dummy
  setnames(stacked_data_lscores, "grade_level_cd", "grade")
  
  # create new placement type var to dummy #brule
  stacked_data_lscores[dcf_plcmt_type == "Foster Home (Non-Relative)", p_type := "fhome_nonrel"]
  stacked_data_lscores[dcf_plcmt_type == "Foster Home (Relative)", p_type := "fhome_rel"]
  stacked_data_lscores[dcf_plcmt_type == "Group Home", p_type := "group_home"]
  stacked_data_lscores[dcf_plcmt_type == "RCC", p_type := "rcc"]
  stacked_data_lscores[is.na(dcf_plcmt_type) & flag_cur_plcmt == 1, p_type := "other"]

  # adjust region var to dummy
  stacked_data_lscores[lf_region == "Milwaukee", lf_region := "mke"]
  stacked_data_lscores[lf_region == "Northcentral", lf_region := "nc"]
  stacked_data_lscores[lf_region == "Northeast", lf_region := "ne"]
  stacked_data_lscores[lf_region == "Northwest", lf_region := "nw"]
  stacked_data_lscores[lf_region == "South", lf_region := "s"]
  stacked_data_lscores[lf_region == "Southeast", lf_region := "se"]
  stacked_data_lscores[lf_region == "West", lf_region := "w"]

  # convert county var to lowercase and one word to dummy
  stacked_data_lscores[, lf_county := tolower(lf_county)]
  stacked_data_lscores[, lf_county := gsub(" ", "_", lf_county)]
  
  # dummy necessary vars
  out_dummy <- db_dummy(stacked_data_lscores, c("acad_year", "grade", "p_type", "lf_region", "lf_county"))
  
  # save output from dummy function
  analysis_set <- out_dummy$out_data_dummy

#####################
# format and export #
#####################
  
  # copy analysis set to export
  out_analysis_set <- copy(analysis_set)
  
  # remove acad years not in analysis period (< 2008 and > 2012) #brule
  out_analysis_set <- subset(out_analysis_set, acad_year %in% c("2008", "2009", "2010", "2011", "2012"))
  
  # remove unneeded acad year dummies
  out_analysis_set[, c("d_acad_year_2006", "d_acad_year_2007", "d_acad_year_2013", "d_acad_year_2014") := NULL]
  
  # reorder vars
  ea_colorder(out_analysis_set, c("lf_child_id", "lds_student_key", "child_id", "acad_year", "flag_ohc", "flag_cur_plcmt", "flag_prior_plcmt", 
                                  "flag_compare_frl", "flag_analysis_grd", "grade", "age_in_years_cd", "lf_sch_id", "lf_region", "lf_county"))

  # export
  if (p_opt_exp == 1) { 
    
    # save analysis set (multiple formats)
    save(out_analysis_set, file = "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.rdata")
    write.dta(out_analysis_set, "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.dta")
    # ea_write(out_analysis_set, "X:/LFS-Education Outcomes/data/lfs_analysis_sets/analysis_set.csv")
    
    # save qc tables of merge rates
    ea_write(ohc_ids_merge, "X:/LFS-Education Outcomes/qc/ohc_merged_ids.csv")
    ea_write(a_merge_stats, "X:/LFS-Education Outcomes/qc/overall_id_merge_rates.csv")
    ea_write(a_merge_stats_ohc_yr, "X:/LFS-Education Outcomes/qc/ohc_merge_rates_yr.csv")
    ea_write(a_merge_stats_ohc_age, "X:/LFS-Education Outcomes/qc/ohc_merge_rates_age.csv")
    ea_write(a_merge_stats_dpi_yr, "X:/LFS-Education Outcomes/qc/dpi_merge_rates_yr.csv")
    ea_write(a_merge_stats_dpi_grd, "X:/LFS-Education Outcomes/qc/dpi_merge_rates_grade.csv")
    
    # save frequency table from dummy function
    ea_write(out_dummy$out_dummy_freqs, "X:/LFS-Education Outcomes/qc/dummy_freqs.csv")

  }

