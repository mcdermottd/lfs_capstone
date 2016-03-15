######################################################################
# notes:
# - purpose: import, format, and stack individual year files, including consolidating ohc placements and adding dummy variables
# - inputs: 5 ohc long files, 2008 - 2012
# - outputs: consolidated set with all years stacked together
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
  library(lubridate)

#############
# set parms #
#############
    
  # load id xwalk
  p_id_xwalk <- fread("X:/LFS-Education Outcomes/data/raw_data/xwalk_child_id.csv")
  
  # output toggle
  p_opt_exp <- 0

##################################
# create list of files to import #
##################################
  
  # store raw directory path
  p_dir_raw <- "X:/LFS-Education Outcomes/data/raw_data/"
  
  # import names of files
  p_file_names <- list.files(p_dir_raw)
  p_file_names <- grep("placement", p_file_names, value = TRUE)

###################################
# format and stack dcf year files #
###################################

  # define function to open data
  func_format_stack <- function(x_file) {

    # start file list
    file_list <- list()

    # for file in path
    for (m_file in x_file) {
      
      print(m_file)

    #############
    # load data #
    #############
      
      # load long ohc file
      in_ohc_data <- fread(paste0(p_dir_raw, m_file))
      
      # add file to out list
      file_list[[m_file]] <- in_ohc_data
    }

    ######################################
    # stack and return full set of files #
    ######################################

    # stack all files
    out_file <- rbindlist(file_list, fill = TRUE, use.names = TRUE)

    # return the stacked file
    return(out_file)
  }

  # run function for full list of year files
  stacked_set <- func_format_stack(p_file_names)
 
#############################################
# remove exact duplicates from stacked data #
#############################################
  
  # copy stacked file
  ohc_data_full  <- copy(stacked_set)

  # remove all exact duplicates from stacked data
  ohc_data_full <- ea_no_dups(ohc_data_full, opt_key_all = 1) 

###################################################
# remove placements with the same start/end dates #
###################################################
  
  # change var names to lowercase
  setnames(ohc_data_full, colnames(ohc_data_full), tolower(colnames(ohc_data_full)))
  
  # sort based on child id, placement dates, and end reason (end reason = NA appears last) #brule
  setorder(ohc_data_full, child_id, plcmt_begin_date, plcmt_end_date, end_reason, na.last = TRUE)
  
  # remove duplicates based on id, and placement start and end date #brule
  ohc_data_no_dups <- ea_no_dups(ohc_data_full, c("child_id", "plcmt_begin_date", "plcmt_end_date"))
  
#################################
# add additional placement vars #
#################################
  
  # create begin and end years (overall ohc and specific placement)
  ohc_data_no_dups[, ohc_begin_syear := ifelse(month(removal_date) > 5, year(ymd(removal_date) + years(1)), year(removal_date))]
  ohc_data_no_dups[, ohc_end_syear := ifelse(month(discharge_date) > 5, year(ymd(discharge_date) + years(1)), year(discharge_date))]
  ohc_data_no_dups[, plcmt_begin_syear := ifelse(month(plcmt_begin_date) > 5, year(ymd(plcmt_begin_date) + years(1)), year(plcmt_begin_date))]
  ohc_data_no_dups[, plcmt_end_syear := ifelse(month(plcmt_end_date) > 5, year(ymd(plcmt_end_date) + years(1)), year(plcmt_end_date))]
  
#################################################
# calc number of ohc / plcmt days #
#################################################
  
  # remove placements from data that begin after 11-12 academic year (2012-05-31) #brule
  sub_ohc_data <- subset(ohc_data_no_dups, ymd(plcmt_begin_date) <= ymd("2012-05-31"))
  
  # cap placements / ohc that ends after 11-12 academic year or are NA (ongoing placements at time of data pull) at 2012-05-31 #brule
  sub_ohc_data[, adj_plcmt_end_date := plcmt_end_date]
  sub_ohc_data[ymd(adj_plcmt_end_date) > ymd("2012-05-31") | is.na(adj_plcmt_end_date), adj_plcmt_end_date := "2012-05-31"]
  sub_ohc_data[, adj_discharge_date := discharge_date]
  sub_ohc_data[ymd(adj_discharge_date) > ymd("2012-05-31") | is.na(adj_discharge_date), adj_discharge_date := "2012-05-31"]
  
  # calc number of ohc days and plcmt days 
  sub_ohc_data[, tot_ohc_days := (ymd(adj_discharge_date) - ymd(removal_date)) / 86400]
  sub_ohc_data[, plcmt_days := (ymd(adj_plcmt_end_date) - ymd(plcmt_begin_date)) / 86400]

  # remove placement = 0 days

  

  
  
#######################################
# aggregate total placements by child #
#######################################
  
  # aggregate placements by child
  agg_plcmt <- ohc_data_no_dups[, list(tot_num_plcmt = .N,
                                        tot_plcmt_days = sum(plcmt_days)),
                                 by = "child_id"]
  
  # take average number of placement and total placement days
  a_avg_plcmt <- agg_plcmt[, list(avg_plcmt = mean(tot_num_plcmt),
                                    avg_plcmt_days = mean(tot_plcmt_days, na.rm = TRUE))]
    
#######################################################
# create aggregated set of latest placement per child #
#######################################################


  
  # update placement end year and plcmt days vars #brule
  sub_ohc_data[, plcmt_end_year := as.numeric(format(plcmt_end_date, "%Y"))]
  sub_ohc_data[, plcmt_days := difftime(plcmt_end_date, plcmt_begin_date, units = "days")]

  # aggregate placements by child by year
  agg_plcmt_by_year <- sub_ohc_data[, list(yr_num_plcmt = .N,
                                            yr_plcmt_days = sum(plcmt_days)),
                                     by = c("child_id", "plcmt_end_year")]
  
  # remove years with placement days < 30 #brule
  sub_plcmt_by_year <- subset(agg_plcmt_by_year, yr_plcmt_days >= 30)
  
  # sort based on child id and year
  setorder(sub_plcmt_by_year, child_id, -plcmt_end_year)
  
  # subset to one row per child, keeping latest placement #brule
  sub_plcmt_by_year <- ea_no_dups(sub_plcmt_by_year, "child_id")
  
  # shorten placements longer than 365 to full year #brule
  sub_plcmt_by_year[yr_plcmt_days > 365, yr_plcmt_days := 365]
  
####################################################
# create wide set of all placements in latest year #
####################################################
  
  # merge on latest plcmt year set to only keep each child's placements in latest year #brule
  lst_plcmt_set <- ea_merge(sub_ohc_data, sub_plcmt_by_year, c("child_id", "plcmt_end_year"), "y", opt_print = 0)
  
  # sort based on child id and placement start date
  setorder(lst_plcmt_set, child_id, plcmt_begin_date)
  
  # number placements for casting
  lst_plcmt_set[, num_plcmt := seq_len(.N), by = child_id]
  
  # cast placement records wide
  lst_plcmt_wide <- dcast.data.table(lst_plcmt_set, child_id ~ num_plcmt, value.var = c("plcmt_begin_date", "plcmt_end_date", "plcmt_days"))
  
####################################################
# merge latest placement year info with child info #
####################################################
  
  # sort based on child id, year, plcmt days
  setorder(sub_ohc_data, child_id, -plcmt_end_year, -plcmt_days)
  
  # create set with one row per child, keeping record with most placement days in latest placment year #brule
  unique_child_set <- ea_no_dups(sub_ohc_data, "child_id")
  
  # merge on agg plcmt info
  unique_child_set <- ea_merge(unique_child_set, agg_plcmt, "child_id", "x", opt_print = 0)
  
  # keep only child specific vars
  unique_child_set <- subset(unique_child_set, select = c(child_id, child_dob, child_gender, child_race, child_ethnicity, child_hispanic, 
                                                          child_disability, disabilities, icwa_child, fl_mntal_retardatn, fl_phys_disabled, 
                                                          fl_vis_hearing_impr, fl_emotion_dstrbd, fl_othr_spc_care, fl_lrn_disability, 
                                                          child_level_of_need, case_id, case_type, dcf_plcmt_type, region, site_region, 
                                                          placing_county, plcmnt_care_rspnsblty_county, provider_county, ohc_begin_year, ohc_end_year,
                                                          removal_date, discharge_date, tpr_finalization_date, adoption_final_date, end_reason, 
                                                          discharge_reason, tot_ohc_days, tot_num_plcmt, tot_plcmt_days))
  
  # rename plcmt end year var for merge
  setnames(sub_plcmt_by_year, c("plcmt_end_year", "yr_num_plcmt", "yr_plcmt_days"), c("lst_plcmt_yr", "lst_yr_num_plcmt", "lst_yr_plcmt_days"))
      
  # merge aggregate placement info with child info
  full_set_agg_plcmt <- ea_merge(unique_child_set, sub_plcmt_by_year, "child_id", "y", opt_print = 0)

  # merge on wide plcmt info
  full_set_all_plcmt <- ea_merge(full_set_agg_plcmt, lst_plcmt_wide, "child_id", opt_print = 0)

#######################
# add on merge id var #
#######################
  
  # rename id var names for merge
  setnames(p_id_xwalk, c("new_id", "CHILD_ID"), c("dcf_id", "child_id"))

  # remove duplicates on dcf id #brule
  p_id_xwalk <- ea_no_dups(p_id_xwalk, "dcf_id")
  
  # rename child id for merge
  setnames(ohc_data_full, "child_id", "dcf_id")
  setnames(ohc_data_no_dups, "child_id", "dcf_id")
  setnames(full_set_agg_plcmt, "child_id", "dcf_id")
  setnames(full_set_all_plcmt, "child_id", "dcf_id")
  setnames(agg_plcmt, "child_id", "dcf_id")
  setnames(agg_plcmt_by_year, "child_id", "dcf_id")
  
  # merge on xwalk ids
  ohc_data_full <- ea_merge(p_id_xwalk, ohc_data_full, "dcf_id", "y", opt_print = 0)
  ohc_data_no_dups <- ea_merge(p_id_xwalk, ohc_data_no_dups, "dcf_id", "y", opt_print = 0)
  full_set_agg_plcmt <- ea_merge(p_id_xwalk, full_set_agg_plcmt, "dcf_id", "y", opt_print = 0)
  full_set_all_plcmt <- ea_merge(p_id_xwalk, full_set_all_plcmt, "dcf_id", "y", opt_print = 0)
  agg_plcmt <- ea_merge(p_id_xwalk, agg_plcmt, "dcf_id", "y", opt_print = 0)
  agg_plcmt_by_year <- ea_merge(p_id_xwalk, agg_plcmt_by_year, "dcf_id", "y", opt_print = 0)

##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(ohc_data_full, "X:/LFS-Education Outcomes/data/lfs_data/stacked_ohc_data_full.csv")
    ea_write(ohc_data_no_dups, "X:/LFS-Education Outcomes/data/lfs_data/stacked_ohc_data_no_dups.csv")
    ea_write(full_set_agg_plcmt, "X:/LFS-Education Outcomes/data/lfs_data/lst_set_agg_plcmt.csv")
    ea_write(full_set_all_plcmt, "X:/LFS-Education Outcomes/data/lfs_data/lst_set_all_plcmt.csv")
    
    ea_write(agg_plcmt, "X:/LFS-Education Outcomes/data/lfs_data/agg_plcmt_by_child.csv")
    ea_write(agg_plcmt, "X:/LFS-Education Outcomes/data/lfs_data/agg_plcmt_by_child_yr.csv")

  }

