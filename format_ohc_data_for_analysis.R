######################################################################
# notes:
# - purpose: import, format, and stack sets for each academic year
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

  # output toggle
  p_opt_exp <- 0

#############
# load data #
#############
  
  # load long ohc file
  in_ohc_data <- fread("X:/LFS-Education Outcomes/data/raw_data/dcf_long.csv", na.strings = "")

#######################
# format raw ohc data #
#######################
  
  # copy raw data file
  format_ohc_data  <- copy(in_ohc_data)

  # change var names to lowercase
  setnames(format_ohc_data, colnames(format_ohc_data), tolower(colnames(format_ohc_data)))

  # remove duplicates based on id, and placement start and end date #brule
  ohc_data_no_dups <- ea_no_dups(format_ohc_data, c("child_id", "plcmt_begin_date", "plcmt_end_date"))
  
  # fill in December 31, 2014 for placements missing end date or discharge date #brule
  ohc_data_no_dups[is.na(plcmt_end_date), plcmt_end_date := "31DEC2014"]
  ohc_data_no_dups[is.na(discharge_date), discharge_date := "31DEC2014"]

  # change format of date variables
  format_ohc_data[, lfs]
  
  # calc number of ohc days and placement days
  ohc_data_no_dups[, ohc_days := as.numeric(as.duration(dmy(discharge_date) - dmy(removal_date))) / 86400]
  ohc_data_no_dups[, plcmt_days := as.numeric(as.duration(dmy(plcmt_end_date) - dmy(plcmt_begin_date))) / 86400]

  # create school year for placement begin date, if month is after may, year = next academic year #brule
  ohc_data_no_dups[, plcmt_begin_syear := ifelse(month(dmy(plcmt_begin_date)) > 5, year(dmy(plcmt_begin_date) + years(1)), 
                                                 year(dmy(plcmt_begin_date)))]
  
  # create school year for placement end date, if month is after may, year = next academic year #brule
  ohc_data_no_dups[, plcmt_end_syear := ifelse(month(dmy(plcmt_end_date)) > 5, year(dmy(plcmt_end_date) + years(1)), year(dmy(plcmt_end_date)))]
  
#########################################
# loop over placement years in data set #
#########################################  
  
  # create set of placement start years
  plcmt_dates <- subset(ohc_data_no_dups, select = plcmt_begin_syear)
  
  # subset to unique placement years
  plcmt_dates <- ea_no_dups(plcmt_dates, "plcmt_begin_syear", opt_print = 0)
  
  # create string of plcmt years
  p_plcmt_years <- plcmt_dates$plcmt_begin_syear
  
  # initialize file list
  file_list <- list()

  ##############
  # begin loop #
  ##############
  
  for (m_year in p_plcmt_years) {

    #################################################################
    # subset to placements in academic year and calc placement days #
    #################################################################

      # create set of placements that spans placement year
      acad_yr_set <- subset(ohc_data_no_dups, plcmt_begin_syear <= m_year &  plcmt_end_syear >= m_year)
      
      # create adj start date (June 1) if placement begins prior to academic year #brule
      acad_yr_set[, adj_plcmt_begin_date := plcmt_begin_date]
      acad_yr_set[plcmt_begin_syear != m_year, adj_plcmt_begin_date := paste0("01JUN", as.character(m_year - 1))]
      
      # create adj end date (May 31) if placement ends in subsequent years #brule
      acad_yr_set[, adj_plcmt_end_date := plcmt_end_date]
      acad_yr_set[plcmt_end_syear != m_year, adj_plcmt_end_date := paste0("31MAY", as.character(m_year))]
  
      # calc number of plcmt days (in academic year)
      acad_yr_set[, plcmt_days_acad := as.numeric(as.duration(dmy(adj_plcmt_end_date) - dmy(adj_plcmt_begin_date))) / 86400]

      # aggregate placements by child by year
      agg_acad_plcmt <- acad_yr_set[, list(num_plcmt_acad_yr = .N,
                                           plcmt_days_acad_year = sum(plcmt_days_acad)),
                                    by = "child_id"]
  
    ##################################################
    # create wide set of all placements in acad year #
    ##################################################
      
      # sort based on child id and placement start and end dates
      setorder(acad_yr_set, child_id, dmy(adj_plcmt_begin_date), dmy(adj_plcmt_end_date))
      
      # number placements for casting
      acad_yr_set[, num_plcmt := seq_len(.N), by = child_id]
      
      # cast placement records wide
      acad_yr_wide <- dcast.data.table(acad_yr_set, child_id ~ num_plcmt, 
                                       value.var = c("adj_plcmt_begin_date", "adj_plcmt_end_date", "plcmt_days", "plcmt_days_acad"))
  
    ##################################################
    # create unique child set, remove plcmt day info #
    ##################################################
      
      # sort based on child id, year, acad plcmt days
      setorder(acad_yr_set, child_id, -plcmt_days_acad)
      
      # create set with one row per child, keeping record with most placement days #brule
      unique_child_set <- ea_no_dups(acad_yr_set, "child_id")
      
      # remove specific placement start / end info
      unique_child_set <- subset(unique_child_set, select = -c(plcmt_begin_date, plcmt_end_date, adj_plcmt_begin_date, adj_plcmt_end_date, plcmt_days,
                                                               plcmt_days_acad, plcmt_begin_syear, plcmt_end_syear, num_plcmt, days_plcmt_in_rpt_period))
    
      # merge on agg plcmt info
      unique_child_set <- ea_merge(unique_child_set, agg_plcmt, "child_id", "x", opt_print = 0)
  
    ####################################################
    # merge latest placement year info with child info #
    ####################################################  
      
      # merge aggregate placement info with child info
      acad_yr_agg_plcmt <- ea_merge(unique_child_set, agg_acad_plcmt, "child_id", opt_print = 0)
      
      # create acad_year variable
      acad_yr_agg_plcmt[, acad_year := m_year]
      
      # reorder vars
      ea_colorder(acad_yr_agg_plcmt, c("child_id", "acad_year", "child_dob", "child_gender", "child_race", "child_ethnicity", "child_hispanic",
                                       "child_disability", "disabilities", "icwa_child", "ohc_days_tot", "num_plcmt_tot", "plcmt_days_tot", 
                                       "num_plcmt_acad_yr", "plcmt_days_acad_year", "removal_date", "discharge", "discharge_date", "end_reason",
                                       "discharge_reason", "tpr_finalization_date", "adoption_final_date", "region", "dcf_plcmt_type"))
    
      # merge on wide plcmt info
      acad_yr_all_plcmt <- ea_merge(acad_yr_agg_plcmt, acad_yr_wide, "child_id", opt_print = 0)
  
      # add file to out list
      file_list[[m_year]] <- acad_yr_all_plcmt

  }
  
  # stack all files
  stacked_acad_yr_data <- rbindlist(file_list, fill = TRUE, use.names = TRUE)


#######################################
# aggregate total placements by child #
#######################################

  # aggregate placements by child
  agg_plcmt <- ohc_data_no_dups[, list(num_plcmt_tot = .N,
                                       plcmt_days_tot = sum(plcmt_days)),
                                by = "child_id"]

  # take average number of placement and total placement days
  a_avg_plcmt <- agg_plcmt[, list(avg_num_plcmt = mean(num_plcmt_tot),
                                  avg_plcmt_days = mean(plcmt_days_tot, na.rm = TRUE))]
  
  
  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(ohc_data_full, "X:/LFS-Education Outcomes/data/lfs_data/stacked_ohc_data_full.csv")
    ea_write(ohc_data_no_dups, "X:/LFS-Education Outcomes/data/lfs_data/stacked_ohc_data_no_dups.csv")
    ea_write(stacked_acad_yr_data, "X:/LFS-Education Outcomes/data/lfs_data/stacked_acad_yr_set.csv")

    ea_write(agg_plcmt, "X:/LFS-Education Outcomes/data/lfs_data/agg_plcmt_by_child.csv")
    ea_write(a_avg_plcmt, "X:/LFS-Education Outcomes/qc/avg_plcmts.csv")

  }

