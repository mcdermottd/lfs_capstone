######################################################################
# notes:
# - purpose: create formatted OHC data with one row per student per academic year (placements aggregated)
# - inputs: stacked raw OHC file from DCF (updated)
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
  
  # create lf date vars
  ohc_data_no_dups[, ":="(lf_ohc_start_date = dmy(removal_date), lf_ohc_end_date = dmy(discharge_date), lf_pstart_date = dmy(plcmt_begin_date),
                          lf_pend_date = dmy(plcmt_end_date))]
  
  # fill in December 31, 2014 for placements missing end date or discharge date #brule
  ohc_data_no_dups[is.na(lf_ohc_end_date), lf_ohc_end_date := ymd("2014-12-31")]
  ohc_data_no_dups[is.na(lf_pend_date), lf_pend_date := ymd("2014-12-31")]
  
  # remove entries when ohc begin date > placement begin date | ohc end date < placement end date #brule
  ohc_data_no_dups <- subset(ohc_data_no_dups, lf_ohc_start_date <= lf_pstart_date)
  ohc_data_no_dups <- subset(ohc_data_no_dups, lf_ohc_end_date >= lf_pend_date)

  # create school year for placement begin and end dates, if month is after May (5), year = next academic year #brule
  ohc_data_no_dups[, pstart_sch_yr := ifelse(month(lf_pstart_date) > 5, year(lf_pstart_date) + 1, year(lf_pstart_date))]
  ohc_data_no_dups[, pend_sch_yr := ifelse(month(lf_pend_date) > 5, year(lf_pend_date) + 1, year(lf_pend_date))]
  
#########################################
# loop over placement years in data set #
#########################################  
  
  # create set of placement start years
  plcmt_dates <- subset(ohc_data_no_dups, select = pstart_sch_yr)
  
  # subset to unique placement years
  plcmt_dates <- ea_no_dups(plcmt_dates, "pstart_sch_yr", opt_print = 0)
  
  # create string of plcmt years
  p_plcmt_years <- plcmt_dates$pstart_sch_yr

  ##############
  # begin loop #
  ##############
  
  for (m_year in p_plcmt_years) {

    #################################################################
    # subset to placements in academic year and calc placement days #
    #################################################################

      # create set of placements that span placement year
      acad_yr_set <- subset(ohc_data_no_dups, pstart_sch_yr <= m_year &  pend_sch_yr >= m_year)
      
      # if placement begins prior to academic year, set adj begin date to June 1 #brule
      acad_yr_set[, adj_pstart_date := lf_pstart_date]
      acad_yr_set[pstart_sch_yr != m_year, adj_pstart_date := ymd(paste0((m_year - 1), "-06-01"))]

      # if placement ends after academic year, set adj end date to May 31 #brule
      acad_yr_set[, adj_pend_date := lf_pend_date]
      acad_yr_set[pend_sch_yr != m_year, adj_pend_date := ymd(paste0(m_year, "-05-31"))]
  
      # calc number of plcmt days in academic year
      acad_yr_set[, plcmt_days_acad := adj_pend_date - adj_pstart_date]

      # aggregate placements by child by year
      agg_acad_plcmt <- acad_yr_set[, list(n_plcmt_acad = .N,
                                           tot_plcmt_days_acad = sum(plcmt_days_acad),
                                           first_pstart_date_acad = min(adj_pstart_date),
                                           last_pend_date_acad = max(adj_pend_date)),
                                    by = "child_id"]
  
    ##################################################
    # create wide set of all placements in acad year #
    ##################################################
      
      # sort based on child id and placement start and end dates
      setorder(acad_yr_set, child_id, adj_pstart_date, adj_pend_date)
      
      # number placements for casting
      acad_yr_set[, plcmt_num := seq_len(.N), by = child_id]
      
      # cast placement records wide
      acad_yr_wide <- dcast.data.table(acad_yr_set, child_id ~ plcmt_num, value.var = c("adj_pstart_date", "adj_pend_date", "plcmt_days_acad"))
  
    ##################################################
    # create unique child set, remove plcmt day info #
    ##################################################
      
      # sort based on child id, year, acad plcmt days
      setorder(acad_yr_set, child_id, -plcmt_days_acad)
      
      # create set with one row per child, keeping record with most placement days #brule
      unique_child_set <- ea_no_dups(acad_yr_set, "child_id")
      
      # remove specific placement start / end info
      unique_child_set <- subset(unique_child_set, select = c(child_id, child_gender, child_dob, child_age, child_race, child_ethnicity, 
                                                              child_hispanic, child_disability, disabilities, icwa_child, dcf_plcmt_type, region,
                                                              provider_county, tpr_finalization_date, adoption_final_date))
  
    ###################################################
    # merge child info with aggregated placement info #
    ###################################################  
      
      # merge aggregate placement info with child info
      acad_yr_plcmt_full <- ea_merge(unique_child_set, agg_acad_plcmt, "child_id", opt_print = 0)
      
      # remove row if total placement days <=14 days #brule
      acad_yr_plcmt_full <- subset(acad_yr_plcmt_full, tot_plcmt_days_acad > 14)
      
      # merge on wide placement info
      acad_yr_plcmt_full <- ea_merge(acad_yr_plcmt_full, acad_yr_wide, "child_id", "x", opt_print = 0)
      
      # create acad_year variable
      acad_yr_plcmt_full[, acad_year := m_year]
      
      # reorder acad year to beginning of set
      ea_colorder(acad_yr_plcmt_full, c("acad_year", "child_id"))
  
    ####################
    # stack acad files #
    ####################
    
    # set all variables to character to stack  
    acad_yr_plcmt_full[] <- lapply(acad_yr_plcmt_full[], as.character)

    # stack sets
    if (m_year == p_plcmt_years[1]) { stacked_acad_yr_set <- acad_yr_plcmt_full }
    if (m_year != p_plcmt_years[1]) { stacked_acad_yr_set <- rbind(stacked_acad_yr_set, acad_yr_plcmt_full, use.names = TRUE, fill = TRUE) }

  }
  
##############################################
# subset full set of placements to aggregate # 
##############################################

  # remove placements that end before June 1, 2007 or end after May 31, 2012 #brule
  sub_ohc_data <- subset(ohc_data_no_dups, !(lf_pend_date < ymd("2007-06-01")) & !(lf_pstart_date > ymd("2012-05-31")))
  
  # adjust ohc and placement start dates based on truncation: if date < June 1, 2007, set to date #brule
  sub_ohc_data[, ":="(adj_ohc_start_date = lf_ohc_start_date, adj_pstart_date = lf_pstart_date)]
  sub_ohc_data[lf_ohc_start_date < ymd("2007-06-01"), adj_ohc_start_date := ymd("2007-06-01")]
  sub_ohc_data[lf_pstart_date < ymd("2007-06-01"), adj_pstart_date := ymd("2007-06-01")]

  # adjust ohc and placement end dates based on truncation: if date > May 31, 2012, set to date #brule
  sub_ohc_data[, ":="(adj_pend_date = lf_pend_date, adj_ohc_end_date = lf_ohc_end_date)]
  sub_ohc_data[lf_pend_date > ymd("2012-05-31"), adj_pend_date := ymd("2012-05-31")]
  sub_ohc_data[lf_ohc_end_date > ymd("2012-05-31"), adj_ohc_end_date := ymd("2012-05-31")]

  # calc full number of ohc days and placement days
  sub_ohc_data[, ohc_days := lf_ohc_end_date - lf_ohc_start_date]
  sub_ohc_data[, plcmt_days := lf_pend_date - lf_pstart_date]
  
  # calc adjusted number of ohc days and placement days
  sub_ohc_data[, adj_ohc_days := adj_ohc_end_date - adj_ohc_start_date]
  sub_ohc_data[, adj_plcmt_days := adj_pend_date - adj_pstart_date]
  
  # delete placement if adjusted placement days = 0 and != unadjusted placement days #brule
  sub_ohc_data <- subset(sub_ohc_data, !(adj_plcmt_days == 0 & adj_plcmt_days != plcmt_days))

#########################################
# aggregate ohc and placements by child #
#########################################
  
  # aggregate placements by child
  agg_plcmt <- sub_ohc_data[, list(n_plcmt_tot = .N,
                                   tot_plcmt_days = sum(adj_plcmt_days),
                                   first_pstart_date = min(adj_pstart_date),
                                   last_pend_date = max(adj_pend_date)),
                            by = "child_id"]
  
  # create set of unduplicated ohc periods by child
  ohc_period_no_dups <- ea_no_dups(sub_ohc_data, c("child_id", "adj_ohc_start_date", "adj_ohc_end_date"))
  
  # aggregate ohc periods by child
  agg_ohc <- ohc_period_no_dups[, list(n_ohc_tot = .N,
                                       tot_ohc_days = sum(adj_ohc_days),
                                       first_ohc_start_date = min(adj_ohc_start_date),
                                       last_ohc_end_date = max(adj_ohc_end_date)),
                                by = "child_id"]
  
  # combine agg placements and ohc
  agg_plcmt_ohc <- ea_merge(agg_ohc, agg_plcmt, "child_id", opt_print = 0)
  
##########################################
# merge agg totals with stacked acad set #
##########################################
  
  # remove acad years not in analysis period from stacked set #brule
  sub_stacked_acad_set <- subset(stacked_acad_yr_set, acad_year > 2007 & acad_year < 2013)
  
  # change child id var to character for merge
  agg_plcmt_ohc[, child_id := as.character(child_id)]
  
  # merge agg placement info with stacked set
  ohc_analysis_set <- ea_merge(agg_plcmt_ohc, sub_stacked_acad_set, "child_id", "y", opt_print = 0)
  
#####################
# format and export #
#####################
  
  # reorder vars in analysis set
  ea_colorder(ohc_analysis_set, c("child_id", "acad_year", "child_gender", "child_dob", "child_age", "child_race", "child_ethnicity", 
                                  "child_hispanic", "child_disability", "disabilities", "icwa_child", "n_ohc_tot", "tot_ohc_days", 
                                  "first_ohc_start_date", "last_ohc_end_date", "n_plcmt_tot", "tot_plcmt_days", "first_pstart_date", "last_pend_date",
                                  "n_plcmt_acad", "tot_plcmt_days_acad", "first_pstart_date_acad", "last_pend_date_acad"))

  # sort by child id and acad year
  setorder(ohc_analysis_set, child_id, acad_year)

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(ohc_analysis_set, "X:/LFS-Education Outcomes/data/lfs_data/stacked_ohc_analysis_set.csv")
    save(ohc_analysis_set, file = "X:/LFS-Education Outcomes/data/lfs_data/stacked_ohc_analysis_set.rdata")


  }

