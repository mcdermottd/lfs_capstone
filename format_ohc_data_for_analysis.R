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
  library(data.table)

#############
# set parms #
#############
  
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

      # set data year
      p_data_year <- ea_scan(m_file, 3, "_")
      p_data_year <- gsub(".csv", "", p_data_year)

    #############
    # load data #
    #############
      
      # load long ohc file
      in_ohc_data <- fread(paste0(p_dir_raw, m_file))
    
    ############################
    # remove duplicate entries #
    ############################
      
      # copy raw file
      sub_ohc_data  <- copy(in_ohc_data)
      
      # # create list of vars to check on duplicates
      # vars_dup <- colnames(sub_ohc_data)
      # vars_nocheck <- c("site_region", "case_type", "child_person_type", "child_level_of_need", "provider_loc", "plcmt_with_sibling", 
      #                   "days_plcmt_in_rpt_period") 
      # vars_dup <- setdiff(vars_dup, vars_nocheck)
      # 
      # # subset based on duplicate vars list #brule
      # sub_ohc_data <- ea_no_dups(sub_ohc_data, vars_dup)
      # 
      # # sort based on child id, placement end date, and discharge variables (reverse sort discharge so "yes" appears first)
      # setorder(sub_ohc_data, child_id, plcmt_end_date, -discharge, discharge_reason, end_reason)
      # 
      # # remove discharge vars from no dup check
      # vars_dup <- setdiff(vars_dup, c("discharge", "discharge_reason", "end_reason"))
      # 
      # # remove additional duplicates ignoring discharge vars #brule
      # sub_ohc_data <- ea_no_dups(sub_ohc_data, vars_dup)
      
      # remove duplicates based on id, and placement start and end date #brule
      sub_ohc_data <- ea_no_dups(sub_ohc_data, c("child_id", "plcmt_begin_date", "plcmt_end_date"))
      
    ##########################
    # subset to current year #
    ##########################
      
      # create placement year vars
      sub_ohc_data[, plcmt_begin_year := as.numeric(ea_scan(plcmt_begin_date, 1, "-"))]
      sub_ohc_data[, plcmt_end_year := as.numeric(ea_scan(plcmt_end_date, 1, "-"))]
      
      # create flag for in placement year
      sub_ohc_data[, flag_in_year := 0]
      sub_ohc_data[plcmt_begin_year <= p_data_year & plcmt_end_year >= p_data_year, flag_in_year := 1]
      
      # subset to placements from current year #brule
      sub_ohc_data <- subset(sub_ohc_data, flag_in_year == 1)
    
    ################################
    # calculate days per placement #
    ################################
    
      # convert placement date vars to date
      sub_ohc_data[, lfs_begin_date := as.Date(plcmt_begin_date, "%Y-%m-%d")]
      sub_ohc_data[, lfs_end_date := as.Date(plcmt_end_date, "%Y-%m-%d")]
      
      # calculate number of days in placement #brule
      sub_ohc_data[plcmt_begin_year < p_data_year & plcmt_end_year > p_data_year, lfs_ohc_days := 365]
      sub_ohc_data[plcmt_begin_year < p_data_year & plcmt_end_year == p_data_year, 
                   lfs_ohc_days := lfs_end_date - as.Date(paste0(p_data_year, "-01-01"))]
      sub_ohc_data[plcmt_begin_year == p_data_year & plcmt_end_year == p_data_year, lfs_ohc_days := lfs_end_date - lfs_begin_date]
      sub_ohc_data[plcmt_begin_year == p_data_year & plcmt_end_year > p_data_year, 
                   lfs_ohc_days := as.Date(paste0(p_data_year, "-12-31")) - lfs_begin_date]
      
    #######################################
    # cast placements wide for each child #
    #######################################
      
      # # sort based on child id and placement start date
      # setorder(sub_ohc_data, child_id, lfs_end_date)
      # 
      # wide_set <- dcast.data.table(sub_ohc_data, child_id ~ placement_id, value.var = c("lfs_begin_date", "lfs_end_date", "lfs_ohc_days"))
      
    #################################
    # aggregate placements by child #
    #################################
      
      # aggregate placements by child
      agg_plcmt_by_child <- sub_ohc_data[, list(num_plcmt = .N,
                                                tot_plcmt_days = sum(lfs_ohc_days)),
                                         by = child_id]
      
      # if total placement days > 365, set to 365 #brule
      agg_plcmt_by_child[tot_plcmt_days > 365, tot_plcmt_days := 365]
    
    ###############################################################
    # subset to unique child id and merge with agg placement info #
    ###############################################################
      
      # sort based on child id and placement start date
      setorder(sub_ohc_data, child_id, -lfs_ohc_days)
      
      # subset to one row per child, keeping longest ohc placement #brule
      unique_child_set <- ea_no_dups(sub_ohc_data, "child_id")
    
      # keep only child specific vars #NEED TO KEEP THIRD FRIDAY FLAG
      unique_child_set <- subset(unique_child_set, select = c(child_id, child_dob, child_gender, child_race, child_ethnicity, child_hispanic, 
                                                              child_disability, disabilities, dcf_plcmt_type, fl_mntal_retardatn, fl_phys_disabled, 
                                                              fl_vis_hearing_impr, fl_emotion_dstrbd, fl_othr_spc_care, fl_lrn_disability, region, 
                                                              provider_county, removal_date, discharge_date, tpr_finalization_date, adoption_final_date))
      
      # merge aggregate placement info with child info
      full_set <- ea_merge(unique_child_set, agg_plcmt_by_child, "child_id", opt_print = 0)
      
      # add in year of data
      full_set[, dcf_year := p_data_year]
      ea_colorder(full_set, "dcf_year")
      
      # add file to out list
      file_list[[m_file]] <- full_set
    }

    ######################################
    # stack and return fule set of files #
    ######################################

    # stack all files
    out_file <- rbindlist(file_list, fill = TRUE, use.names = TRUE)

    # return the stacked file
    return(out_file)
  }

  # run function for full list of year files
  stacked_set <- func_format_stack(p_file_names)
  
##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write( , ".csv")
    
  }

