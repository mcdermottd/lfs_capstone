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

#################################
# add additional placement vars #
#################################
  
  # change var names to lowercase
  setnames(ohc_data_full, colnames(ohc_data_full), tolower(colnames(ohc_data_full)))
  
  # change dates to date type vars
  ohc_data_full[, 37:43] <- lapply(ohc_data_full[, 37:43, with = FALSE], function(x_var) {as.Date(x_var, "%Y-%m-%d")})

  # create begin and end years (ohc and specific placement)
  ohc_data_full[, ohc_begin_year := as.numeric(format(removal_date, "%Y"))]
  ohc_data_full[, ohc_end_year := as.numeric(format(discharge_date, "%Y"))]
  ohc_data_full[, plcmt_begin_year := as.numeric(format(plcmt_begin_date, "%Y"))]
  ohc_data_full[, plcmt_end_year := as.numeric(format(plcmt_end_date, "%Y"))]

  # calc number of ohc days and plcmt days 
  ohc_data_full[, tot_ohc_days := discharge_date - removal_date]
  ohc_data_full[, plcmt_days := plcmt_end_date - plcmt_begin_date]

###################################################
# remove placements with the same start/end dates #
###################################################
  
  # sort based on child id, placement dates, and end reason (end reason = NA appears last) #brule
  setorder(ohc_data_full, child_id, plcmt_begin_date, plcmt_end_date, end_reason, na.last = TRUE)
  
  # remove duplicates based on id, and placement start and end date #brule
  ohc_data_no_dups <- ea_no_dups(ohc_data_full, c("child_id", "plcmt_begin_date", "plcmt_end_date"))   

#################################
# aggregate placements by child #
#################################
  
  # aggregate placements by child
  agg_plcmt_by_child <- ohc_data_no_dups[, list(tot_num_plcmts = .N,
                                                tot_plcmt_days = sum(plcmt_days)),
                                         by = "child_id"]
  
  # take average number of placement and total placement days
  a_avg_plcmts <- agg_plcmt_by_child[, list(avg_plcmts = mean(tot_num_plcmts),
                                            avg_plcmt_days = mean(tot_plcmt_days, na.rm = TRUE))]

##########################
# subset to current year #
##########################
  
  # copy formatted set
  sub_ohc_data <- copy(format_ohc_data)
  
  # create flag for in placement year #brule
  sub_ohc_data[, flag_in_year := 0]
  sub_ohc_data[plcmt_begin_year <= p_data_year & plcmt_end_year >= p_data_year, flag_in_year := 1]
  
  # subset to placements from current year #brule
  sub_ohc_data <- subset(sub_ohc_data, flag_in_year == 1)
    
###############################################################
# subset to unique child id and merge with agg placement info #
###############################################################
  
  # sort based on child id and placement start date
  setorder(sub_ohc_data, child_id, -lfs_ohc_days)
  
  # subset to one row per child, keeping longest ohc placement #brule
  unique_child_set <- ea_no_dups(sub_ohc_data, "child_id")

  # keep only child specific vars #NEED TO KEEP THIRD FRIDAY FLAG
  unique_child_set <- subset(unique_child_set, select = c(child_id, child_dob, child_gender, child_race, child_ethnicity, child_hispanic, 
                                                          child_disability, disabilities, dcf_plcmt_type, region, provider_county, ohc_begin_year,
                                                          ohc_end_year, removal_date, discharge_date, tpr_finalization_date, adoption_final_date))
  
  # merge aggregate placement info with child info
  full_set <- ea_merge(unique_child_set, agg_plcmt_by_child, "child_id", opt_print = 0)
  
  # add in year of data
  full_set[, dcf_year := p_data_year]
  ea_colorder(full_set, "dcf_year")
  

  
  
      

      

  
######################
# merge with id vars #
######################
  
  # change id var names for merge
  setnames(p_id_xwalk, c("new_id", "CHILD_ID"), c("child_id", "dpi_id"))
  
  # rename 
  
  # remove duplicates on child_id #brule
  p_id_xwalk <- ea_no_dups(p_id_xwalk, "child_id")
  
  # merge on xwalk ids
  out_set <- ea_merge(stacked_set, p_id_xwalk, "child_id", "x", opt_print = 0)
  
  # reorder cols
  ea_colorder(out_set, c("dcf_year", "child_id", "dpi_id"))

##########
# export #
##########

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(out_set, "X:/LFS-Education Outcomes/data/lfs_data/stacked_dcf_set.csv")
    
  }

