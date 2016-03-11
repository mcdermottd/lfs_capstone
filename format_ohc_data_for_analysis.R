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
  
  # output toggle
  p_opt_exp <- 0
  
  # load id xwalk
  p_id_xwalk <- fread("X:/LFS-Education Outcomes/data/raw_data/xwalk_child_id.csv")
  
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
                                                              child_disability, disabilities, dcf_plcmt_type, region, provider_county, removal_date,
                                                              discharge_date, tpr_finalization_date, adoption_final_date))
      
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

################################
# create additional child vars #
################################
  
  # # create gender female flag
  # stacked_set[, flag_female := 0]
  # stacked_set[child_gender == "Female", flag_female := 1]
  # 
  # # create frequency of race / hispanic combinations
  # a_freq_race <- ea_table(stacked_set, c("child_race", "child_hispanic"), opt_percent = 1)
  # 
  # # create race flags #brule
  # stacked_set[, c("flag_amer_indian", "flag_asian", "flag_black", "flag_pac_island", "flag_white", "flag_other") := 0]
  # stacked_set[child_race == "American Indian/Alaskan Native", flag_amer_indian := 1]
  # stacked_set[child_race == "Asian", flag_asian := 1]
  # stacked_set[child_race == "Black/African American", flag_black := 1]
  # stacked_set[child_race == "Native Hawaiian/Other Pacific Islande", flag_pac_island := 1]
  # stacked_set[child_race == "White", flag_white := 1]
  # stacked_set[child_race == "Unable to Determine" | is.na(child_race), flag_other := 1]
  # 
  # # create hispanic flag
  # stacked_set[, flag_hispanic := 0]
  # stacked_set[child_hispanic == "Y", flag_hispanic := 1]
  # 
  # # create disability flag
  # stacked_set[, flag_disability := 0]
  # stacked_set[child_disability == "Y", flag_disability := 1]

##############################
# create additional ohc vars #
##############################
  
  # create ohc begin and end years
  stacked_set[, ohc_begin_year := as.numeric(ea_scan(removal_date, 1, "-"))]
  stacked_set[, ohc_end_year := as.numeric(ea_scan(discharge_date, 1, "-"))]
  
  # create placement type dummies
  stacked_set[, c("flag_fhome_rel", "flag_fhome_nrel", "flag_ghome", "flag_rcc", "flag_plcmt_other") := 0]
  stacked_set[dcf_plcmt_type == "Foster Home (Relative)", flag_fhome_rel := 1]
  stacked_set[dcf_plcmt_type == "Foster Home (Non-Relative)", flag_fhome_nrel := 1]
  stacked_set[dcf_plcmt_type == "Group Home", flag_ghome := 1]
  stacked_set[dcf_plcmt_type == "RCC", flag_rcc := 1]
  stacked_set[is.na(dcf_plcmt_type), flag_plcmt_other := 1]
  
  # create region strings
  p_reg_nc <- c("Vilas", "Oneida", "Forest", "Lincoln", "Langlade", "Marathon", "Wood", "Portage", "Adams")
  p_reg_ne <- c("Florence", "Marinette", "Menominee", "Oconto", "Shawano", "Waupaca", "Outagamie", "Brown", "Kewaunee", "Door", "Waushara", 
                "Winnebago", "Calumet", "Manitowoc", "Green Lake", "Fond Du Lac", "Sheboygan")
  p_reg_nw <- c("Bayfield", "Ashland", "Iron", "Sawyer", "Price", "Taylor", "Douglas", "Burnett", "Washburn", "Polk", "Barron", "Rusk")
  p_reg_se <- c("Walworth", "Racine", "Kenosha")
  p_reg_s <- c("Marquette", "Richland", "Sauk", "Columbia", "Dodge", "Washington", "Ozaukee", "Grant", "Iowa", "Dane", "Jefferson", "Waukesha", 
               "Lafayette", "Green", "Rock")
  p_reg_w <- c("St Croix", "Dunn", "Chippewa", "Pierce", "Pepin", "Eau Claire", "Clark", "Buffalo", "Trempealeau", "Jackson", "La Crosse", 
               "Monroe", "Juneau", "Vernon", "Crawford")

  # create regions for missings #brule
  stacked_set[is.na(region) & provider_county %in% p_reg_nc, region := "Northcentral"]
  stacked_set[is.na(region) & provider_county %in% p_reg_ne, region := "Northeast"]
  stacked_set[is.na(region) & provider_county %in% p_reg_nw, region := "Northwest"]
  stacked_set[is.na(region) & provider_county %in% p_reg_se, region := "Southeast"]
  stacked_set[is.na(region) & provider_county %in% p_reg_s, region := "South"]
  stacked_set[is.na(region) & provider_county %in% p_reg_w, region := "West"]
  stacked_set[is.na(region) & provider_county == "Milwaukee", region := "Milwaukee"]

  # create region dummies
  stacked_set[, c("flag_reg_mke", "flag_reg_nc", "flag_reg_ne", "flag_reg_nw", "flag_reg_s", "flag_reg_se", "flag_reg_w", "flag_reg_other") := 0]
  stacked_set[region == "Milwaukee", flag_reg_mke := 1]
  stacked_set[region == "Northcentral", flag_reg_nc := 1]
  stacked_set[region == "Northeast", flag_reg_ne := 1]
  stacked_set[region == "Northwest", flag_reg_nw := 1]
  stacked_set[region == "South", flag_reg_s := 1]
  stacked_set[region == "Southeast", flag_reg_se := 1]
  stacked_set[region == "West", flag_reg_w := 1]
  stacked_set[is.na(region), flag_reg_other := 1]
  
  
######################
# merge with id vars #
######################
  
  # change id var names for merge
  setnames(p_id_xwalk, c("new_id", "CHILD_ID"), c("child_id", "dpi_id"))
  
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

