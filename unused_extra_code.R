

###############################################
# initial format and remove duplicate entries #
###############################################
  
  # create list of vars to check on duplicates
  vars_dup <- colnames(sub_ohc_data)
  vars_nocheck <- c("site_region", "case_type", "child_person_type", "child_level_of_need", "provider_loc", "plcmt_with_sibling",
                    "days_plcmt_in_rpt_period")
  vars_dup <- setdiff(vars_dup, vars_nocheck)

  # subset based on duplicate vars list #brule
  sub_ohc_data <- ea_no_dups(sub_ohc_data, vars_dup)

  # sort based on child id, placement end date, and discharge variables (reverse sort discharge so "yes" appears first)
  setorder(sub_ohc_data, child_id, plcmt_end_date, -discharge, discharge_reason, end_reason)

  # remove discharge vars from no dup check
  vars_dup <- setdiff(vars_dup, c("discharge", "discharge_reason", "end_reason"))

  # remove additional duplicates ignoring discharge vars #brule
  sub_ohc_data <- ea_no_dups(sub_ohc_data, vars_dup)
   
################################
# create additional child vars #
################################

  # create gender female flag
  stacked_set[, flag_female := 0]
  stacked_set[child_gender == "Female", flag_female := 1]

  # create frequency of race / hispanic combinations
  a_freq_race <- ea_table(stacked_set, c("child_race", "child_hispanic"), opt_percent = 1)

  # create race flags #brule
  stacked_set[, c("flag_amer_indian", "flag_asian", "flag_black", "flag_pac_island", "flag_white", "flag_other") := 0]
  stacked_set[child_race == "American Indian/Alaskan Native", flag_amer_indian := 1]
  stacked_set[child_race == "Asian", flag_asian := 1]
  stacked_set[child_race == "Black/African American", flag_black := 1]
  stacked_set[child_race == "Native Hawaiian/Other Pacific Islande", flag_pac_island := 1]
  stacked_set[child_race == "White", flag_white := 1]
  stacked_set[child_race == "Unable to Determine" | is.na(child_race), flag_other := 1]

  # create hispanic flag
  stacked_set[, flag_hispanic := 0]
  stacked_set[child_hispanic == "Y", flag_hispanic := 1]

  # create disability flag
  stacked_set[, flag_disability := 0]
  stacked_set[child_disability == "Y", flag_disability := 1]

#####################################
# fill in missing placement regions #
#####################################

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
  
#########################################
# create placement dummies for analysis #
#########################################
  
  # create placement type dummies
  stacked_set[, c("flag_fhome_rel", "flag_fhome_nrel", "flag_ghome", "flag_rcc", "flag_plcmt_other") := 0]
  stacked_set[dcf_plcmt_type == "Foster Home (Relative)", flag_fhome_rel := 1]
  stacked_set[dcf_plcmt_type == "Foster Home (Non-Relative)", flag_fhome_nrel := 1]
  stacked_set[dcf_plcmt_type == "Group Home", flag_ghome := 1]
  stacked_set[dcf_plcmt_type == "RCC", flag_rcc := 1]
  stacked_set[is.na(dcf_plcmt_type), flag_plcmt_other := 1]
  
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

#########
# plots #
#########
  
  # histogram - avg plcmt length in acad year, by academic year (15 day bins, <= 750 days)
  plot_hist_avg_days_acad_byr <- ggplot(data = subset(sub_ohc_acad_yr, acad_year > 2007 & acad_year < 2013), aes(x = avg_days_per_plcmt)) + 
                                         geom_histogram(binwidth = 10, colour = "black", fill = "dodgerblue4") +
                                         labs(x = "Average Days Per Placement", y = "Number of Children", 
                                              title = "Average Days Per Out of Home Care Placement Per Year \n - by Academic Year") + 
                                         plot_attributes +
                                         facet_wrap(~acad_year, ncol = 2)

