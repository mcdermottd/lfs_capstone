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
  opt_exp <- 0

#############
# load data #
#############
  
  # load stacked ohc data
  in_stacked_ohc <- fread("X:/LFS-Education Outcomes/data/lfs_data/stacked_dcf_set.csv")
  
  # load ed outcomes data
  in_stacked_dpi <- fread("X:/LFS-Education Outcomes/data/raw_data/DCFmatchedSample03012016.csv")

###################
# format dpi data #
###################
  
  # copy raw file
  format_stacked_dpi <- copy(in_stacked_dpi)
  
  # convert variable names to lowercase
  setnames(format_stacked_dpi, colnames(format_stacked_dpi), tolower(colnames(format_stacked_dpi)))
  
  # create dcf year variable
  format_stacked_dpi[, dcf_year := paste0("20", ea_scan(school_year, 2, "-"))]
  
  # rename child id var
  setnames(format_stacked_dpi, "child_id", "dpi_id")

##################################
# subset and merge with ohc data #
##################################
  
  # copy raw ohc file
  ohc_data_for_merge <- copy(in_stacked_ohc)
  
  # subset to dpi data to years in ohc file
  sub_dpi_for_merge <- subset(format_stacked_dpi, as.numeric(dcf_year) >= 2008 & as.numeric(dcf_year) <= 2012)
  
  # merge data sets
  merged_set <- ea_merge(ohc_data_for_merge, sub_dpi_for_merge, c("dpi_id", "dcf_year"), "x")
  
##########
# export #
##########

  # export
  if (opt_exp == 1) { 
    
    ea_write( , ".csv")
    
  }

