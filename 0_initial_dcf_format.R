#####################################################################################################################################################
# notes:
# - purpose: perform initial, simple format of OHC files
# - inputs: full set of OHC excel files
# - outputs: corresponding CSV files
# - keywords:
# - general:
#####################################################################################################################################################

#######################################
# load packages and clear objects/log #
#######################################

  # load easimple and clear objects log
  library(easimple)
  ea_start()
  
  # load packages
  library(readxl)
  library(data.table)

#############
# set parms #
#############

  # store raw directory path
  p_dir_raw <- "X:/LFS-Education Outcomes/raw_data/dcf_excel/"
  
  # save names of files without file extension
  p_names_dcf <- list.files(p_dir_raw)
  p_names_dcf <- gsub(".xlsx", "", p_names_dcf)

  # output toggle
  p_opt_exp <- 1

#####################################
# load and stack state_teacher data #
#####################################

  # define function to open data
  func_open_dcf <- function(x_file) {
    
    # for file in path
    for (m_file in x_file) {
      
      print(m_file)
      
      # read in raw file
      raw_file <- data.table(read_excel(paste0(p_dir_raw, m_file, ".xlsx")))
      
      # set colnames to lowercase
      setnames(raw_file, colnames(raw_file), tolower(colnames(raw_file)))
      
      # remove NA rows from ohc data
      raw_file <- subset(raw_file, !is.na(child_id))
      
      # output formatted file
      if (p_opt_exp == 1) { ea_write(raw_file, paste0("X:/LFS-Education Outcomes/raw_data/", m_file, ".csv")) }
    }
  }
  
  # run function for full list of file paths in folder
  func_open_dcf(p_names_dcf)

