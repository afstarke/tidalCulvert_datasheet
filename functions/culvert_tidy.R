# Culvert tidy
# Read in tidy style.

# Read in files from specified folder
# Create tibble with columns of spreadsheet name, size, last modified, and list column with tidyxl 'cells' 
# Then use map and mutate functions to extract out the bits that are needed. 
# Can be done additively and transparently as more variables are desired.

# ---- culvert_fetch
#' Tidy up spread sheet contents using tidyxl for later gathering
#' and summarizing across several files. Function takes a file path and returns a 
#' tidyxl dataframe that can be used in downstream extraction of raw values.
#' 
#' @param filepath A path to excel file containing Tidal Culvert datasheet.
#' @return tidyxl dataframe of tidy cell contents of a given 
#' 
#' 
culvert_fetch <- function(filepath){
  cells <- tidyxl::xlsx_cells(filepath) %>%
    filter(sheet != "Data Sheet - BLANK") %>% 
    select(sheet, address, data_type:formula) %>% 
    mutate(date = as.character(date)) %>% # attempt at fixing issue with attributes being dropped by tidyr::gather
    gather(error:character, key = "dataType", value = "value") %>% 
    filter(!is.na(value)) %>% 
    mutate(same = ifelse(data_type == dataType, 1, 0)) 
  return(cells)
}

# ------------------------------------------------------------------------ 
# 
#' surveyHtCorrection
#'
#' @param rawHeight height as recorded in field.
#' @param shotCode control point code, R, U, D, X
#' @param Lidarht 
#' @param roadCentHt 
#' @param TPforsight_upSt 
#' @param TPbacksight_upSt 
#' @param TPforsight_dwSt 
#' @param TPbacksight_dwSt 
#'
#' @return numeric value of height corrected to NAVD88
#' @export
#'
#' @examples
#' 
surveyHtCorrection <- function(rawHeight, shotCode, Lidarht, roadCentHt, TPforsight_upSt, TPbacksight_upSt, TPforsight_dwSt, TPbacksight_dwSt){
  case_when(shotCode == "R" ~ (Lidarht + roadCentHt - rawHeight),
            shotCode == "U" ~ (Lidarht + roadCentHt - TPforsight_upSt + TPbacksight_upSt - rawHeight),
            shotCode == "D" ~ (Lidarht + roadCentHt - TPforsight_dwSt + TPbacksight_dwSt - rawHeight),
            shotCode == "X" ~ -9999, # missing data value for the 'X' code that I don't know how to deal with yet.
            is.na(shotCode) ~ -9999) # if missing shot code, it's likely missing other data.....
}

blankCheck <- function(df){
  if(dim(df)[1] == 0){
    NA
  }else{
    df %>% pull() %>% as.numeric()
  }
}

# ------------------------------------------------------------------------
#' ---- channelLongidinalProfile_extract
#' Fetch longitudinal profile from a tidal crossing Assessment Workbook 
#' @param filepath A path to excel file containing Tidal Culvert datasheet.
#' @return tidy dataframe containing information relating to the cross sectional
#'   profile measured in field and recorded in M99:M102 and A122:M140
#'  
#'
#' @examples
#' 
#' 
# filepath <- list.files(tidalCulvert_datasheetsFolder, full.names = T)[[3]] 

channelLongidinalProfile_extract <- function(filepath){
  # Set up variables for adjusting to NAVD88
  Lidarht <- read_xlsx(path = filepath, sheet = 3, range = "J54", col_names = FALSE) %>% pull() %>% as.numeric()
  roadCentHt <- read_xlsx(path = filepath, sheet = 2, range = "J107", col_names = FALSE) %>% pull() %>% as.numeric()
  TPforsight_upSt <- read_xlsx(path = filepath, sheet = 2, range = "Y110", col_names = FALSE) %>% pull() %>% as.numeric() 
  TPbacksight_upSt <- read_xlsx(path = filepath, sheet = 2, range = "Y111", col_names = FALSE) %>% pull() %>% as.numeric()
  TPforsight_dwSt <- read_xlsx(path = filepath, sheet = 2, range = "AD110", col_names = FALSE) %>% pull() %>% as.numeric()
  TPbacksight_dwSt <- read_xlsx(path = filepath, sheet = 2, range = "AD111", col_names = FALSE) %>% pull() %>% as.numeric()
  
  profile <- read_xlsx(path = filepath, sheet = 2, range = "A122:M140")
  profile <- profile %>% filter(!is.na(Distance)) %>% 
    select(-starts_with("..")) %>% 
    rename(Subsrate = `Sub-\r\nstrate`,
           shotCode = `Shot From (R/U/D)`,
           rawHeight = Height) %>% 
    mutate(adjustedHt = surveyHtCorrection(rawHeight = rawHeight, shotCode = shotCode, Lidarht = Lidarht, roadCentHt = roadCentHt, 
                                           TPforsight_upSt = TPforsight_upSt, TPbacksight_upSt = TPbacksight_upSt, 
                                           TPforsight_dwSt = TPforsight_dwSt, TPbacksight_dwSt = TPbacksight_dwSt))
      
  
  profile
}

# ------------------------------------------------------------------------
# 

crossSection <- function(filepath){
  Lidarht <- read_xlsx(path = filepath, sheet = 3, range = "J54", col_names = FALSE) %>% pull() %>% as.numeric()
  roadCentHt <- read_xlsx(path = filepath, sheet = 2, range = "J107", col_names = FALSE) %>% pull() %>% as.numeric()
  TPforsight_upSt <- read_xlsx(path = filepath, sheet = 2, range = "Y110", col_names = FALSE) %>% pull() %>% as.numeric() 
  TPbacksight_upSt <- read_xlsx(path = filepath, sheet = 2, range = "Y111", col_names = FALSE) %>% pull() %>% as.numeric()
  TPforsight_dwSt <- read_xlsx(path = filepath, sheet = 2, range = "AD110", col_names = FALSE) %>% pull() %>% as.numeric()
  TPbacksight_dwSt <- read_xlsx(path = filepath, sheet = 2, range = "AD111", col_names = FALSE) %>% pull() %>% as.numeric()
  
  cross <- read_xlsx(path = filepath, sheet = 2, range = "R98:AH106", trim_ws = TRUE)
  cross <- cross %>% rename(cntrlPtcode_US = '..12', cntrlPtcode_DS = '..17') %>% 
    select(-starts_with("..")) %>% 
    mutate(adjustedHtUS = surveyHtCorrection(rawHeight = `US Height`, shotCode = cntrlPtcode_US, Lidarht = Lidarht, roadCentHt = roadCentHt, 
                                           TPforsight_upSt = TPforsight_upSt, TPbacksight_upSt = TPbacksight_upSt, 
                                           TPforsight_dwSt = TPforsight_dwSt, TPbacksight_dwSt = TPbacksight_dwSt),
           adjustedHtDS = surveyHtCorrection(rawHeight = `DS Height`, shotCode = cntrlPtcode_DS, Lidarht = Lidarht, roadCentHt = roadCentHt, 
                                             TPforsight_upSt = TPforsight_upSt, TPbacksight_upSt = TPbacksight_upSt, 
                                             TPforsight_dwSt = TPforsight_dwSt, TPbacksight_dwSt = TPbacksight_dwSt))
  cross

  }
# TODO: Add the height sections from the functions above into the data extraction culvert Tidy function below.
# ---- culvert_tidy
#' Create a tidy (nested)data frame of tidal culvert datasheets with 
#' a column containing tidyxl dataframes for each cooresponding file.
#' 
#' @param folder A path to the folder containing Tidal Culvert datasheets.
#' @return dataframe of file information along with a  column of tidy cell contents
#' from tidyxl.
#' 
#'
#'
culvert_tidy <- function(folder){
  
  culvertFolder <- file.path(folder)
  
  culvertFiles <- list.files(culvertFolder) %>% as.tibble()# List files in provided folder
  culvertFiles <- culvertFiles %>% rename(filenames = value) # simple rename
  culvertFiles <- culvertFiles %>% 
    mutate(lastChanges = as.POSIXct(map_dbl(.x = filenames, 
                                            .f = ~file.info(paste(folder, .x, sep = "/"))$mtime), 
                                    origin = "1970-01-01"),
                                          filePath = paste(folder, filenames, sep = "/")) # create a value for last changes to file for QA. 
  culvertFiles <- culvertFiles %>% mutate(tidycells = map(.x = filePath, .f = culvert_fetch))#, # For each file in the table, 'culvert_fetch' the data.
                                          
  return(culvertFiles)
  
}

#
