# Culvert tidy
# Read in tidy style.
# using functional programming

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
# TODO: The plan for this function is to more or less replicate the larger culvert_extract function set but only targeting the cross section data to simplfy the process. 

#' ---- channelLongidinalProfile_extract
#' Fetch longitudinal profile from a tidal crossing Assessment Workbook 
#' @param filepath A path to excel file containing Tidal Culvert datasheet.
#' @return tidy dataframe containing information relating to the cross sectional profile measured in field and recorded in M99:M102 and A122:M140
#'  
#'
#' @examples
#' 
#' 
# filepath <- list.files(tidalCulvert_datasheetsFolder, full.names = T)[[3]] 

channelLongidinalProfile_extract <- function(filepath){
  profile <- read.xlsx(filepath, sheetIndex = 2, rowIndex = 122:140, colIndex = 1:13)
  profile <- profile %>% select(Distance, Height, Feature.Code, ShotFrom = Shot.From..R.U.D.) %>% 
    filter(!is.na(Distance))
  profile
}

# ------------------------------------------------------------------------
# 

crossSection <- function(filepath){
  cross <- read.xlsx(filepath, sheetIndex = 2, rowIndex = 98:111, colIndex = 18:34)
  cross <- cross %>% filter(!is.na(Feature), Feature != "IF NEEDED:") %>% 
    select(1, 8, shotFromUS = 12, 13, shotFromDS = 17)
  cross
}

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
                                          # longProfile = map(.x = filePath, .f = possibly(channelLongidinalProfile_extract, otherwise = "HELP ME")),
                                          # crossSectionalProfile = map(.x = filePath, .f = crossSection)) 
  return(culvertFiles)
  
}

#
