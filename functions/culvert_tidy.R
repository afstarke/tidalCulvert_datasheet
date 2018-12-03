# Culvert tidy
# Read in tidy style.
# using functional programming

# Read in files from specified folder
# Create tibble with columns of spreadsheet name, size, last modified, and list column with tidyxl 'cells' 
# Then use map and mutate functions to extract out the bits that are needed. 
# Can be done additively and transparently as more variables are desired.

# ---- culvert_fetch
#' Tidy up spread sheet contents using tidyxl for later gathering
#' and summarizing across several files.
#' 
#' @param filepath A path to excel file containing Tidal Culvert datasheet.
#' @return tidyxl dataframe of tidy cell contents of a given 
#' 
#' 
culvert_fetch <- function(filepath){
  cells <- xlsx_cells(filepath) %>% 
  filter(sheet != "Data Sheet - BLANK") %>% 
    select(sheet, address, data_type:character) %>% 
    gather(error:character, key = "dataType", value = "value") %>% 
    filter(!is.na(value)) %>% 
    mutate(same = ifelse(data_type == dataType, 1, 0)) 
  return(cells)
}


#' ---- crossSection_fetch
#' Fetch Crossing cross section and longitudinal profile from a tidal crossing Assessment Workbook 
#' @param filepath A path to excel file containing Tidal Culvert datasheet.
#' @return tidy dataframe containing information relating to the cross sectional profile measured in field and recorded in M99:M102 and A122:M140
#'  
#'
#' @examples
crossProfile_extract <- function(filepath){
  
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
  
  culvertFiles <- list.files(culvertFolder) %>% as.tibble()
  culvertFiles <- culvertFiles %>% rename(filenames = value)
  culvertFiles <- culvertFiles %>% mutate(lastChanges = as.POSIXct(map_dbl(.x = filenames, .f = ~file.info(paste(folder, .x, sep = "/"))$mtime), origin = "1970-01-01"),
                                          filePath = paste(folder, filenames, sep = "/"))
  culvertFiles <- culvertFiles %>% mutate(tidycells = map(.x = filePath, .f = culvert_fetch)) 
  return(culvertFiles)
  
}

#
