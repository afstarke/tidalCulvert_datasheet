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
#' @param Lidarht lidar height as obtained using LiDAR data
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
  case_when(shotCode == "R" ~ as.numeric(sum(Lidarht, roadCentHt)) + -as.numeric(rawHeight),
            shotCode == "U" ~ as.numeric(sum(Lidarht, roadCentHt)) + -as.numeric(TPforsight_upSt) + as.numeric(TPbacksight_upSt) + -as.numeric(rawHeight),
            shotCode == "D" ~ as.numeric(sum(Lidarht, roadCentHt)) + -as.numeric(TPforsight_dwSt) + as.numeric(TPbacksight_dwSt) + -as.numeric(rawHeight),
            shotCode == "X" ~ as.numeric(NA), # missing data value for the 'X' code that I don't know how to deal with yet.
            is.na(shotCode) ~ as.numeric(NA)) # if missing shot code, it's likely missing other data.....
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


channelLongidinalProfile_extract <- function(filepath, tidycells){
  # Set up variables for adjusting to NAVD88 with surveyHtCorrection()
  crossingID <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'L7') %>% as.numeric()
  Lidarht <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'J54') %>% as.numeric()
  roadCentHt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'J107') %>% as.numeric()
  
  TPforsight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y110') %>% as.numeric()
  TPbacksight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y111') %>% as.numeric()
  TPforsight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD110') %>% as.numeric()
  TPbacksight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD111') %>% as.numeric()

  profile <- read_xlsx(path = filepath, sheet = 2, range = "A122:M140")
  profile <- profile %>% filter(!is.na(Distance)) %>%
    select(-starts_with("..")) %>%
    rename(Subsrate = `Sub-\r\nstrate`,
           shotCode = `Shot From (R/U/D)`,
           rawHeight = Height) %>%
    mutate(crossingID = crossingID,
           adjustedHt = surveyHtCorrection(rawHeight = rawHeight, 
                                           shotCode = shotCode, 
                                           Lidarht = Lidarht, 
                                           roadCentHt = roadCentHt,
                                           TPforsight_upSt = TPforsight_upSt, 
                                           TPbacksight_upSt = TPbacksight_upSt,
                                           TPforsight_dwSt = TPforsight_dwSt, 
                                           TPbacksight_dwSt = TPbacksight_dwSt))


  profile
}
# ------------------------------------------------------------------------
# 

crossSection <- function(filepath, tidycells){
  crossingID <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'L7') %>% as.numeric()
  Lidarht <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'J54') %>% as.numeric()
  roadCentHt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'V54') %>% as.numeric()
  
  roadWidth <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'J107') %>% as.numeric()
  
  TPforsight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y110') %>% as.numeric()
  TPbacksight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y111') %>% as.numeric()
  TPforsight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD110') %>% as.numeric()
  TPbacksight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD111') %>% as.numeric()
  
  crossvars_num <- c("US Height", "DS Height", "crossingID", "adjustedHtUS", "adjustedHtDS")
  crossvars_chr <- c("Feature", "cntrlPtcode_US", "cntrlPtcode_DS")
  cross <- read_xlsx(path = filepath, sheet = 2, range = "R98:AH106", trim_ws = TRUE)
  tide <- read_xlsx(path = filepath, sheet = 2, range = "P94:AH95") %>%
    rename(cntrlPtcode_US = '..14', cntrlPtcode_DS = '..19') %>%
    select(-starts_with(".."))  %>% 
    mutate_if(is.logical, .funs = ~as.numeric(.)) %>% #ensure nums are nums
    mutate(Feature = "Low Tide Water Elevation") %>% 
    mutate_at(crossvars_chr, .funs = ~as.character(.)) # ensure chars are chars
  # pull values from the longitudinal surveys to calculate Distances against.
  # NOTE: Use max and min to pull values from Long Profile table in case data was not entered into the US/DS invert dist cells.
  Longprofile <- channelLongidinalProfile_extract(filepath, tidycells)
    USinvert <- Longprofile %>% filter(`Feature Code` == "I") %>% 
      filter(Distance == min(Distance)) %>% mutate(Feature = "Invert", Position = "US") %>% select(Feature, Position, Distance, adjustedHt)
    DSinvert <- Longprofile %>% filter(`Feature Code` == "I") %>% 
      filter(Distance == max(Distance)) %>% mutate(Feature = "Invert", Position = "DS") %>% select(Feature, Position, Distance, adjustedHt)
    inverts <- rbind(USinvert, DSinvert) %>% rename(NAVD_ht = adjustedHt)
    # TODO: Create tibble with featureposition, NAVDht and distances to be joined later.
   cross %>% rename(cntrlPtcode_US = '..12', cntrlPtcode_DS = '..17') %>% 
    select(-starts_with("..")) %>% 
    rbind(tide) %>% #TODO: Fix this bug in joining the tide elevation data to the cross-section data. Fixed by using rbind. bind_rows was throwing error due to class type not consistent across sets of data.
    mutate(crossingID = crossingID, 
           Feature = gsub(x = Feature, pattern = "[0-9]", replacement = ""), # remove marsh plain shot #s for later averaging
           adjustedHtUS = surveyHtCorrection(rawHeight = `US Height`, 
                                           shotCode = cntrlPtcode_US, 
                                           Lidarht = Lidarht, 
                                           roadCentHt = roadCentHt,
                                           TPforsight_upSt = TPforsight_upSt, 
                                           TPbacksight_upSt = TPbacksight_upSt,
                                           TPforsight_dwSt = TPforsight_dwSt, 
                                           TPbacksight_dwSt = TPbacksight_dwSt),
           adjustedHtDS = surveyHtCorrection(rawHeight = `DS Height`, 
                                           shotCode = cntrlPtcode_DS, 
                                           Lidarht = Lidarht, 
                                           roadCentHt = roadCentHt,
                                           TPforsight_upSt = TPforsight_upSt, 
                                           TPbacksight_upSt = TPbacksight_upSt,
                                           TPforsight_dwSt = TPforsight_dwSt, 
                                           TPbacksight_dwSt = TPbacksight_dwSt)) %>% 
    mutate_at(crossvars_num, .funs = ~as.numeric(.)) %>% 
    mutate_at(crossvars_chr, .funs = ~as.character(.)) %>% 
  
  # values for calculations of distance
  # USInv <- USinvert %>% pull(Distance)
  # DSInv <- DSinvert %>% pull(Distance)
  # 
  # Insterted this munge code to simplify outputs to a tidy table of Feature-NAVD_ht-Distance for direct use in ggplot.
  # For RAW values remove and output just the cross object.
 # cross %>% 
   select(Feature, adjustedHtUS, adjustedHtDS) %>%
    gather(key = k, value = NAVD_ht, -Feature) %>%
    separate(col = k, sep = "ed", into = c(NA, "ht")) %>%
    unite(col = "FeaturePosition", Feature, ht, remove = FALSE) %>% group_by(FeaturePosition) %>%
    summarize(NAVD_ht = mean(NAVD_ht, na.rm = TRUE)) %>%
    separate(col = FeaturePosition, sep = "_Ht", into = c("Feature", "Position")) #%>%  # section off Up and Down stream.
    # mutate(Distance = NA) %>%
    # rbind(inverts) #%>%
    # add_row(Feature = "Road Center", Position = "US", NAVD_ht = roadCentHt, Distance = (((DSInv - USInv)/2) + USInv)) 


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
