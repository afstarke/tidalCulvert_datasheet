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

# surveyHtCorrection ---------------------------------------------------------
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


# Extract longitudinal profile -----------------------------------------------
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
    rename(Substrate = `Sub-\r\nstrate`,
           shotCode = `Shot From (R/U/D)`,
           rawHeight = Height) %>%
    mutate(crossingID = crossingID,
           `Feature Code` = str_to_upper(`Feature Code`),
           Substrate = str_to_upper(Substrate),
           shotCode = str_to_upper(shotCode),
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

# Create nested culvert datafame -------------------------------------------------------
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
# Extract and calculate culvert cross-section height data  ------------------------------------------
#' crossSection
#' Extract cross sectional data of road and culvert structure 
#' Used with purrr::map functions to extract data into nested tibble
#' Second attempt by extracting calculated fields from Summary sheet. 
#' # IDEA: Think about how this would be integrated into future systems with using raw values.
#' 
#' @param filepath 
#' @param tidycells 
#'
#' @return
#' @export
#'
#' @examples
#' 
crossSection <- function(filepath, tidycells){
  # Set up variables for adjusting to NAVD88 with surveyHtCorrection()
  # constants for each crossing
  
  Lidarht <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'J54') %>% as.numeric()
  roadCentHt <- Lidarht
  # Road width used in the calculations of distance.
  roadWidth <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'J107') %>% as.numeric()
  # Turning point (TP) shots for use in adjusting elevations.
  TPforsight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y110') %>% as.numeric()
  TPbacksight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y111') %>% as.numeric()
  TPforsight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD110') %>% as.numeric()
  TPbacksight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD111') %>% as.numeric()
  
  # Backup to provide correct element types used in mutate_at
  crossvars_num <- c("US Height", "DS Height", "crossingID", "adjustedHtUS", "adjustedHtDS") 
  crossvars_chr <- c("Feature", "US cntrlPtcode", "DS cntrlPtcode")
  
  # Use read_xlsx to pull a cleaner table in (vs using culvert_extract())
  # Crossing Cross Section
  cross <- read_xlsx(path = filepath, sheet = 2, range = "R98:AH106", trim_ws = TRUE) %>% # Raw values
    as_tibble() %>% 
    rename(`US cntrlPtcode` = '..12', `DS cntrlPtcode` = '..17') %>%  
    select(-starts_with(".."))
  # roadCt <- read_xlsx(path = filepath, sheet = 2, range = "J106:M107") %>% # Raw values
  #   rename(Height = '..1', cntrlPtcode = '..4') %>%  
  #   select(-starts_with("..")) %>% mutate(Feature = "Road center", Position = "US")
  # Low Tide Water Elevations
  tide <- read_xlsx(path = filepath, sheet = 2, range = "P94:AH95") %>%
    rename(`US cntrlPtcode` = '..14', `DS cntrlPtcode` = '..19') %>% # label columns as control points.
    select(-starts_with(".."))  %>% 
    mutate_if(is.logical, .funs = ~as.numeric(.)) %>% #ensure nums are nums
    mutate(Feature = "Low Tide Water Elevation") %>% 
    mutate_at(crossvars_chr, .funs = ~as.character(.)) # ensure chars are chars
  cross <- rbind(cross, tide)
  
  # pull values from the longitudinal surveys to calculate Distances against.
  # NOTE: Use max and min to pull values from Long Profile table in case data was not entered into the US/DS invert dist cells.
  Longprofile <- channelLongidinalProfile_extract(filepath, tidycells)
  USinvert <- Longprofile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>%
    # filter(`Feature Code` == "I") %>% 
    filter(Distance == min(Distance)) %>% 
    mutate(Feature = "Invert", Position = "US") %>% 
    select(Feature, Position, Distance, adjustedHt)
  DSinvert <- Longprofile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>% # Arrg.... Some crossings use I as GCP...
    filter(Distance == max(Distance)) %>% 
    mutate(Feature = "Invert", Position = "DS") %>% 
    select(Feature, Position, Distance, adjustedHt)
  inverts <- rbind(USinvert, DSinvert) %>% 
    rename(NAVD_ht = adjustedHt)
  # TODO: Create individual calculations for the distances. PIA...
  # values for calculations of distance
  USInvdist <- USinvert %>% pull(Distance) %>% as.numeric() # Upstream invert distance
  DSInvdist <- DSinvert %>% pull(Distance) %>% as.numeric() # Downstream invert distance
  roadCentDist <- ((DSInvdist - USInvdist)/2) + USInvdist
  
  
    # values for calculations of distance
  USInv <- USinvert %>% pull(Distance)
  DSInv <- DSinvert %>% pull(Distance)
  # 
  # Insterted this munge code to simplify outputs to a tidy table of Feature-NAVD_ht-Distance for direct use in ggplot.
  # For RAW values remove and output just the cross object.
  cross %>% 
    gather(-Feature, key = measure, value = ht) %>% 
    separate(col = measure, into = c('position', 'k'), sep = " ") %>% 
    unite(col = Feature, Feature, position, sep ="_") %>% 
    spread(key = k, value = ht) %>% 
    separate(col = Feature, into = c("Feature", "Position"), sep = "_", extra = "merge", fill = "right") %>% 
    mutate(Height = as.numeric(Height), 
           Feature = str_trim(gsub(x = Feature, pattern = "[0-9]", replacement = ""))) %>% # remove marsh plain shot #s for later averaging
    group_by(Feature, Position) %>% 
    mutate(Height = mean(Height, na.rm = TRUE)) %>% 
    distinct(Feature, .keep_all = TRUE) %>% 
    mutate(adjustedHt = mean(surveyHtCorrection(rawHeight = Height, 
                                                shotCode = cntrlPtcode, 
                                                Lidarht = Lidarht, 
                                                roadCentHt = roadCentHt,
                                                TPforsight_upSt = TPforsight_upSt, 
                                                TPbacksight_upSt = TPbacksight_upSt,
                                                TPforsight_dwSt = TPforsight_dwSt, 
                                                TPbacksight_dwSt = TPbacksight_dwSt))) %>% 
    ungroup() %>% 
    add_row(Feature = "Road Center", adjustedHt = roadCentHt)
  
  
}
# use purrr magic to catch any issues. 
crossSection <- possibly(crossSection, otherwise = "Unable to fetch")


#
# cross section height and distances -----------------------------------------------
#' crossSectionHeights
#' Extract cross sectional data of road and culvert structure 
#' Used with purrr::map functions to extract data into nested tibble
#' Second attempt by extracting calculated fields from Summary sheet. 
#' # IDEA: Think about how this would be integrated into future systems with using raw values.
#' 
#' @param tidycells 
#' @param longitudinalProfile 
#' @param crossHeight 
#'
#' @return
#' @export
#'
#' @examples
#' 
calcHeights <- function(tidycellCol, longitudinalProfile, crossHeight){
  # Set up variables for adjusting to NAVD88 with surveyHtCorrection()
  # constants for each crossing
  
  # Road width used in the calculations of distance.
  roadWidth <- culvert_extract(tidycells = tidycellCol, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'V54') %>% as.numeric()
  # Pull invert distances from longitudinalProfiles
  
  
  
  
  # pull values from the longitudinal surveys to calculate Distances against ----
  # NOTE: Use max and min to pull values from Long Profile table in case data was not entered into the US/DS invert dist cells.
  # TODO: Need to add some sort of control for when only up or downstream inverts are found.
  
  USinvert <- longitudinalProfile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>%
    # filter(`Feature Code` == "I") %>% 
    filter(Distance == min(Distance)) %>% 
    mutate(Feature = "Invert", Position = "US") %>% 
    select(Feature, Position, Distance, adjustedHt)
  # USinvert <- if_else(nrow(USinvert) =< 1, NA, USinvert)
  
  DSinvert <- longitudinalProfile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>% # Arrg.... Some crossings use I as GCP...
    filter(Distance == max(Distance)) %>% 
    mutate(Feature = "Invert", Position = "DS") %>% 
    select(Feature, Position, Distance, adjustedHt)
  
  inverts <- rbind(USinvert, DSinvert) %>% 
    rename(NAVD_ht = adjustedHt)
  # TODO: Create individual calculations for the distances. PIA...
  # values for calculations of distance
  
  USInvdist <- if(exists("USinvert")) USinvert %>% pull(Distance) %>% as.numeric() else 999 # Upstream invert distance
  DSInvdist <- if(exists("DSinvert")) DSinvert %>% pull(Distance) %>% as.numeric() else 999 # Downstream invert distance
  roadCentDist <- ((DSInvdist - USInvdist)/2) + USInvdist
  
  #' distCalc
  #'
  #' @param feature 
  #' @param position 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  distCalc <- function(feature, position){
    # if(missing(feature)){
    #   feature = NA
    # }
    # if(missing(position)){
    #   position = NA
    # }
    code <- paste(feature, position, sep = "_")
    case_when(
      code == "Ceiling of Structure_US" ~  as.numeric(USInvdist),
      code == "Ceiling of Structure_DS" ~ as.numeric(DSInvdist),
      code == "HWI Stain_US" ~ as.numeric(USInvdist/2),
      code == "HWI Stain_DS" ~ as.numeric(DSInvdist/2),
      code == "HWI Wrack_US" ~ as.numeric(USInvdist + (USInvdist/2)),
      code == "HWI Wrack_DS" ~ as.numeric(DSInvdist),
      code == "Low Tide Water Elevation_US" ~ as.numeric((USInvdist/2) -(USInvdist/4)),
      code ==  "Low Tide Water Elevation_DS" ~ as.numeric(DSInvdist),
      code ==  "Marsh Plain Shot_US" ~ as.numeric((USInvdist/2) -(USInvdist/4)),
      code ==  "Marsh Plain Shot_DS" ~ as.numeric(DSInvdist),
      code ==  "Road Surface_US" ~ roadWidth, #as.numeric(roadCentDist - (roadWidth/2)),
      code ==  "Road Surface_DS" ~ as.numeric(roadCentDist + (roadWidth/2)),
      code ==  "Road Center_NA" ~ as.numeric(roadCentDist), # not in an up or downstream position....
      is.null(code) ~ 999,
      is.na(code) ~ 9999, 
      code == "NA_NA" ~ 99999
    )
    
  }
  # distCalc <- safely(distCalc)
  
  # return a sf with distances added in.
  crossHeight %>% mutate(Distance = distCalc(feature = Feature, position = Position)) 
  
  
}

