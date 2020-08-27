# Culvert tidy
# Read in tidy style using nested dataframes.

# Read in files from specified folder
# Create tibble with columns of spreadsheet name, size, last modified, and list column with tidyxl 'cells'
# Then use map and mutate functions to extract out the bits that are needed.
# Can be done additively as more variables are desired.

# ---- culvert_fetch
#' Tidy up spread sheet contents using tidyxl for later gathering
#' and summarizing across several files. Function takes a file path and returns a
#' tidyxl dataframe that can be used in downstream extraction of raw values.
#'
#' @param filepath A path to excel file containing Tidal Culvert datasheet.
#' @return tidyxl dataframe of tidy cell contents of a given
#'
#'
culvert_fetch <- function(filepath) {
  cellNeeds <-
    c("I164",
      "I165",
      "I169",
      "P164",
      "P165",
      "P169",
      "W164",
      "W165",
      "W169")
  cells <- tidyxl::xlsx_cells(filepath) %>%
    filter(sheet != "Data Sheet - BLANK") %>%
    select(sheet, address, data_type:formula) %>%
    mutate(date = as.character(date)) %>% # attempt at fixing issue with attributes being dropped by tidyr::gather
    gather(error:character, key = "dataType", value = "value") %>%
    filter(!is.na(value) |
             address %in% cellNeeds &
             dataType == "logical") %>% # a set of cells are needed that when blank in the data are filtered out here.
    mutate(same = ifelse(data_type == dataType, 1, 0))
  return(cells)
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
culvert_tidy <- function(folder) {
  culvertFolder <- file.path(folder)
  
  culvertFiles <-
    list.files(culvertFolder) %>% as.tibble()# List files in provided folder
  culvertFiles <-
    culvertFiles %>% rename(filenames = value) # simple rename
  culvertFiles <- culvertFiles %>%
    mutate(
      lastChanges = as.POSIXct(map_dbl(
        .x = filenames,
        .f = ~ file.info(paste(folder, .x, sep = "/"))$mtime
      ),
      origin = "1970-01-01"),
      filePath = paste(folder, filenames, sep = "/")
    ) # create a value for last changes to file for QA.
  culvertFiles <-
    culvertFiles %>% mutate(tidycells = map(.x = filePath, .f = culvert_fetch))#, # For each file in the table, 'culvert_fetch' the data.
  
  return(culvertFiles)
  
}


# Clean up field data ----
#' Clean and tidy up field collected data
#'
#' @param decodedcolumn
#'
#' @return
#' @export
#'
#' @examples
cleanField <- function(decodedcolumn) {
  tmp <- decodedcolumn %>%
    unnest() %>%
    select(dataName, values) %>%
    spread(key = dataName, value = values) %>%
    select(crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
    # Add in new 'cleaning methods' here as needed.
    mutate(
      dateAssessed = lubridate::as_date(dateAssessed, "%M-%D-%Y"),
      AsmtStartTime = lubridate::ymd_hms(paste0(dateAssessed, AsmtStartTime %>% str_sub(start = 12, end = 20)), tz = 'EST'),
      AsmtEndTime = lubridate::ymd_hms(paste0(dateAssessed, AsmtEndTime %>% str_sub(start = 12, end = 20)), tz = 'EST'),
      crossingID = as.numeric(crossingID),
      TidePredictTimeHigh = lubridate::ymd_hms(paste0(dateAssessed, TidePredictTimeHigh %>% str_sub(start = 12, end = 20)), tz = 'EST'),
      TidePredictTimeLow = lubridate::ymd_hms(paste0(dateAssessed, TidePredictTimeLow %>% str_sub(start = 12, end = 20)), tz = 'EST')
    ) %>%
    mutate_if(is.character, list( ~ na_if(., "N/A"))) %>%   # Convert character columns with "N/A" to NA
    mutate_at(numericVars, as.numeric) %>%
    mutate_at(logicalVars, as.logical)
  # munge the vegetation matrix into a selection list within one column.
  tmp2 <- tmp %>%
    select(crossingID, starts_with("veg")) %>%
    gather(-crossingID, key = 'key', value = "val") %>%
    separate(col = key, into = c("key", "Vegchoice")) %>%
    mutate(VegetMat_select = ifelse(val, yes = val, no = NA)) %>%
    filter(!is.na(VegetMat_select)) %>% select(crossingID, Vegchoice)
 
  tmp %>% left_join(tmp2) %>% select(-starts_with("vegMat"))
  
}


# surveyHtCorrection ---------------------------------------------------------
#
#' surveyHtCorrection takes the raw elevation value, as collected in the field
#' with a laser level/rod/transit configuration and corrects it to NAVD88 datum
#' referenced from LIDAR (lidar-derived DEM). Important note is that all values
#' are referenced from road center height so accuracy of supplied road elevation
#' is critical for accurate measures. This also assumes that LIDAR values are 
#' accuratly sampled from the same location as taken in the field.
#'
#' @param rawHeight height as recorded in field.
#' @param shotCode control point code, R, U, D, X
#' @param Lidarht lidar height as obtained using LiDAR data from desktop assessment
#' @param roadCentHt Road center height measured in field.
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
surveyHtCorrection <-
  function(rawHeight,
           shotCode,
           Lidarht,
           roadCentHt,
           TPforsight_upSt,
           TPbacksight_upSt,
           TPforsight_dwSt,
           TPbacksight_dwSt) {
    case_when(
      shotCode == "R" ~ as.numeric(sum(Lidarht, roadCentHt, na.rm = T)) +-as.numeric(rawHeight),
      shotCode == "U" ~ as.numeric(sum(Lidarht, roadCentHt, na.rm = T)) +-as.numeric(TPforsight_upSt) + as.numeric(TPbacksight_upSt) +-as.numeric(rawHeight),
      shotCode == "D" ~ as.numeric(sum(Lidarht, roadCentHt, na.rm = T)) +-as.numeric(TPforsight_dwSt) + as.numeric(TPbacksight_dwSt) +-as.numeric(rawHeight),
      shotCode == "X" ~ as.numeric(NA),
      # missing data value for the 'X' code that I don't know how to deal with yet.
      is.na(shotCode) ~ as.numeric(NA)
    ) # if missing shot code, it's likely missing other data.
  }


# Extract longitudinal profile -----------------------------------------------
#' ---- channelLongidinalProfile_extract
#' Fetch longitudinal profile from a tidal crossing Assessment Workbook
#' This data is in found in the field worksheets under the "Data Sheet - SITE" tab from A124:M140.
#' The number of data points varies depending on how many points have been surveyed.
#'
#' @param filepath A path to excel file containing Tidal Culvert datasheet.
#' @param tidycells column containing the tidycells from the datasheet extraction process.
#' @param lidarHt Road center height (NAVD88) collected in desktop assessment, stored in GIS.
#'
#' @return tidy dataframe containing the cross sectional heights and distances at specific features
#' data include:
#' Distance: (numeric) Distance from upstream
#' Height: (numeric) Decimal feet (in this survey protocol) as recorded in the field with a laser level transit/rod setup.
#' Feature Code: Coded value
#' @examples
#'

#'
# filepath <- list.files(tidalCulvert_datasheetsFolder, full.names = T)[[3]]

channelLongidinalProfile_extract <-
  function(filepath, tidycells, lidarHts) {
    # Set up variables for adjusting to NAVD88 with surveyHtCorrection()
    # # DEBUGGING
    # filepath <- wtf$filePath[[1]]
    # tidycells <- wtf$tidycells[[1]]
    # lidarHts <- wtf$da_LiDarHt_CL[[1]]
    #
    crossingID <-
      culvert_extract(tidycells = tidycells,
                      sheetOI = 'Data Sheet - SITE',
                      celladdress = 'L7') %>% as.numeric()
    Lidarht <- lidarHts # Swap in the desktop collected values.
    # Lidarht <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'J54') %>% as.numeric()
    # Road center measured in the field- pre-corrected value.
    roadCentHt <-
      culvert_extract(tidycells = tidycells,
                      sheetOI = 'Data Sheet - SITE',
                      celladdress = 'J107') %>% as.numeric()
    
    TPforsight_upSt <-
      culvert_extract(tidycells = tidycells,
                      sheetOI = 'Data Sheet - SITE',
                      celladdress = 'Y110') %>% as.numeric()
    TPbacksight_upSt <-
      culvert_extract(tidycells = tidycells,
                      sheetOI = 'Data Sheet - SITE',
                      celladdress = 'Y111') %>% as.numeric()
    TPforsight_dwSt <-
      culvert_extract(tidycells = tidycells,
                      sheetOI = 'Data Sheet - SITE',
                      celladdress = 'AD110') %>% as.numeric()
    TPbacksight_dwSt <-
      culvert_extract(tidycells = tidycells,
                      sheetOI = 'Data Sheet - SITE',
                      celladdress = 'AD111') %>% as.numeric()
    
    profile <-
      read_xlsx(path = filepath,
                sheet = 2,
                range = "A122:M140")
    profile <- profile %>% filter(!is.na(Distance)) %>%
      select(-starts_with("...")) %>%
      rename(Substrate = `Sub-\r\nstrate`,
             shotCode = `Shot From (R/U/D)`,
             rawHeight = Height) %>%
      mutate(
        crossingID = crossingID,
        `Feature Code` = str_to_upper(`Feature Code`),
        Substrate = str_to_upper(Substrate),
        shotCode = str_to_upper(shotCode),
        adjustedHt = surveyHtCorrection(
          rawHeight = rawHeight,
          shotCode = shotCode,
          Lidarht = Lidarht,
          roadCentHt = roadCentHt,
          TPforsight_upSt = TPforsight_upSt,
          TPbacksight_upSt = TPbacksight_upSt,
          TPforsight_dwSt = TPforsight_dwSt,
          TPbacksight_dwSt = TPbacksight_dwSt
        )
      )
    
    
    profile
  }



#
# Extract and calculate culvert cross-section height data  ------------------------------------------
#' crossSection
#' Extract cross sectional data of road and culvert structure from
#' field data sheet. The cross section is focused on the structre,
#' low tide elevation, hwi stain and wrack, ceiling of structure and road heights.
#' Currently this function returns only the field elevations and the adjusted NAVD88 heights.
#' Used with purrr::map functions to extract data into nested tibble
#' Second attempt by extracting calculated fields from Summary sheet.
#' # IDEA: Think about how this would be integrated into future systems with using raw values.
#' # BUG: aka data control issue. Some crossings along busy roads weren't surveyed from 
#' the center of the roadway so at times NA was entered in the road center height cell within the data sheet. 
#' To fix - first find the crossings missing the road center and fixing the datasheet. 
#' 
#' @param filepath
#' @param tidycells
#' @param lidarMeasure
#' @param roadWidth 
#'
#' @return
#' @export
#'
#' @examples
#'
crossSection <- function(filepath, tidycells, lidarMeasure, roadWidth) {
  # Set up variables for adjusting to NAVD88 with surveyHtCorrection()
  # surveyHtCorrection(rawHeight = , shotCode = , Lidarht = , roadCentHt = , TPforsight_upSt = , TPbacksight_upSt = , TPforsight_dwSt = , TPbacksight_dwSt = )
  # constants for each crossing
  # Debugging helpers.
  # filepath <- wtf$filePath
  # tidycells <- wtf$tidycells[[1]]
  # lidarMeasure <- wtf$da_LiDarHt_CL
  # roadWidth <- wtf$da_RoadWidth

    # Road center as measured in the field.
  roadCentHt <- culvert_extract(tidycells = tidycells,
                                sheetOI = 'Data Sheet - SITE',
                                celladdress = 'J107') %>% as.numeric()
  # Turning point (TP) shots for use in adjusting elevations.
  TPforsight_upSt <-
    culvert_extract(tidycells = tidycells,
                    sheetOI = 'Data Sheet - SITE',
                    celladdress = 'Y110') %>% as.numeric()

  TPbacksight_upSt <-
    culvert_extract(tidycells = tidycells,
                    sheetOI = 'Data Sheet - SITE',
                    celladdress = 'Y111') %>% as.numeric()
  TPforsight_dwSt <-
    culvert_extract(tidycells = tidycells,
                    sheetOI = 'Data Sheet - SITE',
                    celladdress = 'AD110') %>% as.numeric()
  TPbacksight_dwSt <-
    culvert_extract(tidycells = tidycells,
                    sheetOI = 'Data Sheet - SITE',
                    celladdress = 'AD111') %>% as.numeric()
  
  # Backup to provide correct element types used in mutate_at
  crossvars_num <-
    c("US Height",
      "DS Height",
      "crossingID",
      "adjustedHtUS",
      "adjustedHtDS")
  crossvars_chr <- c("Feature", "US cntrlPtcode", "DS cntrlPtcode")
  
  # Use read_xlsx to pull a cleaner table in (vs using culvert_extract())
  # Crossing Cross Section - section R98:AH106 from Data Sheet - SITE
  cross <-
    read_xlsx(
      path = filepath,
      sheet = 2,
      range = "R98:AH106",
      trim_ws = TRUE
    ) %>% # Raw values
    as_tibble() %>%
    rename(`US cntrlPtcode` = '...12', `DS cntrlPtcode` = '...17') %>%
    select(-starts_with("..."))
  # Low Tide Water Elevations
  tide <-
    read_xlsx(path = filepath,
              sheet = 2,
              range = "P94:AH95") %>%
    rename(`US cntrlPtcode` = '...14', `DS cntrlPtcode` = '...19') %>% # label columns as control points.
    select(-starts_with("..."))  %>%
    mutate_if(is.logical, .funs = ~ as.numeric(.)) %>% #ensure nums are nums
    mutate(Feature = "Low Tide Water Elevation") %>%
    mutate_at(crossvars_chr, .funs = ~ as.character(.)) # ensure chars are chars
  # Combine the tide elevations and the cross section elevations.
  cross <- rbind(cross, tide)
  
  # pull values from the longitudinal surveys to calculate Distances against.
  #' Note that this is the method that NH used in their plotting. Consider an alternative?
  #' NOTE: There's a few instances where there's no longitudinal profile due to water depth/access/other. How do we handle that issue?
  
  Longprofile <-
    channelLongidinalProfile_extract(filepath, tidycells, lidarMeasure)
  USinvert <-
    Longprofile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>% # Following NH, plots are derived from the invert distance measures.
    # filter(`Feature Code` == "I") %>%
    # NOTE: Use min to pull values from Long Profile table in case data was not entered into the US/DS invert dist cells.
    filter(Distance == min(Distance)) %>%
    mutate(Feature = "Invert", Position = "US") %>%
    select(Feature, Position, Distance, adjustedHt)
  DSinvert <-
    Longprofile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>% # Arrg. Some crossings use I as GCP.
    # NOTE: Use max to pull values from Long Profile table in case data was not entered into the US/DS invert dist cells.
    filter(Distance == max(Distance)) %>%
    mutate(Feature = "Invert", Position = "DS") %>%
    select(Feature, Position, Distance, adjustedHt)
  inverts <- rbind(USinvert, DSinvert) %>%
    rename(NAVD_ht = adjustedHt)
  # values for calculations of distance
  USInvdist <-
    USinvert %>% pull(Distance) %>% as.numeric() # Upstream invert distance
  DSInvdist <-
    DSinvert %>% pull(Distance) %>% as.numeric() # Downstream invert distance
  # Road center is located mid-way between the inverts. Not accurate to real-world but good enough for plotting.
  roadCentDist <- ((DSInvdist - USInvdist) / 2) + USInvdist
  
  
  # Insterted this munge code to simplify outputs to a tidy table of Feature-NAVD_ht-Distance for direct use in ggplot.
  # For RAW values remove and output just the cross object.
  # return(cross)
  crossSec <- cross %>%
    gather(-Feature, key = measure, value = ht) %>%
    separate(col = measure,
             into = c('position', 'k'),
             sep = " ") %>%
    unite(col = Feature, Feature, position, sep = "_") %>%
    spread(key = k, value = ht) %>%
    separate(
      col = Feature,
      into = c("Feature", "Position"),
      sep = "_",
      extra = "merge",
      fill = "right"
    ) %>%
    mutate(Height = as.numeric(Height),
           Feature = str_trim(gsub(
             x = Feature,
             pattern = "[0-9]",
             replacement = ""
           ))) %>% # remove marsh plain shot #s for later averaging
    group_by(Feature, Position) %>%
    mutate(Height = mean(Height, na.rm = TRUE)) %>%
    distinct(Feature, .keep_all = TRUE) %>%
    mutate(adjustedHt = mean(
      surveyHtCorrection(
        rawHeight = Height,
        shotCode = cntrlPtcode,
        Lidarht = lidarMeasure,
        roadCentHt = roadCentHt,
        TPforsight_upSt = TPforsight_upSt,
        TPbacksight_upSt = TPbacksight_upSt,
        TPforsight_dwSt = TPforsight_dwSt,
        TPbacksight_dwSt = TPbacksight_dwSt
      )
    )) %>%
    ungroup() %>%
    # add road center measured height using lidar measure as the other values have been corrected to that reference elevation.
    add_row(Feature = "Road Center", adjustedHt = lidarMeasure)
  
  
  #' distCalc
  #'
  #' @param feature
  #' @param position
  #'
  #' @return
  #' @export
  #'
  #' @examples
  distCalc <- function(feature,
                       position,
                       roadwidth,
                       roadCentDist) {
    # values for calculations of distance
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
      code == "HWI Stain_US" ~ as.numeric(USInvdist),
      code == "HWI Stain_DS" ~ as.numeric(DSInvdist),
      code == "HWI Wrack_US" ~ as.numeric(USInvdist),
      code == "HWI Wrack_DS" ~ as.numeric(DSInvdist),
      code == "Low Tide Water Elevation_US" ~ as.numeric(USInvdist),
      code ==  "Low Tide Water Elevation_DS" ~ as.numeric(DSInvdist),
      code ==  "Marsh Plain Shot_US" ~ as.numeric(USInvdist),
      code ==  "Marsh Plain Shot_DS" ~ as.numeric(DSInvdist),
      code ==  "Road Surface_US" ~ as.numeric(roadCentDist - (roadwidth /
                                                                2)),
      code ==  "Road Surface_DS" ~ as.numeric(roadCentDist + (roadwidth /
                                                                2)),
      code ==  "Road Center_NA" ~ as.numeric(roadCentDist),
      # not in an up or downstream position.
      is.null(code) ~ 999,
      is.na(code) ~ 9999,
      code == "NA_NA" ~ 99999
    )
    
  }
  # distCalc <- safely(distCalc)
  
  # return a sf with distances added in.
  crossSec %>% mutate(
    Distance = distCalc(
      feature = Feature,
      position = Position,
      roadwidth = roadWidth,
      roadCentDist = roadCentDist
    )
  )
  
}

# Deprecated ----
#' # REworking the function to calculate and add in the distance measures to the 'cross section'.
#' #
#' # cross section height and distances -----------------------------------------------
#' #' crossSectionHeights
#' #' This function should return the heights for the features mease
#' #' Extract cross sectional data of road and culvert structure
#' #' Used with purrr::map functions to extract data into nested tibble
#' #' Second attempt by extracting calculated fields from Summary sheet.
#' #' # IDEA: Think about how this would be integrated into future systems with using raw values.
#' #'
#' #' @param longitudinalProfile column containing longitudinal profile data as extracted by function channelLongidinalProfile_extract
#' #' @param tidycellCol 
#' #' @param roadWidth column containing roadwidth measures from desktop assessment
#' #' @param lidarMeasure column containing lidar measured from desktop assessments.
#' #'
#' #' @return tibble with the intention of being included in a nested df from calling purrr::map
#' #' @export
#' #'
#' #' @examples
#' #'
#' calcHeights <-
#'   function(tidycellCol,
#'            longitudinalProfile,
#'            roadWidth,
#'            lidarMeasure) {
#'     # Set up variables for adjusting to NAVD88 with surveyHtCorrection()
#'     
#'     # Road width used in the calculations of distance.
#'     # NOTE:  Road width was never recorded in Desktop side of assessments (as of 22-June-2020)
#'     # TODO: Finish collecting road widths
#'     # AFS- Addded field to record road width following NH protcol - measured from aearials from road shoulder to road shoulder.
#'     roadw <- roadWidth
#'     # Pull invert distances from longitudinalProfiles
#'     
#'     # pull values from the longitudinal surveys to calculate Distances against ----
#'     # NOTE: Use max and min to pull values from Long Profile table in case data was not entered into the US/DS invert dist cells.
#'     # TODO: Need to add some sort of control for when only up or downstream inverts are found.
#'     
#'     USinvert <-
#'       longitudinalProfile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>%
#'       # filter(`Feature Code` == "I") %>%
#'       filter(Distance == min(Distance)) %>%
#'       mutate(Feature = "Invert", Position = "US") %>%
#'       select(Feature, Position, Distance, adjustedHt)
#'     # USinvert <- if_else(nrow(USinvert) =< 1, NA, USinvert)
#'     
#'     DSinvert <-
#'       longitudinalProfile %>% filter(str_detect(`Feature Code`, pattern = "I")) %>% # Arrg. Some crossings use I as GCP.
#'       filter(Distance == max(Distance)) %>%
#'       mutate(Feature = "Invert", Position = "DS") %>%
#'       select(Feature, Position, Distance, adjustedHt)
#'     
#'     inverts <- rbind(USinvert, DSinvert) %>%
#'       rename(NAVD_ht = adjustedHt)
#'     
#'     USInvdist <-
#'       if (exists("USinvert"))
#'         USinvert %>% pull(Distance) %>% as.numeric()
#'     else
#'       999 # Upstream invert distance
#'     DSInvdist <-
#'       if (exists("DSinvert"))
#'         DSinvert %>% pull(Distance) %>% as.numeric()
#'     else
#'       999 # Downstream invert distance
#'     roadCentDist <- ((DSInvdist - USInvdist) / 2) + USInvdist
#'     
#'     cross <-
#'       read_xlsx(
#'         path = filepath,
#'         sheet = 2,
#'         range = "R98:AH106",
#'         trim_ws = TRUE
#'       ) %>% # Raw values
#'       as_tibble() %>%
#'       rename(`US cntrlPtcode` = '...12', `DS cntrlPtcode` = '...17') %>%
#'       select(-starts_with("..."))
#'     # Low Tide Water Elevations
#'     tide <-
#'       read_xlsx(path = filepath,
#'                 sheet = 2,
#'                 range = "P94:AH95") %>%
#'       rename(`US cntrlPtcode` = '...14', `DS cntrlPtcode` = '...19') %>% # label columns as control points.
#'       select(-starts_with("..."))  %>%
#'       mutate_if(is.logical, .funs = ~ as.numeric(.)) %>% #ensure nums are nums
#'       mutate(Feature = "Low Tide Water Elevation") %>%
#'       mutate_at(crossvars_chr, .funs = ~ as.character(.)) # ensure chars are chars
#'     cross <- rbind(cross, tide)
#'     cross <- cross %>%
#'       gather(-Feature, key = measure, value = ht) %>%
#'       separate(col = measure,
#'                into = c('position', 'k'),
#'                sep = " ") %>%
#'       unite(col = Feature, Feature, position, sep = "_") %>%
#'       spread(key = k, value = ht) %>%
#'       separate(
#'         col = Feature,
#'         into = c("Feature", "Position"),
#'         sep = "_",
#'         extra = "merge",
#'         fill = "right"
#'       ) %>%
#'       mutate(Height = as.numeric(Height),
#'              Feature = str_trim(gsub(
#'                x = Feature,
#'                pattern = "[0-9]",
#'                replacement = ""
#'              ))) %>% # remove marsh plain shot #s for later averaging
#'       group_by(Feature, Position) %>%
#'       mutate(Height = mean(Height, na.rm = TRUE)) %>%
#'       distinct(Feature, .keep_all = TRUE) %>%
#'       mutate(adjustedHt = mean(
#'         surveyHtCorrection(
#'           rawHeight = Height,
#'           shotCode = cntrlPtcode,
#'           Lidarht = lidarMeasure,
#'           roadCentHt = roadCentHt,
#'           TPforsight_upSt = TPforsight_upSt,
#'           TPbacksight_upSt = TPbacksight_upSt,
#'           TPforsight_dwSt = TPforsight_dwSt,
#'           TPbacksight_dwSt = TPbacksight_dwSt
#'         )
#'       )) %>%
#'       ungroup() %>%
#'       add_row(Feature = "Road Center", adjustedHt = roadCentHt)
#'     
#'     
#'     
#'     
#'     #' distCalc
#'     #'
#'     #' @param feature
#'     #' @param position
#'     #'
#'     #' @return
#'     #' @export
#'     #'
#'     #' @examples
#'     distCalc <- function(feature,
#'                          position,
#'                          roadwidth,
#'                          roadCentDist) {
#'       # TODO: Create individual calculations for the distances. PIA.
#'       # values for calculations of distance
#'       # if(missing(feature)){
#'       #   feature = NA
#'       # }
#'       # if(missing(position)){
#'       #   position = NA
#'       # }
#'       code <- paste(feature, position, sep = "_")
#'       case_when(
#'         code == "Ceiling of Structure_US" ~  as.numeric(USInvdist),
#'         code == "Ceiling of Structure_DS" ~ as.numeric(DSInvdist),
#'         code == "HWI Stain_US" ~ as.numeric(USInvdist),
#'         code == "HWI Stain_DS" ~ as.numeric(DSInvdist),
#'         code == "HWI Wrack_US" ~ as.numeric(USInvdist),
#'         code == "HWI Wrack_DS" ~ as.numeric(DSInvdist),
#'         code == "Low Tide Water Elevation_US" ~ as.numeric(USInvdist),
#'         code ==  "Low Tide Water Elevation_DS" ~ as.numeric(DSInvdist),
#'         code ==  "Marsh Plain Shot_US" ~ as.numeric(USInvdist),
#'         code ==  "Marsh Plain Shot_DS" ~ as.numeric(DSInvdist),
#'         code ==  "Road Surface_US" ~ as.numeric(roadCentDist - (roadwidth / 2)),
#'         code ==  "Road Surface_DS" ~ as.numeric(roadCentDist + (roadwidth / 2)),
#'         code ==  "Road Center_NA" ~ as.numeric(roadCentDist),
#'         # not in an up or downstream position.
#'         is.null(code) ~ 999,
#'         is.na(code) ~ 9999,
#'         code == "NA_NA" ~ 99999
#'       )
#'       
#'     }
#'     # distCalc <- safely(distCalc)
#'     
#'     # return a sf with distances added in.
#'     cross %>% mutate(
#'       Distance = distCalc(
#'         feature = Feature,
#'         position = Position,
#'         roadwidth = roadw,
#'         roadCentDist = roadCentDist
#'       )
#'     )
#'     
#'     
#'   }


# Debugging work ====
#TODO: Drop this after fixes.
channelLongidinalProfile_extract <-
  possibly(.f = channelLongidinalProfile_extract, otherwise = "Error encountered on longitudinal profile.")
crossSection <- possibly(crossSection, otherwise = "Data not collected")
# calcHeights <- possibly(calcHeights, otherwise = "Error encountered")
