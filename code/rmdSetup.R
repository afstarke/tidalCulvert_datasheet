# Rmd setup


## @knitr directorySetup  ----
# 
source("00_libraries.R")
source("functions/culvert_tidy.R")
source("photoMatchTable.R")
# Data Update:
## Update the Data?!
# dataUpdate <- menu(c("Update Data?", "Use cached data?"), title = "Do you want to update the data?") 
dataUpdate <- 2
pivots <- 2
# Write outputs to folders option- Change to allow writes to be made- 
# Keep false when running many times to avoid conflicts with Box

writeOutputs = dataUpdate


## Set up folders ----
# for pulling data and storing outputs.
# Folder and file INPUTS
# Master document that was used for tracking data needs etc.
tidalCulvert_Checklist <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/MASTERCrossingSpreadsheetChecklistEdit.xlsx"
# key sheet- for decoding the values extracted from the workbooks. Add cells of interest and what to call the data upon it's exctraction here.
keysheet <- read_excel("../../../Box Sync/Culvert Assessment/Tidal Assessments/key.xlsx")
#DONE: cull out the desktop data being extracted from the workbooks as we move towards managed data in AGOL or other system.
keysheet <- keysheet %>% filter(AssessmentType != "Desktop")
# Folder where workiung copies of assessment workbooks live
# tidalCulvert_datasheetsFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/Tidal Culvert DataSheets_WORKINGvers/"
# Began QA process on these files and moved files into new QAed folder
tidalCulvert_datasheetsFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/TidalAssessmentWorkbooks_QAvers/"

# Field schedule from Teams.
crossingTracker <- read_excel("Z:/Culvert_Field_Visit_Schedule.xlsx") 
## folder OUTPUTS 
# Teams folder
# Mapped locally using the PIA method of browsing to Teams link in Internet Explorer, then mapping drive link to Z: in my PC
# Box folder for outputs
tidalCulvert_outputs <- "../../../Box Sync/Culvert Assessment/Tidal Assessments"

# pivots <- menu(c("Create pivot tables?", "Proceed without?"), title = "Generate a pivot summary for reviewing field assessment schedule?")


# Vector listing the crossings that are on the 'Field schedule' on Teams
crossings_TODO <- crossingTracker %>% select(crossingID) %>% unlist()
crossingTrackerlist <- crossingTracker %>% 
  select(crossingID, visited = `Visited(Y or N)`, 
         date_visited_ct = `Date visited`, # _ct for data from crossing tracker sheet.
         FieldAssesComplete_ct = `Field Ass. Complete(Y/N)`, 
         incompleteReason_ct = `Reason: inaccesible, un-assessable, not a crossing`)



# create vectors of column names for easier selecting and mutating later ====
  fieldVars <- keysheet %>% filter(AssessmentType == 'Field_min') %>%  pluck('dataName') %>% unlist()
  numericVars <- keysheet %>% filter(VariableType == 'numeric') %>% pluck('dataName') %>% unlist() 
  characterVars <- keysheet %>% filter(VariableType == 'character') %>% pluck('dataName') %>% unlist()
  logicalVars <- keysheet %>% filter(VariableType == 'logical') %>% pluck('dataName') %>% unlist()
  desktopVars <- keysheet %>% filter(AssessmentType == 'Desktop') %>% pluck('dataName') %>% unlist() 


## @knitr spatialDataLoad  ----
# Read in shapefile containing identified crossing locations with attributes related to the priorities
# read in as sf and transform to WGS84 
# add a few attributes/fields for prioritization and tracking 'field assessment schedules'

# Culvert locations ----
# Pull from AGOL directly. 
# tidalCrossings_desktop <- sf::st_read("https://services.arcgis.com/F7DSX1DSNSiWmOqh/arcgis/rest/services/TidalCrossing_desktopDataEntry_fieldCrew/FeatureServer/0/query?where=fid+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=&returnHiddenFields=true&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=wSuBiKGEfq4ra4Yq5jjnNdKhHXjZsu_L7odXzo6hAGqm3wywSBG8YlhBn6dkYoLj6prfIslEqU4yACzLAYL88aEK7jrmr7kSETHTLlAhIkuPP-w9vdP-3KesrVFb6kamruDJIqpFDxUZHxfdvVDRyGejkNg4piNxyfHGqr35JrYIeClmL_vYQBjvVr4v-qft547OhEZiXQsU9szPUg3nx_Lg1zkQfclMhoX9YAsMseSZ3qr_ySTcfxkki57S0nLw")
tidalCrossings_desktop <- sf::st_read("https://services.arcgis.com/F7DSX1DSNSiWmOqh/arcgis/rest/services/TidalCrossing_desktopDataEntry/FeatureServer/0/query?where=fid+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=K9dBF5FaWPzxogvnryqp3PsntWLZe0zZ0CvY8T7gVxs_IRWpW5ZwsohNW8Jo2ZdGKf4S2XJj-I3YISnaXPLnx3YuvuBZj6oimop3LDBQJxFqnliZwhIQ-7zyyBnd7msldjFnJiddPv19I9pJMJ4wpRIAAC7a7iM2Zfgk9jZ7Cs_AgDO1mi99lcTAvsAasonuQCqJjTqJoGj5SNtNGzpeXYGRA7gJLlAUmzA4BxzTifeZ5096FujzukRc2X8q4ZNG")
culvertPOTpts <- tidalCrossings_desktop #%>% select(crossingID:Latitude) # trim desktop data?
  
  
if(dataUpdate == 1){
  LIculvert_GISpts <- culvertPOTpts %>% st_transform(4326) %>% 
    st_zm(drop = TRUE) %>% 
    mutate(PriorityScore = as.numeric(PtrPriorit) + as.numeric(TNCPriorit) + as.numeric(MarPriorit)) %>% # create a new 'priority score'
    mutate(listedOnFieldSched = crossingID %in% crossings_TODO, Located = "Y",
           crossingID = as.numeric(crossingID)) 
  LIculvert_GISpts %>% write_rds(path = "data/LIculvert_GISpts.rds")
} else{
  LIculvert_GISpts <- read_rds("data/LIculvert_GISpts.rds")
}

# QA ----
# DEPRECATED
# Identify points that may cause issues
# errorPts <- LIculvert_GISpts %>% filter(Latitude < 30) %>% select(crossingID) %>% st_set_geometry(NULL)
# Check for duplicate points
# duplicatePts <- LIculvert_GISpts %>% 
#   group_by(crossingID) %>% 
#   filter(n()> 1) %>% 
#   select(crossingID) %>%  
#   st_set_geometry(NULL) %>% as_vector()

## @knitr culvertData  ----
# Extract the field data from the workbooks and bring them in.
# Key functions
source("functions/culvert_tidy.R")
source("functions/culvert_extract.R")    
if(dataUpdate == 1){
  # Create a nested dataframe with filenames, file paths, and tidied cells from tidal assessment workbooks,
  # columns include tidycells- raw cell content from each workbook; decoded- transformed raw values extracted from 
  # workbooks using key as lookup; 
  # This tibble should be the same length as the number of worksheets in the folder where the data is stored. 
  LIculvertsAssessments <- culvert_tidy(tidalCulvert_datasheetsFolder) 
  LIculvertsAssessments <- LIculvertsAssessments %>% 
    mutate(crossingID = map_chr(.x = tidycells, ~culvert_extract(tidycells = .x, sheetOI = 'Data Sheet - SITE', celladdress = 'L7')),
           decoded = map(.x = tidycells, .f = ~decodeSheet(.x, keysheet)), # this is where the sausage is made. 
           longProfile = map2(.x = filePath, .y = tidycells, .f = ~channelLongidinalProfile_extract(.x, .y)), # longitudinal profile data
           heights = map2(.x = filePath, .y = tidycells, .f = ~crossSection(.x, .y)), # crossing sectional profile data # Added possibly() to function call in tidyCulvert.R
           crossHeights = pmap(.l = list(tidycells, longProfile, heights), 
                      .f = possibly(calcHeights, otherwise = "Failed to calculate")))
  LIculvertsAssessments %>% write_rds(path = "data/LIculvertsAssessments.rds")
}else{
  LIculvertsAssessments <- read_rds(path = "data/LIculvertsAssessments.rds")
}

  # Culvert Field Assessment Data ----
if(dataUpdate == 1){
  # unnest the tidyxl data to create a large tidy dataframe
  LIculvertData <- LIculvertsAssessments %>% 
    select(filenames, lastChanges, decoded) %>% 
    unnest() %>% select(filenames, lastChanges, dataName, values) %>% 
    spread(key = dataName, value = values) %>% 
    select(filenames, crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
    mutate(dateAssessed2 = lubridate::parse_date_time(dateAssessed, orders = 'ymd'),
           AsmtStartTime = format(AsmtStartTime, format = "%H:%M:%S"),
           AsmtEndTime = format(AsmtEndTime, format = "%H:%M:%S"),
           crossingID = as.numeric(crossingID)) %>% 
    mutate_if(is.character, list(~na_if(., "N/A"))) %>%   # Convert character columns with "N/A" to NA
    mutate_at(numericVars, as.numeric) %>% 
    mutate_at(logicalVars, as.logical)
  # DONE: mutate outlet to Atlantic and outlet Subtidal to be one column each not 2 as in the datasheet.
  # DONE: Add column in key sheet that will be used as data dictionary. Select and paste that info to sheet 2 of the culvert data output below.
  LIculvertData %>% write_rds(path = "data/LIculvertData.rds")
}else{
  LIculvertData <- read_rds(path = "data/LIculvertData.rds")
}

# QA (con't) ----

if (typeof(LIculvertData$crossingID) == typeof(LIculvert_GISpts$crossingID)) {
  # Are the two columns the same datatype?
  matchedLIculverts <-
    LIculvertData %>% filter(crossingID %in% LIculvert_GISpts$crossingID) %>% 
    select(filenames, crossingID) # Culvert datasheets that ARE IN GIS point dataset
  missingLIculverts <-
    LIculvertData %>% filter(!crossingID %in% LIculvert_GISpts$crossingID) %>% 
    select(filenames, crossingID) # Culvert datasheets that ARE NOT IN GIS points.
} else{
  stop("CrossingID not same data type (character vs numeric)")
}

# TODO: MAYBE... add spatial component earlier in code. No need to wait to gather it here.
# if sourced LIculvert_GISpts and LIculvertData already are in global environment.
LIculvertData_location <- LIculvert_GISpts %>% 
  left_join(LIculvertData, by = "crossingID")  # this object is picking up 2 extra joins- perhaps dupicate CrossingIDs? YUP> 2007 is duplicated in GIS file.
  

if(dataUpdate == 1){
  LIculvertDataStatus <- LIculvertsAssessments %>% 
    select(filenames, lastChanges, filePath, decoded) %>% 
    unnest(decoded, .drop = FALSE) %>% 
    select(filenames, lastChanges, filePath, dataName, values) %>%
    spread(key = dataName, value = values) %>% 
    select(filenames, crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
    mutate(CrossingTypeIDed = ifelse(CrossingType >= 2, yes = 1, no = 0), 
           crossingID = as.numeric(crossingID)) %>% 
    # rowwise() %>% 
    # mutate(MissingchannelPoolWidths = sum(!is.na(c(ChannelWidth_upStream, 
    #                                                ChannelWidth_dwnStream, 
    #                                                MaxPoolWidth_upStream, 
    #                                                MaxPoolWidth_dwnStream)))/4 == 1,
    #        MissingCatchmentAttributes = sum(!is.na(c(WatershedArea_upStream, 
    #                                                  saltMarshArea, 
    #                                                  WatershedLndCover_wetland, 
    #                                                  WatershedLndCover_forested, 
    #                                                  WatershedLndCover_impervious, 
    #                                                  WatershedLndCover_developed)))/6 == 1,
    #        MissingMarshMigrationPotential = sum(!is.na(c(MarshMigrPot_acres, 
    #                                                      MarshMigrPot_evalUnit, 
    #                                                      NWI_class_upStream, 
    #                                                      NWI_class_dwnStream)))/4 == 1) %>%
    replace_na(list(FieldAssessmentComplete = "N", 
                    DesktopAssessmentComplete = "N", 
                    FullAssessmentComplete = "N")) %>% 
    select(filenames, lastChanges, crossingID, FieldAssessmentComplete,
           FullAssessmentComplete, MissingFromAssessment, observers,
           dateAssessed, photoStorageLocation, CrossingTypeIDed) %>%
    left_join(crossingTrackerlist, by = "crossingID") %>% 
    left_join(pics)
  LIculvertDataStatus %>% write_rds(path = "data/LIculvertDataStatus.rds")
}else{
  LIculvertDataStatus <- read_rds(path = "data/LIculvertDataStatus.rds")
}

if(dataUpdate == 1){
  LIculvertDataStatus_location <- LIculvert_GISpts %>%
    left_join(LIculvertDataStatus, by = "crossingID") %>%
    mutate(missingWorkbook = is.na(filenames)) %>%
    replace_na(list(FieldAssessmentComplete = "N", 
                    # DesktopAssessmentComplete = "N", 
                    FullAssessmentComplete = "N")) %>%
    select(crossingID, listedOnFieldSched:missingWorkbook, starts_with("da_"), ends_with("_ct")) %>%
    replace_na(list(FieldAssesComplete_ct = 'N')) %>%
    mutate(discrp = FieldAssesComplete_ct != FieldAssessmentComplete)
  LIculvertDataStatus_location %>% write_rds(path = "data/LIculvertDataStatus_location.rds")
}else{
  LIculvertDataStatus_location <- read_rds(path = "data/LIculvertDataStatus_location.rds")
}

# 
# LIculvertDataStatus_location %>% group_by(crossingID) %>% tally() %>% filter(n > 1)
# 
# AssessmentTally <- ftable(addmargins(xtabs(~ listedOnFieldSched+FieldAssessmentComplete+DesktopAssessmentComplete, LIculvertDataStatus_location)))


## Data outputs ====
if(writeOutputs == TRUE){
  # st_write(LIculvertDataStatus_location, dsn = "outputs/tidalCulvertLocations.gpkg",
  #          layer = "tidalCrossingStatus", update  = TRUE, layer_options = "OVERWRITE=YES")
  # # # This keeps failing to write to file. giving an error of GDAL Error 1:
  # # st_write(LIculvertData_location, dsn = "outputs/tidalCulvertLocations.gpkg", 
  # #          layer = "tidalCrossingData", update = TRUE, layer_options = c("OVERWRITE=YES"))
  # 
  writexl::write_xlsx(LIculvertData_location, path = paste0(tidalCulvert_outputs, "/compiledCulvert_data.xlsx"))
  # write.xlsx(LIculvertDataStatus_location %>% st_drop_geometry(), file = paste0(tidalCulvert_outputs, "/CulvertData_status.xlsx"), sheetName = "Culvert Status")
  # writexl::write_xlsx(LIculvertDataStatus_location %>% 
  #              st_drop_geometry(), 
  #            path = paste0(tidalCulvert_outputs, "/AssessmentStatusList.xlsx"))
  # # Save to TEAMS folder/file.
  writexl::write_xlsx(LIculvertDataStatus_location %>%
               st_drop_geometry(),
             path = "Z:/AssessmentStatusList.xlsx")
  # write.table(crossings_TODO, file = paste0(tidalCulvert_outputs, "/CrossingsToVisit_2019.txt"), sep = ",", quote = F, col.names = F,
  #             row.names = FALSE, eol = ",")
  
} else{
  warning("NO Files are being written to Box folder")
}


# Pivot tables ----
## @knitr fieldAssessmentPivot
# grouped by = FieldAssessmentComplete,listedOnFieldSched

LIculvertDataStatus_location %>% st_drop_geometry() %>% 
  group_by(FieldAssessmentComplete,listedOnFieldSched) %>%
  rpivotTable::rpivotTable(rows = "FieldAssessmentComplete", cols = "listedOnFieldSched")


if(pivots == 1){
  # crossingTracker %>% select(crossingID, PtrPriorit:`TNC&PtnrSum`, `Revisit required?`, `Field Ass. Complete(Y/N)`) %>% 
  # group_by(`Field Ass. Complete(Y/N)`, TNCPriorit) %>% tally() 
  crossingTracker %>% 
    select(crossingID, PtrPriorit:`TNC&PtnrSum`, `Revisit required?`, `Field Ass. Complete(Y/N)`) %>% 
    rpivotTable()
}



# # DataExplorer reports
# config <- configure_report(
#   global_ggtheme = quote(theme_ipsum(base_size = 12))
# )
# create_report(data = tidalCrossings_desktop %>% st_drop_geometry(), config = config, output_file = "Desktop data report.html")
# create_report(data = LIculvertData, output_file = "Tidal culvert data: Field.html", config = config)
# create_report(data = st_drop_geometry(LIculvertDataStatus_location), output_file = "Tidal Culvert Data.html", config = config)
