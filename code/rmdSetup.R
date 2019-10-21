# Rmd setup


## @knitr directorySetup
# 
source("00_libraries.R")
source("functions/culvert_tidy.R")
# Data Update:
## Update the Data?!
dataUpdate <- menu(c("Update Data?", "Use cached data?"), title = "Do you want to update the data?") 

# Write outputs to folders option- Change to allow writes to be made- 
# Keep false when running many times to avoid conflicts with Box

writeOutputs = dataUpdate

# Setup the folders for pulling and storing
# Folder where workiung copies of assessment workbooks live
# tidalCulvert_datasheetsFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/Tidal Culvert DataSheets_WORKINGvers/"
# Began QA process on these files and moved files into new QAed folder
tidalCulvert_datasheetsFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/TidalAssessmentWorkbooks_QAvers/"

# Feild schedule from Teams.
crossingTracker <- read_excel("Z:/Culvert_Field_Visit_Schedule.xlsx") 

pivots <- menu(c("Create pivot tables?", "Proceed without?"), title = "Generate a pivot summary for reviewing field assessment schedule?")

if(pivots == 1){
  # crossingTracker %>% select(crossingID, PtrPriorit:`TNC&PtnrSum`, `Revisit required?`, `Field Ass. Complete(Y/N)`) %>% 
  # group_by(`Field Ass. Complete(Y/N)`, TNCPriorit) %>% tally() 
  crossingTracker %>% 
    select(crossingID, PtrPriorit:`TNC&PtnrSum`, `Revisit required?`, `Field Ass. Complete(Y/N)`) %>% 
    rpivotTable()
}

crossings_TODO <- crossingTracker %>% select(crossingID) %>% unlist()
crossingTrackerlist <- crossingTracker %>% 
  select(crossingID, visited = `Visited(Y or N)`, 
         date_visited_ct = `Date visited`, # _ct for data from crossing tracker sheet.
         FieldAssesComplete_ct = `Field Ass. Complete(Y/N)`, 
         incompleteReason_ct = `Reason: inaccesible, un-assessable, not a crossing`)


# Folders to write outputs to.
# Box folder ----
tidalCulvert_outputs <- "../../../Box Sync/Culvert Assessment/Tidal Assessments"
# Master document that was used for tracking data needs etc.
tidalCulvert_Checklist <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/MASTERCrossingSpreadsheetChecklistEdit.xlsx"
# key sheet- for decoding the values extracted from the workbooks. Add cells of interest and what to call the data upon it's exctraction here.
keysheet <- read_excel("../../../Box Sync/Culvert Assessment/Tidal Assessments/key.xlsx")
#TODO: cull out the desktop data being extracted from the workbooks as we move towards managed data in AGOL or other system.

# Teams folder ----
# Mapped locally using the PIA method of browsing to Teams link in Internet Explorer, then mapping drive link to Z: in my PC


# Crossing data that Karen and Stephen put together for tidal assessments. This data is actively managed on AGOL through a hosted feature service
# that is hosted FROM NYSPATIAL. To work around reading into R, with out a ESRI license on my local machine, I've created a .mxd with the feature service added
# as a layer in that map. To update, you must open the .mxd and export data as a 'snapshot'.
# THis sucks... Methods to fix requires a local ESRI license/install to use arcgis-R bridge. With that bridge direct reads from SDE are possible.
culvertPts_path <- "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/Tidal_Crossing_Data.gdb" # Direct path to the gdb used as base. Change as needed. 

# Potential culvert locations:
# st_layers(dsn = "M:/Projects/LI/Culvert_Assessment/LI_Culvert_Assessment.gdb")
st_layers(dsn = "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/Tidal_Crossing_Data.gdb")
culvertPOTpts <- st_read(dsn = culvertPts_path, layer = "TidalCrossings_20191008") # latest version as of 9 Oct 2019

# Ideally this st_read would pull direct from the web url but need to provide some sort of credentials to get past tnc.nyspatial. sign on securely.
tidalCrossings_desktop <- sf::st_read("https://services.arcgis.com/F7DSX1DSNSiWmOqh/arcgis/rest/services/TidalCrossing_desktopDataEntry_fieldCrew/FeatureServer/0/query?where=FID%3E0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnHiddenFields=false&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=gLV7StsBBMzzjRUfTdkBHaaJoLfKqZi4fTl-VjnjyfeRid4xV1NwnBKdDCUMqCPdbrhm5apyTxI_DcDhmqDuUGPoENT83Ivy50AdQXtE2RtRNeZFKedx3O28cMk-G6dG1yvoTC_ZL3r6mEnaMwNHkOzP7RZoCRSzhuLcZILIUXD64IjzYtGRQJOOASGcGAjewlx9h06TIrM2T4P3iII8S52znyZswzuOpTQulVPhXzAYcBBjZpQFgo2NEuZLxSIs")

# tidalCrossings_desktop <- sf::st_read(dsn = "M:/tnc_viewer@nyspatial.tnc.org.sde", layer = "TidalCrossings_Latest")
# http://nyspatial.tnc.org:6080/arcgis/services


  fieldVars <- keysheet %>% filter(AssessmentType == 'Field_min') %>%  pluck('dataName') %>% unlist()
  numericVars <- keysheet %>% filter(VariableType == 'numeric') %>% pluck('dataName') %>% unlist() 
  characterVars <- keysheet %>% filter(VariableType == 'character') %>% pluck('dataName') %>% unlist()
  logicalVars <- keysheet %>% filter(VariableType == 'logical') %>% pluck('dataName') %>% unlist()
  desktopVars <- keysheet %>% filter(AssessmentType == 'Desktop') %>% pluck('dataName') %>% unlist() 

# GIS data

## @knitr spatialDataLoad
# Read in shapefile containing identified crossing locations with attributes related to the priorities
# read in as sf and transform to WGS84 
# add a few attributes/fields for prioritization and tracking 'field assessment schedules'
if(dataUpdate == 1){
  LIculvert_GISpts <- st_read(dsn = culvertPts_path, layer = "TidalCrossings_20191008") %>% st_transform(4326) %>% 
    st_zm(drop = TRUE) %>% 
    mutate(crossingID = as.character(Tidal_ID)) %>% # For joining on this attribute later.
    mutate(PriorityScore = as.numeric(PtrPriorit) + as.numeric(TNCPriorit) + as.numeric(MarPriorit)) %>% # create a new 'priority score'
    # select(Ownership, 
    #        Name, 
    #        crossingID, 
    #        MarshCompl, 
    #        PtrPriorit, 
    #        TNCPriorit, 
    #        MarPriorit, 
    #        Road,
    #        Rd_Owner,
    #        Town, 
    #        BASIN_SQMI,
    #        FOR_PERC,
    #        WET_PERC,
    #        DEV_PERC,
    #        WATER_PERC,
    #        OTHER_PERC,
    #        MARSH_AC,
    #        IS_PERC_MEAN,
    #        Notes) %>% 
    mutate(ToBeAssessed_2019 = crossingID %in% crossings_TODO, Located = "Y",
           crossingID = as.numeric(crossingID)) 
  LIculvert_GISpts %>% write_rds(path = "data/LIculvert_GISpts.rds")
} else{
  LIculvert_GISpts <- read_rds("data/LIculvert_GISpts.rds")
}


# Identify points that mau cause issues...
# errorPts <- LIculvert_GISpts %>% filter(Latitude < 30) %>% select(crossingID) %>% st_set_geometry(NULL)
# Check for duplicate points
duplicatePts <- LIculvert_GISpts %>% 
  group_by(crossingID) %>% 
  filter(n()> 1) %>% 
  select(crossingID) %>%  
  st_set_geometry(NULL) %>% as_vector()
  
# crossSection2Safe <- safely(crossSection2)

## @knitr culvertData
# Extract the field and desktop data from the workbooks and bring them in.
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
                      .f = possibly(calcHeights, otherwise = "SHIT!")))
  LIculvertsAssessments %>% write_rds(path = "data/LIculvertsAssessments.rds")
}else{
  LIculvertsAssessments <- read_rds(path = "data/LIculvertsAssessments.rds")
}

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
  # TODO: Add column in key sheet that will be used as data dictionary. Select and paste that info to sheet 2 of the culvert data output below.
  LIculvertData %>% write_rds(path = "data/LIculvertData.rds")
}else{
  LIculvertData <- read_rds(path = "data/LIculvertData.rds")
}



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
    rowwise() %>% 
    mutate(MissingchannelPoolWidths = sum(!is.na(c(ChannelWidth_upStream, 
                                                   ChannelWidth_dwnStream, 
                                                   MaxPoolWidth_upStream, 
                                                   MaxPoolWidth_dwnStream)))/4 == 1,
           MissingCatchmentAttributes = sum(!is.na(c(WatershedArea_upStream, 
                                                     saltMarshArea, 
                                                     WatershedLndCover_wetland, 
                                                     WatershedLndCover_forested, 
                                                     WatershedLndCover_impervious, 
                                                     WatershedLndCover_developed)))/6 == 1,
           MissingMarshMigrationPotential = sum(!is.na(c(MarshMigrPot_acres, 
                                                         MarshMigrPot_evalUnit, 
                                                         NWI_class_upStream, 
                                                         NWI_class_dwnStream)))/4 == 1) %>%
    replace_na(list(FieldAssessmentComplete = "N", DesktopAssessmentComplete = "N", FullAssessmentComplete = "N")) %>% 
    select(filenames, lastChanges, crossingID, FieldAssessmentComplete, 
           DesktopAssessmentComplete, FullAssessmentComplete, MissingFromAssessment, observers, 
           dateAssessed, photoStorageLocation, CrossingTypeIDed, MissingchannelPoolWidths, MissingCatchmentAttributes, 
           MissingMarshMigrationPotential) %>% 
    left_join(crossingTrackerlist, by = "crossingID")
  LIculvertDataStatus %>% write_rds(path = "data/LIculvertDataStatus.rds")
}else{
  LIculvertDataStatus <- read_rds(path = "data/LIculvertDataStatus.rds")
}

if(dataUpdate == 1){
  LIculvertDataStatus_location <- LIculvert_GISpts %>% 
    left_join(LIculvertDataStatus, by = "crossingID") %>% 
    mutate(missingWorkbook = is.na(filenames)) %>% 
    replace_na(list(FieldAssessmentComplete = "N", DesktopAssessmentComplete = "N", FullAssessmentComplete = "N")) %>% 
    select(crossingID, ToBeAssessed_2019:photoStorageLocation, MissingchannelPoolWidths:MissingMarshMigrationPotential, ends_with("_ct")) %>% 
    replace_na(list(FieldAssesComplete_ct = 'N')) %>% 
    mutate(discrp = FieldAssesComplete_ct != FieldAssessmentComplete)
  LIculvertDataStatus_location %>% write_rds(path = "data/LIculvertDataStatus_location.rds")
}else{
  LIculvertDataStatus_location <- read_rds(path = "data/LIculvertDataStatus_location.rds")
}


LIculvertDataStatus_location %>% group_by(crossingID) %>% tally() %>% filter(n > 1)

# AssessmentTally <- ftable(addmargins(xtabs(~ ToBeAssessed_2019+FieldAssessmentComplete+DesktopAssessmentComplete, LIculvertDataStatus_location)))


## Data outputs ====
if(writeOutputs == TRUE){
  st_write(LIculvertDataStatus_location, dsn = "outputs/tidalCulvertLocations.gpkg",
           layer = "tidalCrossingStatus", update  = TRUE, layer_options = "OVERWRITE=YES")
  # # This keeps failing to write to file. giving an error of GDAL Error 1:
  # st_write(LIculvertData_location, dsn = "outputs/tidalCulvertLocations.gpkg", 
  #          layer = "tidalCrossingData", update = TRUE, layer_options = c("OVERWRITE=YES"))
  
  writexl::write_xlsx(LIculvertData, path = paste0(tidalCulvert_outputs, "/compiledCulvert_data.xlsx"))
  # write.xlsx(LIculvertDataStatus_location %>% st_drop_geometry(), file = paste0(tidalCulvert_outputs, "/CulvertData_status.xlsx"), sheetName = "Culvert Status")
  writexl::write_xlsx(LIculvertDataStatus_location %>% 
               st_drop_geometry(), 
             path = paste0(tidalCulvert_outputs, "/AssessmentStatusList.xlsx"))
  # Save to TEAMS folder/file.
  writexl::write_xlsx(LIculvertDataStatus_location %>% 
               st_drop_geometry(), 
             path = "Z:/AssessmentStatusList.xlsx")
  write.table(crossings_TODO, file = paste0(tidalCulvert_outputs, "/CrossingsToVisit_2019.txt"), sep = ",", quote = F, col.names = F,
              row.names = FALSE, eol = ",")
  
} else{
  warning("NO Files are being written to Box folder")
}

# Pivot table ----
rpivotTable::rpivotTable(LIculvertDataStatus_location)


