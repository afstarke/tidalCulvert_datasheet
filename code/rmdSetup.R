# Rmd setup

## @knitr directorySetup
# 
# Setup the folders for pulling and storing
# Folder where workiung copies of assessment workbooks live
# tidalCulvert_datasheetsFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/Tidal Culvert DataSheets_WORKINGvers/"
# Began QA process on these files and moved files into new QAed folder
tidalCulvert_datasheetsFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/TidalAssessmentWorkbooks_QAvers/"

crossingTracker <- read_excel("Z:/Culvert_Field_Visit_Schedule.xlsx") %>% 
  as.tibble() 

crossings_TODO <- crossingTracker %>% select(crossingID) %>% unlist()

# Folder to write outputs to.
tidalCulvert_outputs <- "../../../Box Sync/Culvert Assessment/Tidal Assessments"
# Master document that was used for tracking data needs etc.
tidalCulvert_Checklist <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/MASTERCrossingSpreadsheetChecklistEdit.xlsx"
# key sheet- for decoding the values extracted from the workbooks. Add cells of interest and what to call the data upon it's exctraction here.
keysheet <- read_excel("../../../Box Sync/Culvert Assessment/Tidal Assessments/key.xlsx")
# Crossing data that Karen and Stephen put together for tidal assessments.
culvertPts_path <- "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/TidalCrossings_latest.shp" # Direct path to the shapefile used as base. Change as needed. # UPDATE:
# Priority marsh features used to build the original set of crossing locations.

priorityMarshes_gdb <- "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/Tidal_Crossing_Data.gdb"
# Priority marsh shapefile: D:\gisdata\Projects\LI\Culvert_Assessment\data\Tidal Crossings\Tidal_Crossing_Data.gdb\MarshComplex_PriorityAreas 
# Filter for high priority - Priority == “High” 

# Potential culvert locations:
culvertPOTpts <- "M:/Projects/LI/Culvert_Assessment/data/Potential or Missing culverts - latest.lyr"

# huc8 <- "M:/Base_Data/Hydro/Watersheds/sde_tnc_viewer.sde" # TODO: Add proper link to HUC watersheds

# GIS data

## @knitr spatialDataLoad
# Read in shapefile containing identified crossing locations with attributes related to the priorities
# read in as sf and transform to WGS84 
# add a few attributes/fields for prioritization and tracking 'field assessment schedules'
LIculvert_GISpts <- st_read(culvertPts_path) %>% st_transform(4326) %>% 
  st_zm(drop = TRUE) %>% 
  mutate(crossingID = as.character(Tidal_ID)) %>% # For joining on this attribute later.
  mutate(PriorityScore = as.numeric(PtrPriorit) + as.numeric(TNCPriorit) + as.numeric(MarPriorit)) %>% # create a new 'priority score'
  select(Ownership, Name, crossingID, MarshCompl, PtrPriorit, TNCPriorit, MarPriorit, PriorityScore) %>% 
  mutate(ToBeAssessed_2019 = crossingID %in% crossings_TODO, Located = "Y",
         crossingID = as.numeric(crossingID)) 
# SDE <- "M:/Projects/LI/Culvert_Assessment/data/LI_collector@nyspatial.tnc.org.sde"

# culvertSDE <- st_read("SDE:M:/Projects/LI/Culvert_Assessment/data, LI_collector,nyspatial.tnc.org.sde")
# LI_collector@nyspatial.tnc.org.sde\tncgdb.LI_COLLECTOR.TidalCrossings_latest_prj
# st_layers("SDE:nyspatial.tnc.org,LI_collector,astarke,2uningVWs&Teles!")

# 
priorityMarshes <- st_read(priorityMarshes_gdb, layer = "MarshComplex_PriorityAreas") %>% st_transform(4326)


# Identify points that mau cause issues...
# errorPts <- LIculvert_GISpts %>% filter(Latitude < 30) %>% select(crossingID) %>% st_set_geometry(NULL)
# Check for duplicate points
duplicatePts <- LIculvert_GISpts %>% 
  group_by(crossingID) %>% 
  filter(n()> 1) %>% 
  select(crossingID) %>%  
  st_set_geometry(NULL) %>% as_vector()
  


## @knitr culvertData
# Extract the field and desktop data from the workbooks and bring them in.
# Key functions
source("functions/culvert_tidy.R")
source("functions/culvert_extract.R")    

# Create a nested dataframe with filenames, file paths, and tidied cells from tidal assessment workbooks,
# columns include tidycells- raw cell content from each workbook; decoded- transformed raw values extracted from 
# workbooks using key as lookup; 
# This tibble should be the same length as the number of worksheets in the folder where the data is stored. 
LIculvertsAssessments <- culvert_tidy(tidalCulvert_datasheetsFolder) %>% 
  mutate(decoded = map(.x = tidycells, .f = ~decodeSheet(.x, keysheet))) # this is where the sausage is made. 


# unnest the tidyxl data to create a large tidy dataframe
LIculvertData <- LIculvertsAssessments %>% 
  select(filenames, lastChanges, decoded) %>% 
  unnest() %>% select(filenames, lastChanges, dataName, values) %>% 
  spread(key = dataName, value = values) %>% 
  select(filenames, crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
  mutate(dateAssessed2 = lubridate::parse_date_time(dateAssessed, orders = 'ymd'),
         AsmtStartTime = format(AsmtStartTime, format = "%H:%M:%S"),
         AsmtEndTime = format(AsmtEndTime, format = "%H:%M:%S"),
         crossingID = as.numeric(crossingID))
# TODO: mutate outlet to Atlantic and outlet Subtidal to be one column each not 2 as in the datasheet.
# TODO: Add column in key sheet that will be used as data dictionary. Select and paste that info to sheet 2 of the culvert data output below.


if(typeof(LIculvertData$crossingID) == typeof(LIculvert_GISpts$crossingID)){ # Are the two columns the same datatype?
  matchedLIculverts <- LIculvertData %>% filter(crossingID %in% LIculvert_GISpts$crossingID) %>% select(filenames, crossingID) # Culvert datasheets that ARE IN GIS point dataset
  missingLIculverts <- LIculvertData %>% filter(!crossingID %in% LIculvert_GISpts$crossingID) %>% select(filenames, crossingID) # Culvert datasheets that ARE NOT IN GIS points.
} else{
  stop("CrossingID not same data type (character vs numeric)")
}

LIculvertData_location <- LIculvert_GISpts %>% left_join(LIculvertData, by = "crossingID")  # this object is picking up 2 extra joins- perhaps dupicate CrossingIDs? YUP> 2007 is duplicated in GIS file.
  

fieldVars <- keysheet %>% filter(AssessmentType == 'Field_min') %>%  pluck('dataName') %>% unlist()
 

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
                                          maxPoolWidth_upStream, 
                                          maxPoolWidth_dwnStream)))/4 == 1,
         MissingCatchmentAttributes = sum(!is.na(c(WatershedArea_upStream, 
                                            saltMarshArea, 
                                            WatershedLandCover_wetland, 
                                            WatershedLandCover_forested, 
                                            WatershedLandCover_impervious, 
                                            WatershedLandCover_developed)))/6 == 1,
         MissingMarshMigrationPotential = sum(!is.na(c(marshMigrationPotential_acres, 
                                                marshMigrPot_evalUnit, 
                                                NWI_class_upStream, 
                                                NWI_class_dwnStream)))/4 == 1) %>%
  replace_na(list(FieldAssessmentComplete = "N", DesktopAssessmentComplete = "N", FullAssessmentComplete = "N")) %>% 
  select(filenames, lastChanges, crossingID, FieldAssessmentComplete, 
         DesktopAssessmentComplete, FullAssessmentComplete, MissingFromAssessment, observers, 
         dateAssessed, photoStorageLocation, CrossingTypeIDed, MissingchannelPoolWidths, MissingCatchmentAttributes, 
         MissingMarshMigrationPotential)

LIculvertDataStatus_location <- LIculvert_GISpts %>% 
  left_join(LIculvertDataStatus, by = "crossingID") %>% 
  mutate(missingWorkbook = is.na(filenames)) %>% 
  replace_na(list(FieldAssessmentComplete = "N", DesktopAssessmentComplete = "N", FullAssessmentComplete = "N")) %>% 
  select(crossingID, ToBeAssessed_2019:photoStorageLocation, MissingchannelPoolWidths:MissingMarshMigrationPotential)

AssessmentList <- LIculvertDataStatus_location #%>% filter(ToBeAssessed_2019 == TRUE)

## Data outputs

write.xlsx(LIculvertData, file = paste0(tidalCulvert_outputs, "/compiledCulvert_data.xlsx"), sheetName = "Compiled Culvert data")
# write.xlsx(LIculvertDataStatus_location %>% st_drop_geometry(), file = paste0(tidalCulvert_outputs, "/CulvertData_status.xlsx"), sheetName = "Culvert Status")
write.xlsx(AssessmentList %>% 
             st_drop_geometry(), 
           file = paste0(tidalCulvert_outputs, "/AssessmentStatusList.xlsx"), 
           sheetName = "AssessmentStatusLIST", append = FALSE, row.names = FALSE)
write.xlsx(AssessmentList %>% 
             st_drop_geometry(), 
           file = "Z:/AssessmentStatusList.xlsx", 
           sheetName = "AssessmentStatusLIST", append = FALSE, row.names = FALSE)
write.table(crossings_TODO, file = paste0(tidalCulvert_outputs, "/CrossingsToVisit_2019.txt"), sep = ",", quote = F, col.names = F,
                  row.names = FALSE, eol = ",")

# Write GeoPackage too!
# st_write(LIculvertData_location, dsn = "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/TidalCrossing_collector.gpkg", delete_layer = TRUE)
