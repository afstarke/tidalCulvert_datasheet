# Rmd setup

## @knitr directorySetup
# 
# Setup the folders for pulling and storing
tidalCulvert_datasheetsFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/Culvert Excel Sheets"
tidalCulvert_outputs <- "../../../Box Sync/Culvert Assessment/Tidal Assessments"
tidalCulvert_Checklist <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/MASTERCrossingSpreadsheetChecklistEdit.xlsx"
keysheet <- read_excel("../../../Box Sync/Culvert Assessment/Tidal Assessments/key.xlsx")
culvertPts_path <- "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/TidalCrossings_latest.shp" # Direct path to the shapefile used as base. Change as needed. # UPDATE:
ddmt <- "M:/Projects/LI/Marsh_DMMT/data/DMMT.gdb"
culvertPOTpts <- "M:/Projects/LI/Culvert_Assessment/data/Potential or Missing culverts - latest.lyr"
# huc8 <- "M:/Base_Data/Hydro/Watersheds/sde_tnc_viewer.sde" # TODO: Add proper link to HUC watersheds

# GIS data

## @knitr spatialDataLoad
# Read in shapefile as sf
LIculvert_GISpts <- st_read(culvertPts_path) %>% st_transform(4326) %>% 
  st_zm(drop = TRUE) %>% 
  mutate(crossingID = as.character(Tidal_ID)) %>% # For joining on this attribute later.
  mutate(PriorityScore = as.numeric(PtrPriorit) + as.numeric(TNCPriorit) + as.numeric(MarPriorit)) %>% # create a new 'priority score'
  select(Ownership, Name, Latitude, Longitude, crossingID, MarshCompl, PtrPriorit, TNCPriorit, MarPriorit, PriorityScore)

# hucWatersheds <- st_read(huc8)
priorityMarshes <- st_read(ddmt) %>% st_transform(4326)

# Identify points that have issues. . 
errorPts <- LIculvert_GISpts %>% filter(Latitude < 30) %>% select(crossingID) %>% st_set_geometry(NULL)
duplicatePts <- LIculvert_GISpts %>% group_by(crossingID) %>% filter(n()> 1) %>% select(crossingID) %>%  st_set_geometry(NULL)
  


## @knitr culvertData
# Extract the field and desktop data from the workbooks and bring them in.
# Key functions
source("functions/culvert_tidy.R")
source("functions/culvert_extract.R")    

# Create a nested dataframe with filenames, file paths, and tidied cells from tidal assessment workbooks
# This tibble should be the same length as the number of worksheets in the folder where the data is stored. 
LIculvertsAssessments <- culvert_tidy(tidalCulvert_datasheetsFolder) %>% 
  mutate(decoded = map(.x = tidycells, .f = ~decodeSheet(.x, keysheet))) # this is where the sausage is made. 


# unnest the tidyxl data to create a large tidy dataframe
LIculvertData <- LIculvertsAssessments %>% 
  select(filenames, decoded) %>% 
  unnest() %>% select(filenames, dataName, values) %>% 
  spread(key = dataName, value = values) %>% 
  select(filenames, crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
  mutate(dateAssessed2 = lubridate::parse_date_time(dateAssessed, orders = 'ymd'))



if(typeof(LIculvertData$crossingID) == typeof(LIculvert_GISpts$crossingID)){ # Are the two columns the same datatype?
  matchedLIculverts <- LIculvertData %>% filter(crossingID %in% LIculvert_GISpts$crossingID) %>% select(filenames, crossingID) # Culvert datasheets that ARE IN GIS point dataset
  missingLIculverts <- LIculvertData %>% filter(!crossingID %in% LIculvert_GISpts$crossingID) %>% select(filenames, crossingID) # Culvert datasheets that ARE NOT IN GIS points.
} else{
  stop("CrossingID not same data type (character vs numeric)")
}

LIculvertData_location <- LIculvert_GISpts %>% left_join(LIculvertData, by = "crossingID") # this object is picking up 2 extra joins- perhaps dupicate CrossingIDs? YUP> 2007 is duplicated in GIS file.

fieldVars <- keysheet %>% filter(DataSource == 'Field Collected') %>%  pluck('dataName') %>% unlist()
 

LIculvertDataStatus <- LIculvertsAssessments %>% 
  select(filenames, lastChanges, filePath, decoded) %>% 
  unnest() %>% 
  select(filenames, lastChanges, filePath, dataName, values) %>%
  spread(key = dataName, value = values) %>% 
  select(filenames, crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
  mutate(fieldCompletion = rowSums(!is.na(select(., one_of(fieldVars))))/length(fieldVars) - 0.488888888888889) %>% # Calculate proportion of NAs on variables marked as 'Field Collected' from key sheet.
  rowwise() %>% 
  mutate(channelPoolWidths = sum(!is.na(c(ChannelWidth_upStream, 
                                          ChannelWidth_dwnStream, 
                                          maxPoolWidth_upStream, 
                                          maxPoolWidth_dwnStream)))/4,
         CatchmentAttributes = sum(!is.na(c(WatershedArea_upStream, 
                                            saltMarshArea, 
                                            WatershedLandCover_wetland, 
                                            WatershedLandCover_forested, 
                                            WatershedLandCover_impervious, 
                                            WatershedLandCover_developed)))/6,
         MarshMigrationPotential = sum(!is.na(c(marshMigrationPotential_acres, 
                                                marshMigrationPotential_evalUnit, 
                                                NWI_class_upStream, 
                                                NWI_class_dwnStream)))/4,
         
         fieldAssessed = ifelse(fieldCompletion != 1 | is.na(fieldCompletion), FALSE, TRUE), # this isn't the best approach. Transition to extracting from datahseet.
         DesktopCompletion = sum(channelPoolWidths, CatchmentAttributes, MarshMigrationPotential)/3,
         DesktopComplete = ifelse(sum(channelPoolWidths, CatchmentAttributes, MarshMigrationPotential) == 3, "YES", "NO")) %>%
  select(filenames, lastChanges, crossingID, fieldAssessed, fieldCompletion, dateAssessed, observers, channelPoolWidths, CatchmentAttributes, MarshMigrationPotential, DesktopCompletion, DesktopComplete)

LIculvertDataStatus_location <- LIculvert_GISpts %>% 
  left_join(LIculvertDataStatus, by = "crossingID") %>% mutate(missingWorkbook = is.na(filenames))


## Data outputs

write.xlsx(LIculvertData, file = paste0(tidalCulvert_outputs, "/compiledCulvert_data.xlsx"), sheetName = "Compiled Culvert data")
write.xlsx(LIculvertDataStatus_location %>% st_drop_geometry(), file = paste0(tidalCulvert_outputs, "/CulvertData_status.xlsx"), sheetName = "Culvert Status")
