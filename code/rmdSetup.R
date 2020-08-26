# Rmd setup


## @knitr directorySetup  ----
# 
source("00_libraries.R")
source("functions/culvert_tidy.R")
source("utilities/photoMatchTable.R")
source('~/R/roadstreamCrossings/utilities/helpers.R')
# Data Update:
## Update the Data?!
dataUpdate <- 1
spatialDataUpdate <- 1
pivots <- 0
# Write outputs to folders option- Change to allow writes to be made- 
# Keep false when running many times to avoid conflicts with Box

writeOutputs = dataUpdate
arcgisbinding::arc.check_product()
arcgisbinding::arc.check_portal()


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

# Field schedule from Teams.----
crossingTracker <- read_excel("../../../The Nature Conservancy/Long Island Road-Water Culvert Assessments - General/Culvert_Field_Visit_Schedule.xlsx", sheet = "Tidal Priorities") 
## folder OUTPUTS 
# Teams folder
# Mapped locally using the PIA method of browsing to Teams link in Internet Explorer, then mapping drive link to Z: in my PC
# Box folder for outputs
tidalCulvert_outputs <- "../../../Box Sync/Culvert Assessment/Tidal Assessments"

# pivots <- menu(c("Create pivot tables?", "Proceed without?"), title = "Generate a pivot summary for reviewing field assessment schedule?")


# Vector listing the crossings that are on the 'Field schedule' on Teams ----
crossings_TODO <- crossingTracker %>% select(crossingID) %>% unlist()
# Subset list for checking on status.
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

# Culvert Catchments ----
  # ALL DATA contained in these catchments that was relevant has been transferred to the tidal_desktop points themselves. If updates needed rerun this and rejoin missing values to hosted feature.
# if(spatialDataUpdate == 1){
  # catchments <- st_read("M:/Projects/LI/Culvert_Assessment/data/Tidal_Desktop_Assessment/Tidal_Catchments.gdb", layer = "MASTER_TidalCatchments_Latest")
#   catchments %>% write_rds(path = "data/LIculvert_Catchments.rds")
# }else{
  # catchments <- read_rds("data/LIculvert_Catchments.rds")
#   }
    

  # Culvert locations ----
  #' 
  #' Complete list of culvert locations and desktop assessment data housed in ArcGIS Online TidalCrossing_desktopDataEntry hosted feature serice. 
  #' ALL data additions, edits updates etc, for culvert locations and desktop assessment happens here.
  #'  
if(dataUpdate == 1){
 
  # Pull from AGOL directly using credentials contained in the arcgisbindings arc.check_portal call
  tidalCrossings_desktop <- arcgisbinding::arc.open("https://services.arcgis.com/F7DSX1DSNSiWmOqh/arcgis/rest/services/TidalCrossing_desktopDataEntry/FeatureServer/0") %>% 
    arc.select() %>% 
    arcgisbinding::arc.data2sf()
    
  # culvertPOTpts <- tidalCrossings_desktop #%>% select(crossingID:Latitude) # trim desktop data?
  # # remove to keep the workspace tidy.
  # rm(tidalCrossings_desktop)
  LIculvert_GISpts <- tidalCrossings_desktop %>% st_transform(4326) %>% 
    st_zm(drop = TRUE) %>% 
    mutate(crossingID = CrossingID_, PriorityScore = as.numeric(PtrPriorit) + as.numeric(TNCPriorit_1) + as.numeric(MarPriorit_1)) %>% # create a new 'priority score'
    mutate(listedOnFieldSched = crossingID %in% crossings_TODO, Located = "Y",
           crossingID = as.numeric(crossingID)) 
  LIculvert_GISpts %>% write_rds(path = "data/LIculvert_GISpts.rds")
  rm(tidalCrossings_desktop)
} else{
  LIculvert_GISpts <- read_rds("data/LIculvert_GISpts.rds")
}
#' roadMeas- used below to add in road widths and height as columns in nested dataframe (to be then used in calculations) that 
#' is needed for longitudinal profile plots.
roadMeas <- LIculvert_GISpts %>% select(crossingID, da_LiDarHt_CL, da_RoadWidth) %>% st_drop_geometry() %>%
  mutate(crossingID = as.character(crossingID))

## @knitr culvertData  ----
  #' Extract the field data from the workbooks and bring them in as an tidy nested dataframe
  #' Key functions for this are found in the functions folder.
  #' This dataframe functions as the primary container for the field data currently housed in the many excel files on Box.
source("functions/culvert_tidy.R") 
source("functions/culvert_extract.R")    
source(here::here("summarySheets", "tidal_longitudinalPlots.R"))
if(dataUpdate == 1) {
  # Create a nested dataframe with filenames, file paths, and tidied cells from tidal assessment workbooks,
  # columns include tidycells- raw cell content from each workbook; decoded- transformed raw values extracted from
  # workbooks using key as lookup;
  # This tibble should be the same length as the number of worksheets in the folder where the data is stored.
  # TODO: Check that the fielddata column contains the same data as the LIculvertData object- if so remove the later from this script
  LIculvertAssessments <-
    culvert_tidy(tidalCulvert_datasheetsFolder)
  # Build out dataframe to add crossings longitudinal profiles, cross sectional measures, 
  LIculvertAssessments <- LIculvertAssessments %>%
    mutate(crossingID = map_chr(
      .x = tidycells,
      ~ culvert_extract(
        tidycells = .x,
        sheetOI = 'Data Sheet - SITE',
        celladdress = 'L7'
      )
    )) %>%
    # join in the LIDAR road heights and road widths from the desktop collected data. That data stored in ArcGIS Online service feature
    left_join(roadMeas, by = "crossingID") %>%
    mutate(
      # this is where the sausage is made. decodeSheet pulls needed cells from excel sheets, names the data appropriately, and addes it to dataframe stored in 'decoded'
      decoded = map(.x = tidycells, .f = ~ decodeSheet(.x, keysheet)),
      # Clean up some data formating etc.
      fielddata = map(.x = decoded, .f = ~ cleanField(.x)),
      # longitudinal profile data
      longProfile = pmap(
        .l = list(
          filepath = filePath,
          tidycells = tidycells,
          lidarHt = da_LiDarHt_CL
        ),
        .f = ~ channelLongidinalProfile_extract(
          filepath = ..1,
          tidycells =  ..2,
          lidarHts = ..3
        )
      ),
      
      crossSectionProfile = pmap(
        .l = list(
          filepath = filePath,
          tidycells = tidycells,
          lidarMeasure = da_LiDarHt_CL,
          roadWidth = da_RoadWidth
        ),
        .f = ~ crossSection(
          filepath = ..1,
          tidycells = ..2,
          lidarMeasure = ..3,
          roadWidth = ..4
        )
      ),
      
      profilePlots = map2(
        .x = longProfile,
        .y = crossSectionProfile,
        .f = ~ drawCrossing(longitudinal = .x, crossSectional = .y)
      )
      
    )
  LIculvertAssessments %>% write_rds(path = "data/LIculvertAssessments.rds") # LIculvertAssessments 
} else{
  LIculvertAssessments <-
    read_rds(path = "data/LIculvertAssessments.rds")
}

#   # Culvert Field Assessment Data ----
LIculvertData <- LIculvertAssessments %>% select(fielddata) %>% unnest(cols = c(fielddata))


# if(dataUpdate == 1){
#   # unnest the tidyxl data to create a large tidy dataframe
#   LIculvertData <- LIculvertAssessments %>% 
#     select(filenames, lastChanges, decoded) %>% 
#     unnest() %>% select(filenames, lastChanges, dataName, values) %>% 
#     spread(key = dataName, value = values) %>% 
#     select(filenames, crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
#     mutate(dateAssessed2 = lubridate::parse_date_time(dateAssessed, orders = 'ymd'),
#            AsmtStartTime = format(AsmtStartTime, format = "%H:%M:%S"),
#            AsmtEndTime = format(AsmtEndTime, format = "%H:%M:%S"),
#            crossingID = as.numeric(crossingID)) %>% 
#     mutate_if(is.character, list(~na_if(., "N/A"))) %>%   # Convert character columns with "N/A" to NA
#     mutate_at(numericVars, as.numeric) %>% 
#     mutate_at(logicalVars, as.logical)
#   # DONE: mutate outlet to Atlantic and outlet Subtidal to be one column each not 2 as in the datasheet.
#   # DONE: Add column in key sheet that will be used as data dictionary. Select and paste that info to sheet 2 of the culvert data output below.
#   
#   # Munge and add in the vegetation matrix selections
#   vegMats <- LIculvertData %>% 
#     select(crossingID, starts_with("veg")) %>% 
#     gather(-crossingID, key = 'key', value = "val") %>% 
#     separate(col = key, into = c("key", "Vegchoice")) %>% 
#     mutate(VegetMat_select = ifelse(val, yes = val, no = NA)) %>% 
#     filter(!is.na(VegetMat_select)) %>% select(crossingID, Vegchoice) 
#   LIculvertData <- LIculvertData %>% left_join(vegMats) %>% select(-starts_with("vegMat"))
#   
#   
#   LIculvertData %>% write_rds(path = "data/LIculvertData.rds")
# }else{
#   LIculvertData <- read_rds(path = "data/LIculvertData.rds")
# }


LIculvertData_location <- LIculvert_GISpts %>% 
  left_join(LIculvertData, by = "crossingID")  # this object is picking up 2 extra joins- perhaps dupicate CrossingIDs? YUP> 2007 is duplicated in GIS file.
  
# Culvert Data Status ----
#' summaries for general project guidance
#' Mostly unused now as project transitions to prioritizations.

if(dataUpdate == 1){
  LIculvertDataStatus <- LIculvertAssessments %>% 
    select(filenames, lastChanges, filePath, decoded) %>% 
    unnest(decoded, .drop = FALSE) %>% 
    select(filenames, lastChanges, filePath, dataName, values) %>%
    spread(key = dataName, value = values) %>% 
    select(filenames, crossingID, dateAssessed, observers, everything()) %>% # organize the order of the columns.
    mutate(CrossingTypeIDed = ifelse(CrossingType >= 2, yes = 1, no = 0), 
           crossingID = as.numeric(crossingID)) %>% 
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
  write_rds(LIculvertData_location, path = "data/LIculvertData_location.rds")
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
             path = "../../../The Nature Conservancy/Long Island Road-Water Culvert Assessments - General/AssessmentStatusList.xlsx")
  # write.table(crossings_TODO, file = paste0(tidalCulvert_outputs, "/CrossingsToVisit_2019.txt"), sep = ",", quote = F, col.names = F,
  #             row.names = FALSE, eol = ",")
  
} else{
  warning("NO Files are being written to Box folder")
}



# # GIS ouputs for Karen- M:\Projects\LI\Culvert_Assessment\data\Tidal Crossings
# LIculvertDataStatus_location %>% select(crossingID, FieldAssessmentComplete) %>%
#   mutate(AssessmentStatus = if_else(FieldAssessmentComplete  != "N" | FieldAssessmentComplete == "Y", 
#                                     true = "Assessment Complete", 
#                                     false = "Assessment Incomplete")) %>% 
#   st_write(dsn = "M:/Projects/LI/Culvert_Assessment/data/Tidal Crossings/tidalFieldAssessmentStatus.shp", delete_layer=TRUE)



# # DataExplorer reports
# config <- configure_report(
#   global_ggtheme = quote(theme_ipsum(base_size = 12))
# )
# create_report(data = tidalCrossings_desktop %>% st_drop_geometry(), config = config, output_file = "Desktop data report.html")
# create_report(data = LIculvertData, output_file = "Tidal culvert data: Field.html", config = config)
# create_report(data = st_drop_geometry(LIculvertDataStatus_location), output_file = "Tidal Culvert Data.html", config = config)

