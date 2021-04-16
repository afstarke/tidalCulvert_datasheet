# Database Creation
#'
#' The goal of this code block is to build a relational database in ArcGIS Pro using the arcgisbindings 
#' package to write data from the R env direct to the NYSPATIAL. Initially this will be a trial 
#' version for sharing data out, eventually this will 
#' replace the current data structure that's been used to date.
#' 
#' There are several tables that need to be joined through relations built from the CrossingID used. 
#' Each table will be assembled individually, written to the gdb in ArcGIS, related and domains 
#' defined in ArcGIS Pro. (Unless another method is found...) 
#' Tables to be created:
#' Crossings (spatial data) containing general info about the crossing, including owner, road info, etc,
#' DesktopAssessment - all the data generated within the desktop portion of the asssessments
#' FieldAssessment- attribtues of the structure and some general field info
#' Longitudinal Profile - data on survey
#' CrossProfile - survey oof the crossing.
#' Prioritization

# gdb to write to:: 
#' NYSPATAIL -- gisdata/Projects/LI/Culvert_Assessment/CulvertPrioritizations/TidalCrossings_beta.gdb
#' LOCAL -- C:/Users/astarke/Documents/ArcGIS/Projects/Culverts_LongIsland/tidal_Crossings_beta.gdb

# Fields that will need to fall under domains in gdb.
fields_for_domains <- c(
  "InvasiveSppsPresent_upStream",
  "InvasiveSppsPresent_dwnStream",
  "MarshCondition_upStream",
  "MarshCondition_dwnStream",
  "CrossingType",
  "StructureConditionOverall",
  "StructureMaterial",
  "HeadwallCondition_dwnStream",
  "HeadwallCondition_upStream",
  "HeadwallMaterial_dwnStream",
  "HeadwallMaterial_upStream",
  "WingwallCondition_dwnStream",
  "WingwallCondition_upStream",
  "WingwallMaterials_dwnStream",
  "WingwallMaterials_upStream",
  "RoadSurfaceCondition",
  "Scour_inStructure",
  "ScourSeverity_dwnStream",
  "ScourSeverity_inStructure",
  "ScourSeverity_upStream",
  "ScourStructure_dwnStream",
  "ScourStructure_upStream",
  "NatCommClassification_upStream",
  "NatCommClassification_dwnStream"
)

# 'Condition' domain coding values start at 3 for some god-forsaken reason.
fields_for_domains2 <- c(
  "HeadwallCondition_dwnStream",
  "HeadwallCondition_upStream",
  "WingwallCondition_dwnStream",
  "WingwallCondition_upStream"
)


######## LOCATION DATA ######## unique location for each crossing
# Subset Town, Road info, crossingID (relationships all built from this ID), name, Estuary, <- stuff that doesn't change
# Domains needed for functional class and functional class supervised.
tidal_crossings <-
  LIculvertData_location %>% select(
    crossingID,
    roadName,
    Name,
    Municipality,
    Ownership,
    town_name,
    PtrPriorit,
    streamName,
    Road,
    Rd_Owner,
    Functional_Class,
    FunctionalClass_supervised,
    Evac_Rte,
    da_RoadWidth,
    transportType,
    Estuary_Name,
    AssessmentStatusCode
  ) %>% replace_with_na_at(.vars = fields_for_domains, condition = ~ .x == 1) %>%  # value 1 in source spreadsheets were coded as 'blanks' for cleanliness. Swap out to NA
  replace_with_na_at(.vars = fields_for_domains2, condition = ~ .x == 2) %>% 
  rename(roadWidth = da_RoadWidth)
####### FIELD ASSESSMENTS ########## many assessments (potentially)
# Data associated with the field visit
# CrossingID, date, observers, times, tides, ecological observations, (NH included the HWI, marsh plain etc, which I was calling the crossSectionalProfile data)


field_assessment <-
  LIculvertAssessmentData %>% st_drop_geometry() %>%
  select(
    crossingID,
    number_structures,
    dateAssessed:AsmtStartTime,
    GeneralAssessmentNotes,
    TidePredictElevationHigh,
    TidePredictElevationLow,
    TidePredictTimeHigh,
    TidePredictTimeLow,
    InvasiveSppsPresent_upStream,
    InvasiveSppsPresent_dwnStream,
    MarshCondition_upStream,
    MarshCondition_dwnStream,
    NatCommClassification_upStream,
    NatCommClassification_dwnStream,
    NatCommClass_Comments,
    InvasiveSppsPresent_upStream,
    InvasiveSppsPresent_dwnStream,
    MarshCondition_upStream,
    MarshCondition_dwnStream,
    Vegchoice
  ) %>%
  mutate(
    dateAssessed = as.POSIXct.Date(dateAssessed),
    NatCommClassification_upStream = as.integer(NatCommClassification_upStream),
    NatCommClassification_dwnStream = as.integer(NatCommClassification_dwnStream)
  ) %>%
  replace_with_na_at(.vars = fields_for_domains, condition = ~ .x == 1) %>% # value 1 in source spreadsheets were coded as 'blanks' for cleanliness. Swap out to NA
  replace_with_na_at(.vars = fields_for_domains2, condition = ~ .x == 2) 

###### STRUCTURE CONDITION ###### (many structures and many visits)
# NH includes this info seperately, likely to allow for multiple crossing sturcutres at a single location
# All the condition scores, structure dimensions, scour scores, materials ec.
structure_assessment <-
  LIculvertAssessmentData %>% st_drop_geometry() %>%
  select(
    crossingID,
    CrossingType,
    StructureConditionOverall, 
    StructureMaterial,
    TideGate,
    CrosDim_dwnA:CrosDim_upD,
    CrossingType_conditionComts,
    HeadwallCondition_dwnStream:HeadwallMaterial_upStream,
    WingwallCondition_dwnStream:WingwallMaterials_upStream,
    HighTidePerch_upStream,
    HighTidePerch_dwnStream,
    LowTidePerch_upStream,
    LowTidePerch_dwnStream, 
    RoadSurfaceCondition, 
    Scour_inStructure:ScourStructure_upStream
  ) %>% replace_with_na_at(.vars = fields_for_domains, condition = ~.x == 1) %>%  # value 1 in source spreadsheets were coded as 'blanks' for cleanliness. Swap out to NA
  replace_with_na_at(.vars = fields_for_domains2, condition = ~ .x == 2) %>% 
  replace_with_na_at(.vars = c("ScourStructure_dwnStream", "ScourStructure_upStream"), condition = ~ .x == 0)
######## DESKTOP ASSESSMENTS ######### (perhaps many assessments over time but not immediately likely.)
#' Subset the attributes/columns needed for desktop assessments.
#' All fields stating with da_***, crossingID, marsh complex, formerlyconnected, 

desktop_assessment <- LIculvertAssessmentData %>% 
  st_drop_geometry() %>% 
  select(crossingID, 
         starts_with("da"), formerlyConnected, MarshComplex_Scoring, -da_RoadWidth, -dateAssessed)


######## SURVEY / ELEVATION DATA #####
# TODO: Empty tibbles in nested column causing grief. Also, a new error message arose relating to a character issue.
#' Data related to the longitudinal profile and cross sectional profile. 
#' These data are tricky in that they aren't structured in a consistent manner and need
#' to be organized in a vertical table orientation, not a tidy single row as the other data are.
#' 
longitudinalSurvey <- LIculvertAssessments %>% 
  select(longProfile) %>% 
  filter(!is.null(longProfile)) %>% unnest(longProfile) %>% 
  select(crossingID, everything())

crossSectionalFieldSurvey <- LIculvertAssessments %>% 
  select(crossingID, crossSectionProfile) %>% 
  filter(!is.null(crossSectionProfile)) %>% unnest(crossSectionProfile) 

crossSectional_corrected <- LIculvertAssessmentData %>% 
  select(crossingID, HeightOfRoadCenter:HtofControlPt_X, LowTideWaterElev_dwnStream:LowTideWaterElev_upStreamCode, roadWidth, `Ceiling of Structure DS`:`Marsh Plain Shot US`, `Road Center NA`:`Road Surface US`, crossSection_hts_coms )

prioritizations_tidal <- LIculvertPrioritization %>% st_drop_geometry()

## Check that all attributes have been captured. 
includedAttributes <-
  c(
    names(tidal_crossings),
    names(field_assessment),
    names(structure_assessment),
    names(desktop_assessment),
    names(longitudinalSurvey), # TODO: Determine best method for the structure of table.
    names(crossSectionalFieldSurvey), # TODO: Determine best method for the structure of table. 
    names(crossSectional_corrected) # TODO: Determine best method for the structure of table.
  )

# Create a not in function
`%notin%` <- Negate(`%in%`)
# List out what attributes/columns aren't found in the datasets created above.
# names(LIculvertAssessmentData)[names(LIculvertAssessmentData) %notin% includedAttributes]
# names(LIculvertAssessmentData)[names(LIculvertAssessmentData) %notin% keysheet$dataName]

# 
# 
writeGIS <-  FALSE
if(writeGIS){
  ## Generate the geoDB from the tibbles above.
  arcgisbinding::arc.check_product()
  # C:\Users\astarke\Documents\ArcGIS\Projects\Culverts_LongIsland\tidal_Crossings_beta.gdb
  gdbpath <- "C:/Users/astarke.TNC/Documents/ArcGIS/Projects/Culverts_LongIsland/tidal_Crossings.gdb"
  
  # Write the feature class that is the crossing locations
  arc.write(path = file.path(gdbpath, "tidalCrossings"), data = tidal_crossings, overwrite = TRUE)
  
  # write Tables for joining. ----
  # Prioritizations  prioritizations_tidal
  arc.write(path = file.path(gdbpath, "tidalPrioritizations"), data = prioritizations_tidal, overwrite = TRUE)
  # Field data - field_assessment
  arc.write(path = file.path(gdbpath, "fieldAssessments"), data = field_assessment, overwrite = TRUE)
  # Desktop assessments  desktop_assessment
  arc.write(path = file.path(gdbpath, "desktopAssessment"), data = desktop_assessment, overwrite = TRUE)
  # Structure assessment structure_assessment
  arc.write(path = file.path(gdbpath, "structureAssessment"), data = structure_assessment, overwrite = TRUE)
  
}else{
  print("Nothing written to .gdb")
}


# Metadata--
# Data dictionary arranging.
#'
#' The data that feeds into this table is pulled form the 'keysheet' data located in the Box folder
#' No edits should be made to the field names without rolling those edits up into the code contained within 
#' this project. Renaming of fields can be made while writing out the objects to ESRI gdb above.
#' 

keysheet <- read_excel("../../../Box/Culvert Assessment/Tidal Assessments/key.xlsx") # this is read in RmdSetup.R as well. Add in here to avoid having to rerun that full code each update.

masterMeta <- includedAttributes %>% as_tibble() %>% rename(FieldName = value) %>% 
  mutate(tableOrigin = case_when(FieldName %in% names(tidal_crossings) ~ "tidalCrossings",
                                 FieldName %in% names(field_assessment) ~ "fieldAssessments",
                                 FieldName %in% names(structure_assessment) ~ "structureAssessment",
                                 FieldName %in% names(desktop_assessment) ~ "desktopAssessment",
                                 FieldName %in% names(longitudinalSurvey) ~ "longitudinalSurvey", 
                                 FieldName %in% names(crossSectionalFieldSurvey) ~ "crossSectionalFieldSurvey",
                                 FieldName %in% names(crossSectional_corrected) ~ "crossSectional_corrected",
                                 TRUE ~ "Not included")) %>% left_join({keysheet %>% select(dataName, `Data Dictionary`)}, by = c("FieldName" = "dataName")) 
write_xlsx(masterMeta, "datadictionary.xlsx", col_names = T)

