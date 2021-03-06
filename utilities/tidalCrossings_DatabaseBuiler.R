# Database Creation
#'
#' The goal of this code block is to build a relational database in ArcGIS Pro using the arcgisbindings package to write data 
#' from the R env direct to the NYSPATIAL. Initially this will be a trial version for sharing data out, eventually this will 
#' replace the current data structure that's been used to date.
#' 
#' There are several tables that need to be joined through relations built from the CrossingID used. Each table will be assembled
#' individually, written to the gdb in ArcGIS, related and domains defined in ArcGIS Pro. (Unless another method is found...)
#' Tables to be created:
#' Crossings (spatial data) containing general info about the crossing, including owner, road info, etc,
#' Desktop - all the data generated within the desktop portion of the asssessments
#' FieldCollected- attribtues of the structure and some general field info
#' Longitudinal Profile - data on survey
#' CrossProfile - survey oof the crossing.

# gdb to write to:: 
#' NYSPATAIL -- gisdata/Projects/LI/Culvert_Assessment/CulvertPrioritizations/TidalCrossings_beta.gdb
#' LOCAL -- C:/Users/astarke/Documents/ArcGIS/Projects/Culverts_LongIsland/tidal_Crossings_beta.gdb

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
    streamName,
    Rd_Owner,
    Functional_Class,
    FunctionalClass_supervised,
    Evac_Rte,
    transportType,
    Estuary_Name,
    AssessmentStatusCode
  )

####### FIELD ASSESSMENTS ########## many (potentially)
# Data associated with the field visit
# CrossingID, date, observers, times, tides, ecological observations, (NH included the HWI, marsh plain etc, which I was calling the crossSectionalProfile data)

field_assessments <-
  LIculvertAssessmentData %>% st_drop_geometry() %>% 
  select(
    crossingID:AsmtStartTime,
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
  )

###### STRUCTURE CONDITION ###### (many structures and many visits)
# NH includes this info seperately, likely to allow for multiple crossing sturcutres at a single location
# All the condition scores, structure dimensions, scour scores, materials ec.
structure_assessmenst <-
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
  )

######## DESKTOP ASSESSMENTS ######### (perhaps many assessments over time but not immediately likely.)
#' Subset the attributes/columns needed for desktop assessments.
#' All fields stating with da_***, crossingID, marsh complex, formerlyconnected, 

desktop_assessment <- LIculvertAssessmentData %>% 
  st_drop_geometry() %>% 
  select(crossingID, 
         starts_with("da"), formerlyConnected)


######## SURVEY / ELEVATION DATA #####
#' Data related to the longitudinal profile and cross sectional profile. 
#' These data are tricky in that they aren't structured in a consistent manner and need
#' to be organized in a vertical table orientation, not a tidy single row as the other data are.
#' 
LIculvertAssessments %>% select(crossingID, longProfile) %>% filter(crossingID == 71)






## Check that all attributes have been captured. 
includedAttributes <- c(names(tidal_crossings), names(field_assessments), names(structure_assessmenst), names(desktop_assessment))

# Create a not in function
`%notin%` <- Negate(`%in%`)
# List out what attributes/columns aren't found in the datasets created above.
names(LIculvertAssessmentData)[names(LIculvertAssessmentData) %notin% includedAttributes]

