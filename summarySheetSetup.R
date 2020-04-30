# Summary Sheet Setup
# Source this code when needing an update. 

source(here::here("00_libraries.R"))
##%######################################################%##
#                                                          #
####                Summary Sheet Info                  ####
#                                                          #
##%######################################################%##

## @knitr summaryLoad
#' Excel file located in Teams-General tab for ease of editing names etc. 
#' Workflow:
#' 1) Read in the excel which has:
#' field_Name == the name of the field/attribute that contains the data
#' needed == is the field needed? A way to add/remove fields easily.
#' SummarySection == Which section of the summary sheet does this data fit
#' protocol == Fresh or Tidal. Just to keep it to one file and used to filter.
#' dataType == start of a metadata field but mainly possibly used to change formatting in summary sheet (calculated might appear different than score)
#' Description == What text to use in describing the field. This is where the editing can happen.
summaryNeeds <- read_xlsx(path = here::here("../../../The Nature Conservancy/Long Island Road-Water Culvert Assessments - General/SummarySheet buildout/summarySheetDataLinks.xlsx"))

#' 2) Filter the prioritization data for the proper protocol and to the crossing of interest.
#' 3) Munge data as needed to get in proper format.
#' 4) transpose/rotate the filed data to set it up in long format
#' 5) Join the summaryNeeds to the field data (fw_doc_data or tidal_doc_data) by the 


# ##%######################################################%##
#                                                          #
####                       Tidal                        ####
#                                                          #
##%######################################################%##

## @knitr tidalDataRead 

#' load the Prioritization data
#' which contains scoring and metrics for scoring.
# TODO: Add tidal to the obect name to distinguish
LIculvertPrioritization <- read_rds(here::here("data", "/LIculvertPrioritizations.rds"))
# load the assessment data which contains some additional metrics and info used in the summary sheets.
LIculvertAssessmentData <- read_rds(here::here("data", "/LIculvertAssessmentData.rds"))

# Pull in sheet with meta info and field headings needed
# tsdn = Tidal summary data needs.
tsdn <- summaryNeeds %>% 
  filter(protocol == "Tidal") %>% 
  filter(needed == "Y") %>% 
  select(field_Name, Description, SummarySection)

write_rds(tsdn, here::here("data/tsdn.rds"))
# IDEA: 1st step: filter, select the proper columns needed for the summary sheet, with column identifying the section that it will land. 
# 2nd step: filter crossing of interest from larger dataset, transpose the data to be in long format and then join to above data frame. 
# 3rd step: For each chunk of the summary sheet, filter based on section and then make table using kable or gt or other.

# ##%######################################################%##
#                                                          #
####                     Freshwater                     ####
#                                                          #
##%######################################################%##
# FRom STephen- [2/18 7:21 PM] Stephen Lloyd
# OK so the layer (with attached photos) you should work from is in here:
# \\nyspatial.tnc.org\gisdata\Projects\LI\Culvert_Assessment\data\FreshwaterPrioritization\FreshwaterPrioritization_for_webtool.gdb
# [2/18 7:22 PM] Stephen Lloyd
# 
# I also put the Field Description list on Teams, as well as in the folder: \\nyspatial.tnc.org\gisdata\Projects\LI\Culvert_Assessment\data\FreshwaterPrioritization\
# [2/18 7:23 PM] Stephen Lloyd
# Photos are here: \\nyspatial.tnc.org\gisdata\Projects\LI\Culvert_Assessment\photos and the match table I created is in there called: match_table_021820.csv

# fw_data <- st_read("M://Projects/LI/Culvert_Assessment/data/FreshwaterPrioritization/FreshwaterPrioritization_for_webtool.gdb", stringsAsFactors = F, layer = "TNC_NAACC_FreshwaterPrioritization_032720") %>%
# st_transform(crs = 4326)
# fw_data %>% write_rds(here::here("data/fwaterPrioritizations.rds"))
## @knitr freshDataRead

fw_data <- read_rds(here::here("data/fwaterPrioritizations.rds")) 

# fsdn = freshwater summary data needs
fsdn <- summaryNeeds %>% 
  filter(protocol == "Freshwater") %>% 
  filter(needed == "Y") %>% 
  select(field_Name, Description, SummarySection, dataType, tableOrder)
  
write_rds(fsdn, here::here("data/fsdn.rds"))

# Warren Pinnacle lookup:
slrScenarios <- tibble::tribble(
  ~value,                                  ~inund_freq,
      0L,                                 "open water",
      1L,      "inundated at least once every 30 days",
      2L,      "inundated at least once every 60 days",
      3L,      "inundated at least once every 90 days",
      4L,            "inundated by the 10 years storm",
      5L,           "inundated by the 100 years storm",
      6L,                   "above the 100 year storm",
      7L, "below the 100 year storm but not connected",
      8L,                         "protected by dikes",
    -99L,                              "no data/blank"
  )
write_rds(slrScenarios, here::here("data/slrScenariosLOOKUP.rds"))

## Photos



photo_check <- function(path){
  if(!file.exists(path)){
    return(knitr::include_graphics(path = "256px-No_image_available.png", dpi = 72))
  }else{
   pic <-  knitr::include_graphics(path = path, auto_pdf = TRUE, dpi = 72)
   return(pic)
  }
}

#' photo_link
#'
#' @param crossingID 
#' @param matchTable 
#' @param subject of the photo, options being "inlet", "outlet", "upstream", "downstream", "other"
#' @param path path to master photo directory, not crossing specific subfolder.
#'
#' @return vector of url/link to image file.
#' @export
#'
#' @examples
photo_link <- function(crossingID, matchTable, subject, path){
 
  imgs <- matchTable %>% filter(ID == crossingID) %>% select(ImageName) %>% as_vector()
  switch(subject,
         inlet = {imgs %>% str_detect("nlet") -> imgname}, # make a more robust str_detect statemement.
         outlet = {imgs %>% str_detect("utlet") -> imgname},
         upstream = {imgs %>% str_detect("pstream") -> imgname},
         downstream = {imgs %>% str_detect("ownstream") -> imgname},
         # other = {imgs %>% str_detect("")} Doesn't seem to be any 'others' as of now.
         stop("Missing subject argument.") # Return a message indicating missing argument.
         )
  selectimg <- imgs[imgname] # subset the vector that is TRUE
  picpath <- paste0(path, "/", crossingID, "/", selectimg) # construct the path
  if(length(picpath) == 1){ # catch duplicate paths 
    return(picpath)
  }else{
    if(length(picpath) >= 2){
      return(picpath[1])
    }else{
      return(NULL)
    }
  }
}



# Freshwater Prioritizations
#' This might not be the best method as interating over the knitr function will require the data to be reloaded each time.
#' 

# ## Scratch BELOW:
# LIculvertPrioritization %>% 
#   filter(crossingID == 200) %>% 
#   tidyr::gather(key = "field_Name", value = "dataValue") %>% 
#   right_join(tsdn) %>% select(AssociatedText, dataValue, Section) %>% 
#   gt(rowname_col = "AssociatedText", 
#      groupname_col = "Section") %>%
#   tab_header(
#     title = "Tidal Prioritization"
#   )
