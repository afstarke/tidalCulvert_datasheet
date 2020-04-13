# Summary Sheet Setup

# load the Prioritization data
LIculvertPrioritization <- read_rds("data/LIculvertPrioritizations.rds")
# load the assessment data which contains some additional metrics and info used in the summary sheets.
LIculvertAssessmentData <- read_rds("data/LIculvertAssessmentData.rds")

# # Create a temporary excel file to select which fields are needed and what sections they will correspond to.
# 
# summaryNeeds <- tibble(
#   fields =c(names(LIculvertPrioritization), names(LIculvertAssessmentData)),
#   needed = NA,
#   Section = NA
# )
# 
# write_xlsx(summaryNeeds, path = "../../../The Nature Conservancy/Long Island Road-Water Culvert Assessments - Tidal Crossing Assessments/summarySheetNeeds.xlsx")

summaryNeeds <- read_xlsx(path = "../../../The Nature Conservancy/Long Island Road-Water Culvert Assessments - Tidal Crossing Assessments/summarySheetNeeds2.xlsx")

