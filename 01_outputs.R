
# Working output folder - CUrrently the Teams- LI culvert Assessments - Tidal Crossings channel
outputpath <- "C:/Users/astarke/The Nature Conservancy/Long Island Road-Water Culvert Assessments - Tidal Crossing Assessments"

# Render out the prioritizations for all the crossings
render(input = "tidalCrossing_Prioritizations.Rmd", output_dir = outputpath, output_file = "Prioitization_calcs.html")



# Render summary document for a given crossing #

summarySheets <- function(crossingNu){
  rmarkdown::render("SummarySheet_tidal.Rmd", params = list(id = crossingNu), output_dir = "summarySheets/", output_file = paste0("priorizartionReport_", crossingNu))
}
