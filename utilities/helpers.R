# Helper functions

# Save a summaryRoutput dataframe to excel for others to review ----

saveSumary <- function(summaryDF){
  xlsx::write.xlsx(x = summaryDF, file = "outputs/summaryRoutput.xlsx", showNA = FALSE)
}




openDS <- function(crossing) {
  if (!exists(x = 'LIculvertAssessments'))
  {
    print("Must run rmdSetup.R or load LIculvertsAssessments into the environment")
  } else {
    path <-
      LIculvertAssessments %>% filter(crossingID == crossing) %>% pull(filePath)
    
    if (!rlang::is_empty(path)) {
      browseURL(url = path)
    } else {
      print("No field datasheet available")
    }
  }
}

openPics <- function(crossing){
  paths <- list.files(path = glue::glue("C:/Users/astarke.TNC/Box/Culvert Assessment/Tidal Assessments/Photos/Crossing ID #{crossing}"), full.names = T)
  paths %>% walk(.x = ., .f = ~browseURL(url = .x, browser = NULL))
}

openField <- function(crossing){
  openDS(crossing)
  openPics(crossing)
}

fieldData_view <- function(crossing) {
  LIculvertAssessments %>% filter(crossingID == 96) %>%
    pull(fielddata) %>%
    pluck(1) %>%
    t() %>% view()
}

crossGlimpse <- function(crossing){
  LIculvertAssessmentData %>% filter(crossingID == crossing) %>% glimpse()
}

crossVisit <- function(crossing) {
  visitDate <-
    LIculvertAssessmentData %>% filter(crossingID == crossing) %>% pull(dateAssessed)
  
  if (length(visitDate) == 0) {
   print("Not Visited")
   openDS(crossing)
  } else{
    if(is.na(visitDate)){
      openDS(crossing)
    }else{
      visitDate
    }
    
  }
}

crossPlot <- function(crossing){
  LIculvertAssessments %>% filter(crossingID == crossing) %>%
    pull(profilePlots)
}

mapCross <- function(crossing){
  cross <- LIculvertAssessmentData %>% filter(crossingID == crossing)
  mapview(cross)
}
