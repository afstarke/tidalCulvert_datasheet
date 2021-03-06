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
  paths <- list.files(path = glue::glue("C:/Users/astarke/Box Sync/Culvert Assessment/Tidal Assessments/Photos/Crossing ID #{crossing}"), full.names = T)
  paths %>% walk(.x = ., .f = ~browseURL(url = .x, browser = NULL))
}

openField <- function(crossing){
  openDS(crossing)
  openPics(crossing)
}

fieldData_view <- function(crossing) {
  LIculvertsAssessments %>% filter(crossingID == crossing) %>%
    pull(fielddata) %>%
    pluck(1) %>%
    pivot_longer(
      cols = everything(),
      names_to = "metric",
      values_to = "values",
      values_ptypes = list(values = 'character', metric = 'character')
    )
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
