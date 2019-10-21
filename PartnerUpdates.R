# Partner updates
PEPsites <- c("46269",
"46271",
"201",
"107",
"428",
"441",
"102",
"40243",
"40062",
"397",
"40029",
"7",
"444",
"446",
"37",
"37",
"140",
"44")
# Select sites using attribute filter
PEPsitesStatus <- LIculvertDataStatus_location %>% filter(crossingID %in% PEPsites) %>% select(crossingID, FieldAssessmentComplete, DesktopAssessmentComplete, ToBeAssessed_2019, MissingFromAssessment)
mapview(PEPsitesStatus, zcol = "FieldAssessmentComplete")

PEPsitesStatus %>% st_drop_geometry() %>% 
write.xlsx2(file = paste0(teamsDrive, "/PEPsiteStatus.xlsx"), append = FALSE)

# Select sites using spatial seletion
# southoldsection <- mapedit::editMap()
# southoldsection %>% st_write("outputs/southoldSection.shp", delete_dsn=TRUE)
southoldsection <- st_read("outputs/southoldSection.shp")
southoldCrossings <- LIculvertDataStatus_location[southoldsection,]

southoldCrossings %<>% select(crossingID, ToBeAssessed_2019, FieldAssessmentComplete, DesktopAssessmentComplete, dateAssessed)
mapview(southoldCrossings, zcol = "FieldAssessmentComplete")
southoldCrossings %>% filter(ToBeAssessed_2019 == FALSE) %>% st_write(dsn = paste0(tidalCulvert_outputs, "/southoldTidalCrossingsNotPrioritized.shp"), delete_layer = TRUE)
st_write(southoldCrossings, dsn = paste0(tidalCulvert_outputs, "/southoldTidalCrossings.shp"), delete_layer = TRUE)

SoutholdSummaryData <- southoldCrossings %>% st_drop_geometry() %>% 
  replace_na(list(FieldAssessmentComplete = "N", DesktopAssessmentComplete = "N", FullAssessmentComplete = "N")) %>% 
  select(crossingID, FieldAssessmentComplete:FullAssessmentComplete) %>% 
  group_by(FieldAssessmentComplete, DesktopAssessmentComplete) %>% 
  tally()
SoutholdSummaryData


