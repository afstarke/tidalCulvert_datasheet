# Pivot Table ----

## @knitr fieldAssessmentPivot
# grouped by = FieldAssessmentComplete,listedOnFieldSched

LIculvertDataStatus_location %>% st_drop_geometry() %>% 
  group_by(FieldAssessmentComplete,listedOnFieldSched) %>%
  rpivotTable::rpivotTable(rows = "FieldAssessmentComplete", cols = "listedOnFieldSched")


# crossingTracker %>% select(crossingID, PtrPriorit:`TNC&PtnrSum`, `Revisit required?`, `Field Ass. Complete(Y/N)`) %>%
# group_by(`Field Ass. Complete(Y/N)`, TNCPriorit) %>% tally()
fieldData_pivot <-  crossingTracker %>%
  select(
    crossingID,
    PtrPriorit:`TNC&PtnrSum`,
    `Revisit required?`,
    `Field Ass. Complete(Y/N)`
  ) %>%
  rpivotTable()


rpivotTable(LIculvertDataStatus_location, rows = c("ToBeAssessed_2019", "FieldAssessmentComplete"), 
            cols = "DesktopAssessmentComplete", width = "100%",
            rendererName = "Heatmap")
# Prioritizations 
LIculvertPrioritization <- read_rds(path = "data/LIculvertPrioritizations.rds")
rpivotTable(LIculvertPrioritization, )
