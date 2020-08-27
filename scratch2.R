
# Longitudinal profile work: ----
# Use crossing #115 as a test dummy. SEe Teams post regarding the issue found in the datasheet.
testData <- LIculvertsAssessments_debug %>% filter(crossingID == 115)
filepath <-  "C:\\Users\\astarke\\Box Sync\\Culvert Assessment\\Tidal Assessments\\TidalAssessmentWorkbooks_QAvers\\Culvert115.xlsm"
# filepath <- testData$filePath

testData %>% select(crossingID, longProfile) %>% unnest() ->longdata
View(longdata)
testData %>% select(crossingID, rawheights) %>% unnest() ->heights
View(heights)

# flatten out this test table to figure out if we should just append to the full dataset.
LIculvertsAssessments %>% select(crossingID, rawheights)
  select(crossingID, rawheights) %>% 
  unnest() %>% 
  unite(Feature, Position, col = "FP", sep = " ") %>% 
  select(crossingID, FP, adjustedHt) %>% 
  spread(key = FP, value = adjustedHt) -> heights


flat_heights <- function(heights){
  # quo_heights <- enquo(heights)
  unite(data = heights, Feature, Position, col = "FP", sep = " ") %>% 
    select(FP, adjustedHt) %>% 
    spread(key = FP, value = adjustedHt)
}

# TODO: FIX this...
testData %>% select(crossingID, crossHeights) %>% unnest() ->crossdata
View(crossdata)

CompletedTidal <- LIculvertDataStatus %>% filter(FieldAssessmentComplete == "Y") %>% 
  pull(crossingID)


 # 
# # purrr using safely to catch errors.
# crossSafe <- safely(crossSection)
# crossSafeLY <- LIculvertsAssessments %>% 
#          mutate(cross = map2(.x = filePath, .y = tidycells, .f = ~crossSafe(.x, .y))) 
# safetyInspect <- crossSafeLY %>% select(filenames, cross) %>% unnest() 
# safetyInspect %>% filter(cross != 'NULL') -> keeps
# 
# allCrossProf <- LIculvertsAssessments %>% 
#   select(crossingID, crossSectionalProfile) %>% 
#   unnest()
# 
# crossSum <- allCrossProf %>% select(-crossingID1) %>% 
#   group_by(crossingID, Feature) %>% 
#   summarize(heightUS = mean(adjustedHtUS, na.rm = T), heightDS = mean(adjustedHtDS, na.rm = T))



  
  crossdata %>% 
    # mutate(Distance = seq(1, 48, by = 4)) %>% # need to program this calc.
    ggplot(aes(x = Distance, y = adjustedHt, xend = Distance + 3, yend = adjustedHt)) + 
    geom_segment(aes(color = Feature), size = 3) + 
    geom_text(aes(label = Feature)) + 
    theme_ipsum_rc()
  
  
  
  
# Purrr profile plots. 
  
  profilePlots <- LIculvertsAssessments %>% 
    filter(crossingID %in% CompletedTidal) %>% 
    select(crossingID, longProfile) %>% 
    mutate(profPlots = map2(.x = crossingID, 
                            .y = longProfile, 
                            .f = ~ggplot(data = .y, aes(x = Distance, y = adjustedHt)) + 
    geom_point(aes(shape = `Feature Code`), size = 2) + 
    #geom_line(aes(color = Subsrate), size = 1, linetype = 1) +
    geom_line(aes(), size = 1) +
    geom_hline(aes(yintercept = 0)) +
    labs(title = paste0("Tidal Crossing # ", .x),
         x = "Distance from Upstream Hydraulic Control (feet)",
         y = "NAVD88 (feet)") + 
      theme_ipsum_rc()))

  profilePlots$profPlots[[2]]
  
profilePlots %>% filter(crossingID == 70) %>% pull(profPlots)


# determine the missing HWI's 
LIculvertsAssessments_debug %>% select(crossingID, heights) %>%  
  unnest() %>% mutate(hasHt = !is.na(Height)) -> hwi_qunat


rpivotTable(hwi_qunat, rows = c("Feature", "Position"), cols = "hasHt", rendererName = "Heatmap", aggregatorName = "Count as Fraction of Rows")


LIculvertDataStatus_location %>% select(crossingID, FieldAssessmentComplete) %>% st_drop_geometry() %>% 
  mutate(AssessmentStatus = if_else(FieldAssessmentComplete  != "N" | FieldAssessmentComplete == "Y", 
                                    true = "Assessment Complete", 
                                    false = "Assessment Incomplete")) %>% st_write()
  group_by(AssessmentStatus) %>% tally


  
  LIculvertAssessmentData %>% st_drop_geometry() %>% 
    left_join(tidalPrioritization) %>% 
    mutate(relElevation = `Ceiling of Structure US` - `HWI Stain US`) %>% 
    ggplot(aes(y = relElevation, x = as.factor(crossingID))) + 
    geom_hline(yintercept = 39/12, size = 2, color = tnc_color("Sandstone")) +
    geom_point(aes(color = Total_Prioritization), size = 2) +
    geom_hline(yintercept = 0, size = 1, color = "grey70") +
    geom_text(aes(x = 300, y = 39/12, label = "39 in of sea level rise", vjust = -1)) +
    theme_tnc_base() +
    scale_color_tnc("gradient1", discrete = F, reverse = F) + 
    theme(axis.text.x = element_blank()) +
    labs(y = "Ceiling ht relative to high tide indicator (NAVD88)", x = NULL)
  
                                                                                  