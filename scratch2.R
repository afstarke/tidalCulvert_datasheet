
# Longitudinal profile work: ----
# Use crossing #115 as a test dummy. SEe Teams post regarding the issue found in the datasheet.
testData <- LIculvertsAssessments %>% filter(crossingID == 115)
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

# TESTING FOR BUGS IN DISTANCE CALCULATIONS >----
# COMMENT OUT WHEN NOT IN USE!
# filepath <- testData$filePath
# tidycells <- testData$tidycells

test2longitudinalProfile <- LIculvertsAssessments %>% filter(crossingID == 115) %>% select(longProfile) %>% unnest()
View(test2longitudinalProfile)
test2crossHeight <- LIculvertsAssessments %>% filter(crossingID == 115) %>% select(crossHeights) %>% unnest()
View(test2crossHeight)

test2longitudinalProfile %>% 
  ggplot(aes(x = Distance, y = adjustedHt, shape = `Feature Code`)) + 
  # geom_smooth(se = FALSE, span = 0.4) + 
  geom_point(size = 2) + 
  #geom_line(aes(color = Subsrate), size = 1, linetype = 1) +
  geom_line(aes(), size = 1) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Distance from Upstream Hydraulic Control (feet)",
       y = "NAVD88 (feet)") + theme_ipsum_rc() + 
  geom_segment(data = test2crossHeight, mapping = aes(x = Distance, 
                                               y = adjustedHt, 
                                               xend = Distance + 5, 
                                               yend = adjustedHt, 
                                               color = Feature), size = 2) 



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


# make ggplot which includes the longitudinal profile and the crossing sectional 
# TODO: need to figure out how to layout data for the different types of info for plots.


plot <- longdata %>% ggplot(aes(x = Distance, y = adjustedHt)) + 
  # geom_smooth(se = FALSE, span = 0.4) + 
  geom_point(aes(shape = `Feature Code`), size = 2) + 
  #geom_line(aes(color = Subsrate), size = 1, linetype = 1) +
  geom_line(aes(), size = 1) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Distance from Upstream Hydraulic Control (feet)",
       y = "NAVD88 (feet)") + theme_ipsum_rc() + 
  geom_segment(data = crossdata, mapping = aes(x = Distance,
                                               y = adjustedHt,
                                               xend = Distance + 5,
                                               yend = adjustedHt,
                                               color = Feature), size = 2) +
  NULL
 direct.label(plot, "first.bumpup")


  
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


channelLongidinalProfile_extract <- function(filepath, tidycells){
  # Set up variables for adjusting to NAVD88 with surveyHtCorrection()
  crossingID <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'L7') %>% as.numeric()
  Lidarht <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SUMMARY', celladdress = 'J54') %>% as.numeric()
  roadCentHt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'J107') %>% as.numeric()
  
  TPforsight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y110') %>% as.numeric()
  TPbacksight_upSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'Y111') %>% as.numeric()
  TPforsight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD110') %>% as.numeric()
  TPbacksight_dwSt <- culvert_extract(tidycells = tidycells, sheetOI = 'Data Sheet - SITE', celladdress = 'AD111') %>% as.numeric()
  
  profile <- read_xlsx(path = filepath, sheet = 2, range = "A122:M140")
  profile <- profile %>% filter(!is.na(Distance)) %>%
    select(-starts_with("..")) %>%
    rename(Subsrate = `Sub-\r\nstrate`,
           shotCode = `Shot From (R/U/D)`,
           rawHeight = Height) %>%
    mutate(crossingID = crossingID,
           adjustedHt = surveyHtCorrection(rawHeight = rawHeight, 
                                           shotCode = shotCode, 
                                           Lidarht = Lidarht, 
                                           roadCentHt = roadCentHt,
                                           TPforsight_upSt = TPforsight_upSt, 
                                           TPbacksight_upSt = TPbacksight_upSt,
                                           TPforsight_dwSt = TPforsight_dwSt, 
                                           TPbacksight_dwSt = TPbacksight_dwSt))
  
  
  profile
}
  
# determine the missing HWI's 
LIculvertsAssessments %>% select(crossingID, heights) %>%  
  unnest() %>% mutate(hasHt = !is.na(Height)) -> hwi_qunat


rpivotTable(hwi_qunat, rows = c("Feature", "Position"), cols = "hasHt", rendererName = "Heatmap", aggregatorName = "Count as Fraction of Rows")


LIculvertDataStatus_location %>% select(crossingID, FieldAssessmentComplete) %>% st_drop_geometry() %>% 
  mutate(AssessmentStatus = if_else(FieldAssessmentComplete  != "N" | FieldAssessmentComplete == "Y", 
                                    true = "Assessment Complete", 
                                    false = "Assessment Incomplete")) %>% st_write()
  group_by(AssessmentStatus) %>% tally


                                                                                  