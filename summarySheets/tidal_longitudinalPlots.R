# Tidal summary sheet longitudinal plot

# TESTING FOR BUGS IN DISTANCE CALCULATIONS >----
# COMMENT OUT WHEN NOT IN USE!
# filepath <- testData$filePath
# tidycells <- testData$tidycells
troubleshooter = FALSE # turn this on/off for troubleshooter mode.

# TODO: Create a mock plot that will be used in instances where there's insufficient data to produce a real plot.
# crossing 113 is a decent canidate.
#' 
#' Depends on rmdSetup.R for updates on the data pulled from ArcGIS Online and the worksheets.
#' 1) connect to VPN and T and M drives
#' 2) #' source("code/rmdSetup.R")
#'
# # images for plots ----
# car_back <- image_read( "summarySheets/car-back.jpg")
# car_front <- image_read("summarySheets/car-front.jpg")
# marsh <- image_read("summarySheets/ClipartKey_46580.png") %>% image_colorize(color = "white", opacity = 60)
# pipe <- image_read("summarySheets/culvert.jpg")
# 



drawCrossing <- function(longitudinal, crossSectional) {
  
  
  ccode <- longitudinal$crossingID[1]
  lon <- longitudinal
  cross <- crossSectional 
   # Supply a default longitidinal profile tibble is there isn't one provided in the input data
  if (nrow(lon) == 0) {
    lon <- read_rds(here::here("data", "defaultPlotLongdata.rds")) %>% 
      arrange(Distance)
  }
  
  # provide a named vector of colors
  crossColors <- c(
    "Ceiling of Structure" = "#7e6a65",
    # storm
    "HWI Stain" = "#c5351c",
    # Crimson
    "HWI Wrack" = "#a0c04d",
    # Spring Green
    "Low Tide Water Elevation" = "#23487a",
    # Indigo
    "Marsh Plain Shot" = "#00703c",
    # Oak Green
    "Road Surface" = "grey50",
    # Grey 50
    "Road Center" = "grey50" # Grey 50
  )
  # Data munging for plots ----
  # culvert pipe data for plot ====
  culvert <- cross %>%
    filter(Feature == "Ceiling of Structure") %>%
    select(Feature, Position, adjustedHt, Distance) %>%
    bind_rows({
      lon %>%
        filter(`Feature Code` == "I" |
                 str_detect(string = `Feature Code`, pattern = "I")) %>%
        select(`Feature Code`, adjustedHt, Distance) %>%
        rename(Feature = `Feature Code`)
    }) %>%
    mutate(Structure = "Culvert")
  
  # low tide water level ====
  lowtide <-
    cross %>% filter(Feature == "Low Tide Water Elevation") %>%
    slice(rep(1:n(), each = 2)) %>% arrange(Distance)
  lowtide$Distance[1] <- 0
  lowtide$Distance[4] <- max(longitudinal$Distance, na.rm = T)
  lowtide$minY <- min(longitudinal$adjustedHt, na.rm = T)
  # lowtide$adjustedHt[c(1,4)] <- min(longitudinal$adjustedHt)
  
  roadFill <- cross %>% filter(Feature == "Ceiling of Structure" |
                                 Feature == "Road Surface" |
                                  Feature == "Road Center") %>% 
   unite(col = "ord", Position, Feature, sep = "_", remove = F) %>% 
    mutate(ord = factor(x = ord, levels = c("US_Road Surface", "NA_Road Center", "DS_Road Surface", "DS_Ceiling of Structure", "US_Ceiling of Structure"))) %>% 
    arrange(ord)
  
  roadSurface <- cross %>% dplyr::filter(str_detect("Road", string = Feature)) %>% arrange(Distance)
  roadLabel <- cross %>% dplyr::filter(str_detect("Road", string = Feature)) %>% arrange(Distance) %>% slice(2)
  
  
waterIndicators <- cross %>%
  filter(str_detect(Feature, pattern = "HWI") |
           Feature == "Low Tide Water Elevation" | # Added water level as ribbon
           str_detect(Feature, pattern = "Marsh")) %>% 
  mutate(endDist = case_when(Position == "DS" ~  max(cross$Distance, lon$Distance, na.rm = T), 
                             Position == "US" ~ 0))

axisLims_y <- c(min(cross$adjustedHt, lon$adjustedHt, na.rm = T), 1.2 * (max(cross$adjustedHt, lon$adjustedHt, na.rm = T)))

axisLims_x <- c(min(cross$Distance, lon$Distance, na.rm = T), max(cross$Distance, lon$Distance, na.rm = T))
 
 # browser()

   crossPlot <- ggplot() +
 
    # Water level - Low tide ====
    geom_ribbon(
      data = lowtide,
      aes(ymax = adjustedHt, ymin = minY - .5 , x = Distance), color = "#23487a", fill = "#23487a", alpha = 0.35) +
     geom_line(data = lowtide,
               aes(y = adjustedHt, x = Distance), color = "#23487a", alpha = 0.9, size = 2) +
    
     ## Arrow showing stream flow. ====
  
   geom_segment(aes(
     x = 0,
     y = max(axisLims_y, na.rm = T) ,
     xend = max(axisLims_x, na.rm = T),
     yend = max(axisLims_y, na.rm = T)),
     size = 2,
     alpha = 1,
     arrow = arrow(length = unit(0.1, "inches"), type = "closed", angle = 25),
     lineend = "round", 
     linejoin = "round",
     color = "grey70"
   ) +
     geom_label(aes(
       label = "stream flow",
       x = 0,
       y = max(axisLims_y, na.rm = T)),
       color = "grey40",
       hjust = 0
     ) +
     # Water indicators
     geom_segment(
       data = waterIndicators,
       aes(x = Distance, 
           y = adjustedHt, 
           color = Feature, 
           xend = endDist, 
           yend = adjustedHt),
       size = 2, linetype = "dashed"
     ) + 
    # fill between road and culvert ====
    geom_polygon(data = roadFill,
    aes(x = Distance, y = adjustedHt),
    fill = tnc_color("Canyon")) +
     
    # stream bed ====
    geom_ribbon(data = lon, aes(ymax = adjustedHt, ymin = min(adjustedHt, na.rm = T) - .5 , x = Distance),
      linetype = 3,
      fill = "grey8",
      alpha = 1) +

    
    # culvert structure ====
    geom_shape(data = culvert, aes(y = adjustedHt, x = Distance),
      fill = "grey40",
      color = "black", 
      alpha = 1) +
    # Culevrty structure label ====
    annotate(label = "Culvert structure",
             geom = "label",
             x = (max(culvert$Distance, na.rm = T) - min(culvert$Distance, na.rm = T))/3 + min(culvert$Distance, na.rm = T),
             y = (max(culvert$adjustedHt, na.rm = T) - min(culvert$adjustedHt, na.rm = T))/2 + min(culvert$adjustedHt, na.rm = T)) +
    
    
    # Road surface ====
    geom_line(
      data = roadSurface,
      aes(x = Distance, y = adjustedHt),
      size = 3,
      lineend = "round", 
      linejoin = "round",
      color = "black"
    ) +
    # road surface label ====
   annotate(geom = "label", 
            label = "Road surface", 
            color = "black", 
            nudge_y = 3,
            x = roadLabel$Distance,
            y = roadLabel$adjustedHt * 1.1) +
   
     # Legend labels and descriptors
    scale_color_manual(values = crossColors, 
                       breaks = c('HWI Stain', 'HWI Wrack', 'Marsh Plain Shot', 'Low Tide Water Elevation'),
                       labels = c("<strong style='color:#c5351c'> High water indicator - Stain: </strong> <br> darkened stain representing <br>typical high tide levels.",
                                  "<strong style='color:#a0c04d'> High water indicator - wrack line: </strong> <br> Represents wrack line <br>representing highest recent tide",
                                  "<strong style='color:#00703c'> Marsh plain shot: </strong> <br> Average marsh <br>surface elevation",
                                  "<strong style='color:#23487a'> Low tide elevation: </strong> <br> Recorded at time<br> of field survey")) + 
     
    # Stream bed label
    annotate(geom = "label", 
             label = "Stream bed", 
             color = "black", 
             hjust = 0,
             # nudge_x = 3,
             x = 2,
             y = min(axisLims_y, na.rm = T) + .25) +
   
     # Scales for limits and ticks. ====
    # scale_x_continuous(limits = c(0, max(lon$Distance) + 10), expand = expansion(0)) + 
    # scale_y_continuous(limits = axisLims, expand = expansion(c(0, .3))) + # using scale_*_continuous clips data that lands outside the lims.
    coord_cartesian(ylim = axisLims_y, xlim = c(0, max(axisLims_x, na.rm = T)), clip = "on") + # Using coords_cartesian zooms in on area.
    
    labs(x = "Distance from Upstream Hydraulic Control (feet)",
         y = "NAVD88 (feet)") +
    theme_ipsum_rc() +
    theme(
      plot.subtitle = element_text(family = "sans"),
      plot.caption = element_text(family = "sans"),
      axis.title = element_text(family = "sans", size = 12),
      plot.title = element_text(family = "sans",
                                size = 14),
      legend.position = "right", 
      text = element_text(size = 11),
      legend.spacing.y = unit(.5, 'cm'),
      legend.text = element_markdown(margin = margin(t = 10)),
      legend.key.size = unit(1, "cm"),
      plot.title.position = "plot"
    ) + labs(
      title = "",
      subtitle = paste0("Crossing #: ", ccode),
      size = 12
    )
  crossPlot
  
    
  
}


drawCrossing <- purrr::safely(drawCrossing, otherwise = "Insufficient Data") # Add in default plot here.
# # Test out plots and inspect.
# Run single crossing through funcicton (quicker)
# Best plot for longitudinals so far... ----
# test2longitudinalProfile <- LIculvertAssessments %>% filter(crossingID == 96) %>% select(longProfile) %>% unnest(cols = longProfile)
# # View(test2longitudinalProfile)
# test2crossHeight <- LIculvertAssessments %>% filter(crossingID == 96) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
# # View(test2crossHeight)
# drawCrossing(longitudinal = test2longitudinalProfile, crossSectional = test2crossHeight)

# 
# troubleshooter <- TRUE
# if(troubleshooter == TRUE){
#   plotter <-
#     function(crosscode) {
#       test2longitudinalProfile <-
#         LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(longProfile) %>% unnest(cols = longProfile)
#       # View(test2longitudinalProfile)
#       test2crossHeight <-
#         LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
#       # View(test2crossHeight)
#       drawCrossing(longitudinal = test2longitudinalProfile, crossSectional = test2crossHeight)
#     }
#   
#   c(96, 110, 9999) %>% map(plotter)
#   plotter(9999)
#   
#   plotter_data <-
#     function(crosscode) {
#       test2longitudinalProfile <-
#         LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(longProfile) %>% unnest(cols = longProfile)
#       # View(test2longitudinalProfile)
#       test2crossHeight <-
#         LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
#       # View(test2crossHeight)
#       df <- list(test2longitudinalProfile, test2crossHeight)
#       df
#     }
#   plotter_data(49)
# }


# 
# # Crossings that lack longprofile
# LIculvertAssessments %>% mutate(zerorows = map_lgl(.x = longProfile, ~nrow(.x) == 0)) %>%
#   filter(zerorows == 1) %>%
#   pull(crossingID) 
# # Crossings that HAVE longprofile
# LIculvertAssessments %>% mutate(zerorows = map_lgl(.x = longProfile, ~nrow(.x) == 0)) %>%
#   filter(zerorows == 0) %>%
#   pull(crossingID) %>% map(plotter)
# # Crossings that lack crosssectional profile data
# LIculvertAssessments %>% mutate(zerorows = map_lgl(.x = crossSectionProfile, ~is_character(.x))) %>%
#   filter(zerorows == TRUE) %>%
#   pull(crossingID)
# # Crossings that HAVE crosssectional profile data
# LIculvertAssessments %>% mutate(zerorows = map_lgl(.x = crossSectionProfile, ~is_tibble(.x))) %>%
#   filter(zerorows == TRUE) %>%
#   pull(crossingID) %>% map(plotter)
# # 
# get_score <-
#   function(crossID) {
#     LIculvertPrioritization %>% filter(crossingID == crossID) %>% pull(Total_Prioritization)
#   }
# get_score(522)
# # drawCrossing(longitudinal = test2longitudinalProfile, crossSectional = test2crossHeight) + theme(


#   axis.line = element_blank(),
#   axis.text.x = element_blank(),
#   # axis.text.y = element_blank(),
#   axis.ticks = element_blank(),
#   axis.title.x = element_blank(),
#   # axis.title.y = element_blank(),
#   # panel.background = element_blank(),
#   # panel.border = element_blank(),
#   # panel.grid.major = element_blank(),
#   # panel.grid.minor = element_blank(),
#   # plot.background = element_blank()
# ) + labs(title = "Mock crossing", subtitle = glue::glue("Crossing #: {ccode}"))
# 

# Run all crossings through plot function
# a <- LIculvertAssessments %>%
#   mutate(longPlots = map2(.x = longProfile, .y = crossSectionProfile, .f = ~drawCrossing(longitudinal = .x, crossSectional = .y)))
# # # 
# a %>% filter(crossingID == 339) %>% pull(longPlots)
# a %>% sample_n(1) %>% pull(longPlots)#
# # # #
# sample_n(a, 1) %>% pull(longPlots)
# # 
# #
