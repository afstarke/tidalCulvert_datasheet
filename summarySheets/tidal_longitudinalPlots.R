# Tidal summary sheet longitudinal plot

# TESTING FOR BUGS IN DISTANCE CALCULATIONS >----
# COMMENT OUT WHEN NOT IN USE!
# filepath <- testData$filePath
# tidycells <- testData$tidycells

# TODO: Create a mock plot that will be used in instances where there's insufficient data to produce a real plot.

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
# Best plot for longitudinals so far... ----
# test2longitudinalProfile <- LIculvertAssessments %>% filter(crossingID == 443) %>% select(longProfile) %>% unnest(cols = longProfile)
# # View(test2longitudinalProfile)
# test2crossHeight <- LIculvertAssessments %>% filter(crossingID == 443) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
# View(test2crossHeight)
library(ggrepel)
library(ggforce)
#TODO: find solution to automate the placement of each image. (perhaps mutate a column into data OR split data into many small bits for simplicity?)



drawCrossing <- function(longitudinal, crossSectional) {
  # longitudinal <-  {a %>% filter(crossingID == 10) %>% pull(longProfile)}[[1]]
  ccode <- longitudinal$crossingID[1]
  lon <- longitudinal
  cross <- crossSectional 
  height <- cross 
  
  # Issues with mapping color to marker consistently- fix by providing a named vector of colors
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
  
  
  culvert <- height %>%
    filter(Feature == "Ceiling of Structure") %>%
    select(Feature, Position, adjustedHt, Distance, imagepath) %>%
    bind_rows({
      lon %>%
        filter(`Feature Code` == "I" |
                 str_detect(string = `Feature Code`, pattern = "I")) %>%
        select(`Feature Code`, adjustedHt, Distance) %>%
        rename(Feature = `Feature Code`)
    }) %>%
    mutate(Structure = "Culvert")
  
  crossPlot <- ggplot() +
 
  
    # fill between road and culvert
    geom_polygon(data = {
      height %>% filter(Feature == "Ceiling of Structure" |
                          Feature == "Road Surface") %>% arrange(Distance)
    },
    aes(x = Distance, y = adjustedHt),
    fill = tnc_color("Canyon")) +
    
    # stream bed
    geom_ribbon(
      data = lon,
      aes(
        ymax = adjustedHt,
        ymin = min(adjustedHt) - .5 ,
        x = Distance
      ),
      linetype = 3,
      fill = "black",
      alpha = 0.9
    ) +
    # culvert
    # NOTE: We could use geom_ribbon_pattern from ggpattern package to give it some pizazz.
    geom_shape(
      data = culvert,
      aes(y = adjustedHt, x = Distance),
      fill = "grey40",
      alpha = 0.7
    ) +
    annotate(label = "Culvert structure",
             geom = "text",
             x = min(culvert$Distance) - max(culvert$Distance),
             y = min(culvert$adjustedHt) - max(culvert$adjustedHt)) +
    
    # Water indicators
    
    geom_point(
      data = {
        height %>% filter(
          str_detect(Feature, pattern = "HWI") |
            Feature == "Low Tide Water Elevation" |
            str_detect(Feature, pattern = "Marsh")
        )
      },
      aes(x = Distance, y = adjustedHt, color = Feature),
      size = 15,
      shape = "_"
    ) + 
    scale_color_manual(values = crossColors) + 
    
   
   
    # cars
   
    # cowplot::draw_image(
    #   image = car_front,
    #   # the min would be starting from the xmax minus the average width of a car (7ft)
    #   x = car_position_x - 20,
    #   # the max X for the upstream/front side would be the middle - 1 ft. + an addtional 7.5 feet for the width of the car
    #   y = car_position_y,
    #   width = 27,
    #   height = 3,
    #   scale = 1,
    #   interpolate = F
    # ) +
    # cowplot::draw_image(
    #   image = car_back,
    #   # the min would be starting from the xmax minus the average width of a car (7ft)
    #   x = car_position_x,
    #   # the max X for the upstream/front side would be the middle - 1 ft. + an addtional 7.5 feet for the width of the car
    #   y = car_position_y,
    #   width = 27,
    #   height = 3,
    #   scale = 1,
    #   interpolate = F
    # ) +
    # Road surface
    geom_line(
      data = {
        height %>% dplyr::filter(str_detect("Road", string = Feature)) %>% arrange(Distance)
      },
      aes(x = Distance, y = adjustedHt),
      size = 4,
      color = "black"
    ) +
    # scale_color_manual(values = c(tnc_color("Spring Green"), tnc_color("Crimson"), tnc_color("Lake"))) +
    # Arrow showing stream flow. #TODO: Fix- mapping not working properly.
    annotate(
      geom = "segment",
      data = lon,
      x = 0,
      xend = (max(lon$Distance)) / 3,
      y = (min(height$adjustedHt) * 1.1) - (max(height$adjustedHt) * 1.1),
      yend = (min(height$adjustedHt) * 1.1) - (max(height$adjustedHt) * 1.1),
      size = 3,
      alpha = 0.9,
      arrow = arrow()
    ) +
    scale_x_continuous(limits = c(0, max(lon$Distance)), expand = expansion(0)) +
    scale_y_continuous(limits = c(min(height$adjustedHt) * 1.1, max(height$adjustedHt) * 3), expand = expansion(c(0, .3))) +
    # xlim(0, max(lon$Distance)) +
    # ylim(min(height$adjustedHt) * 1.1, max(height$adjustedHt) * 1.5) +
    labs(x = "Distance from Upstream Hydraulic Control (feet)",
         y = "NAVD88 (feet)") +
    theme_ipsum_rc() +
    theme(
      plot.subtitle = element_text(family = "sans"),
      plot.caption = element_text(family = "sans"),
      # panel.grid.major = element_line(colour = "gray85",
      #                                 size = 0.4),
      # panel.grid.minor = element_line(colour = "gray85",
      #                                 size = 0.3),
      axis.title = element_text(family = "sans", size = 12),
      plot.title = element_text(family = "sans",
                                size = 14),
      # panel.background = element_rect(fill = NA),
      legend.position = "right"
    ) + labs(
      title = "",
      subtitle = paste0("Crossing #: ", ccode),
      size = 12
    )
  
  crossPlot
  
}

# drawCrossing <- possibly(drawCrossing, otherwise = "Insufficient Data")
drawCrossing <- purrr::safely(drawCrossing, otherwise = "Insufficient Data") # Add in default plot here.
# # Test out plots and inspect.
a <- LIculvertAssessments %>%
  mutate(longPlots = map2(.x = longProfile, .y = crossSectionProfile, .f = ~drawCrossing(longitudinal = .x, crossSectional = .y)))
# # 
a %>% filter(crossingID == 339) %>% pull(longPlots)
a %>% sample_n(1) %>% pull(longPlots)#
# # #
sample_n(a, 1) %>% pull(longPlots)
# 
#
