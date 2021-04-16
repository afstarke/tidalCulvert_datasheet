# Tidal summary sheet longitudinal plot

#' Depends on rmdSetup.R for updates on the data pulled from ArcGIS Online and the worksheets.
#' 1) connect to VPN and T and M drives
#' 2) #' source("code/rmdSetup.R")
#'



drawCrossing <- function(longitudinal, crossSectional) {
  
  
  ccode <- longitudinal$crossingID[1]
  lon <- longitudinal
  cross <- crossSectional 
   # Supply a default longitidinal profile tibble is there isn't one provided in the input data
   # This was not being helpful... Presneting a plot of non-actual data was a bit misleading.
   # 
   # Add in logicals to catch missing data that leads to terrible plots.
   # 
  if (sum(str_detect(string = lon$`Feature Code`, pattern = "I")) < 2) {
    stop("Crossing lacks sufficient data to present a longitudinal plot")
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
    "Road Surface" = "black",
    # Grey 50
    "Culvert structure" = "grey70" # Grey 40
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
  lowtide <- cross %>% filter(Feature == "Low Tide Water Elevation") %>%
    slice(rep(1:n(), each = 2)) %>% arrange(Distance)
  lowtide$Distance[1] <- -5
  lowtide$Distance[4] <- max(longitudinal$Distance + 5, na.rm = T)
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
  
  axisLims_y <- c(min(cross$adjustedHt, lon$adjustedHt, na.rm = T), 1.2 * (max(cross$adjustedHt, lon$adjustedHt, na.rm = T)))
  axisLims_x <- c(min(cross$Distance, lon$Distance, na.rm = T), 1.1 * max(cross$Distance, lon$Distance, na.rm = T))
  us_adj <- min(culvert$Distance)/3 # this is the distance along the stream where the culvert is located.
  ds_adj <- (max(axisLims_x) - max(culvert$Distance)) / 3 # adjustment value DS which splits the segment between the culvert and the end of the plot into 3
  xo <- min(axisLims_x) #x0 start of the plot.
  xtwo <- max(culvert$Distance) # start of the ds segments.
  
  # browser()
  
  # WaterIndicators provide the data that the legend depends on
waterIndicators <- cross %>%
  filter(str_detect(Feature, pattern = "HWI") |
           Feature == "Low Tide Water Elevation" | # Added water level as ribbon as well
           str_detect(Feature, pattern = "Marsh")) %>% 
  mutate(Feature = fct_reorder(Feature, adjustedHt), # reorder the levels based on the height to try and align the plot and the legend.
  # Rewrite... Declare the x values explicitly to avoid overlapping features.
  Distance_calc = case_when(Feature == "Marsh Plain Shot" & Position == "US" ~ xo - 2,
                       Feature == "HWI Wrack" & Position == "US" ~ xo + us_adj -2,
                       Feature == "HWI Stain" & Position == "US" ~ xo + 2*(us_adj) -2,
                       
                       Feature == "HWI Stain" & Position == "DS" ~ xtwo -2,
                       Feature == "HWI Wrack" & Position == "DS" ~ xtwo + ds_adj -2,
                       Feature == "Marsh Plain Shot" & Position == "DS" ~ xtwo + 2*(ds_adj) -2,
                       TRUE ~ NA_real_), #) %>% 
  endDist = case_when(Feature == "Marsh Plain Shot" & Position == "US" ~ xo + us_adj + 2,
                      Feature == "HWI Wrack" & Position == "US" ~ xo + 2*(us_adj) + 2,
                      Feature == "HWI Stain" & Position == "US" ~ xo + 3*(us_adj) + 2,
                      
                      Feature == "HWI Stain" & Position == "DS" ~ xtwo + ds_adj + 2,
                      Feature == "HWI Wrack" & Position == "DS" ~ xtwo + 2*(ds_adj) + 2,
                      Feature == "Marsh Plain Shot" & Position == "DS" ~ xtwo + 3*(ds_adj) + 2,
                      TRUE ~ NA_real_)) %>%
  add_row(Feature = "Road Surface", Position = NA, Height = NA, Distance = NA, Distance_calc = NA, endDist = NA) %>% # add a blank field here to bring this into the legend.
  add_row(Feature = "Culvert structure", Position = NA, Height = NA, Distance = NA, Distance_calc = NA, endDist = NA)

 # browser()
# 
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
     size = 3,
     alpha = 1,
     arrow = arrow(length = unit(0.15, "inches"), type = "closed", angle = 25),
     lineend = "round", 
     linejoin = "round",
     color = "grey70"
   ) +
     geom_label(aes(
       label = "Stream flow",
       x = 0,
       y = max(axisLims_y, na.rm = T)),
       color = "grey40",
       hjust = 0
     ) +
     
    # fill between road and culvert ====
    geom_polygon(data = roadFill,
    aes(x = Distance, y = adjustedHt),
    fill = tnc_color("Canyon")) +
     
    # stream bed ====
   geom_ribbon(data = lon, aes(ymax = adjustedHt, ymin = min(adjustedHt, na.rm = T) - .5 , x = Distance),
      linetype = 3,
      # pattern = 'd',
      fill = "grey35",
      alpha = 1) +
     
     # Water indicators
     geom_segment(
       data = waterIndicators,
       aes(x = Distance_calc, 
           y = adjustedHt, 
           color = Feature, 
           xend = endDist, 
           yend = adjustedHt),
       size = 2,
       linetype = "solid"
     ) + 
    
    # culvert structure ====
    geom_shape(data = culvert, aes(y = adjustedHt, x = Distance),
      fill = "grey70",
      color = "black", 
      alpha = 1) +
   
    # Road surface ====
    geom_line(
      data = roadSurface,
      aes(x = Distance, y = adjustedHt),
      size = 3,
      lineend = "round", 
      linejoin = "round",
      color = "black"
    ) +
   #  # road surface label ====
  
     # Legend labels and descriptors
    scale_color_manual(values = crossColors, drop = FALSE,
                       breaks = c('HWI Stain', 'HWI Wrack', 'Marsh Plain Shot', 'Low Tide Water Elevation', 'Road Surface', 'Culvert structure'),
                       labels = c("<strong style='color:#c5351c'> High water indicator - stain </strong> <br> darkened stain representing <br>typical daily high tide levels",
                                  "<strong style='color:#a0c04d'> High water indicator - wrack </strong> <br> indicates debris deposited <br>by recent high water event",
                                  "<strong style='color:#00703c'> Marsh surface: </strong> <br> Average marsh <br>surface elevation",
                                  "<strong style='color:#23487a'> Low tide elevation: </strong> <br> Recorded at time<br> of field survey",
                                  "<strong style='color:#2E2E2E'> Road surface: </strong> <br>Center elevation obtained <br>from lidar (USGS 2014)",
                                  "<strong style='color:#666666'> Culvert structure:</strong><br>Estimated shape and <br>position relative to <br>the road surface")) + 
    # scale_size(guide = "none") +
    # # Stream bed label
    # annotate(geom = "label", 
    #          label = "Stream bed", 
    #          color = "black", 
    #          hjust = 0,
    #          # nudge_x = 3,
    #          x = 2,
    #          y = min(axisLims_y, na.rm = T) + .25) +
   # geom_label_repel(data = labels_df,
   #                  mapping = aes(x = x, y = y, label = label, hjust = hjust),
   #                  color = "black", size = 4, 
   #                  # force = .1,
   #                  # label.margin = grid::unit(rep(4, 4), "pt"),
   #                  # min.segment.length = Inf,
   #                  # direction = "both",
   #                  # box.padding = 2, 
   #                  # force_pull = 5,
   #                  point.size = NA,
   #                  xlim = c(-Inf, NA)) +
     annotate(geom = "text", x = axisLims_x, y = axisLims_y, label = "  Stream Bed", color = "white", hjust = 0, size = 4) +
     # Scales for limits and ticks. ====
    # scale_x_continuous(limits = c(0, max(lon$Distance) + 10), expand = expansion(0)) + 
    # scale_y_continuous(limits = axisLims, expand = expansion(c(0, .3))) + # using scale_*_continuous clips data that lands outside the lims.
    coord_cartesian(ylim = axisLims_y, xlim = c(0, max(axisLims_x + 10, na.rm = T)), clip = "off") + # Using coords_cartesian zooms in on area.
    
    labs(x = "Distance from Upstream Hydraulic Control (ft)",
         y = "Elevation in NAVD88 (ft)", size = "legend", color = "legend") +
     guides(color = guide_legend(override.aes = list(size = c(2,2,2,2,3,12)))) +
    theme_ipsum_rc() +
    theme(
      plot.subtitle = element_text(family = "sans"),
      plot.caption = element_text(family = "sans"),
      axis.text.x = element_text(margin = margin(t = 20)),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.title = element_text(family = "sans", size = 12, margin = margin(t = 12)),
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
      size = 12,
      color = ""
    )
  crossPlot
  
    
  
}

# plotter(9999)
# plotter(528)

# drawCrossing <- purrr::safely(drawCrossing, otherwise = "Insufficient Data") # Add in default plot here.
drawCrossing <- purrr::possibly(drawCrossing, otherwise = glue::glue("Crossing lacks sufficient data to present a longitudinal plot"))
# # Test out plots and inspect.
# # ## Run single crossing through function (quicker)
# # ## Best plot for longitudinals so far... ----
# test2longitudinalProfile <- LIculvertAssessments %>% filter(crossingID == 441) %>% select(longProfile) %>% unnest(cols = longProfile)
# # View(test2longitudinalProfile)
# # test2crossHeight <- LIculvertAssessments %>% filter(crossingID == 441) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
# # View(test2crossHeight)
# 
# # drawCrossing(longitudinal = test2longitudinalProfile, crossSectional = test2crossHeight)
# 
# # troubleshooter = TRUE # turn this on/off for troubleshooter mode.
# # # # 
# # # # # if(troubleshooter == TRUE){
# # # # # #   source("00_libraries.R")
  plotter <-
    function(crosscode) {
      test2longitudinalProfile <-
        LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(longProfile) %>% unnest(cols = longProfile)
      # View(test2longitudinalProfile)
      test2crossHeight <-
        LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
      # View(test2crossHeight)
      drawCrossing(longitudinal = test2longitudinalProfile, crossSectional = test2crossHeight)
    }


  plotter_data <-
    function(crosscode) {
      test2longitudinalProfile <-
        LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(longProfile) %>% unnest(cols = longProfile)
      # View(test2longitudinalProfile)
      test2crossHeight <-
        LIculvertAssessments %>% filter(crossingID == crosscode) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
      # View(test2crossHeight)
      df <- list(test2longitudinalProfile, test2crossHeight)
      df #%>% purrr::walk(View)
      # df %>% purrr::map(View)
    }
# # # # # # # 
# # # # # # # # # # }
# # # # # # # # # #
# # c(96, 110, 9999, 441, 105, 49) %>% map(plotter)
# # # # # # # # # # #
# # # # # # plotter(9999)
# # # plotter(528)
# plotter(441)
# plotter_data(441)
# # # plotter_data(507)
# # # # # # # plotter(427)
# # # # plotter(105)
# plotter(140)
# # # # # priorityCrossings %>% map(plotter)

# 
# plotter_data(112)
# plotter_data(415)
# plotter_data(528)
# plotter_data(49)
# plotter_data(107)
# plotter_data(106)
# # #
