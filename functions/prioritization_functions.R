## Tidal Crossing Assessment and Prioritizations
# 

#' Function definitions serve two purposes; documentation and portability.
#'  Calculation of the Prioritization metric 'Evaluation Score' is a 2 (or more) step 
#'  process. 
#' First- calculation of 'raw' values as measured in field and desktop data are made.
#'  This allows modification of the way in which the raw values are included and provides 
#'  an intermediary step for assessment of distribution of values, data viz and/or EDA.
#' 
#' Second- those calculated metrics are then rescaled/scored using evaluation criteria
#'  which are laid out within function documentation. This allows modification of scoring 
#'  criteria for additional assessment of sensitivity. 
#' 
#'


#**********************************************
# E1. Salt Marsh Complex Size ----

#' TODO: Confirm what field this is referencing from Karen's model outputs.
#' likely to be da_saltMarshArea
#'


#**********************************************
# E2. Salt Marsh Size upstream (whole watershed)----

#' TODO: Confirm what field this is referencing from Karen's model outputs.
#' likely to be da_WatershedLndCover_wetland
#'
#'


#**********************************************
# E3. Degree of Tidal Restriction and AOP ----

#' The final score for E3 will be the mean of tidal range ratio score, crossing ratio score,
#' and erosion classification score. Each component will be calculated individually and then rolled 
#' up into a single function for the result of E3.
#'


# Tidal Range Ratio ----
#' E3(a)

#' tidal_range_ratio
#'
#' @param us_hwi_stain 
#' @param ds_hwi_stain 
#' @param us_low_tide_elevation 
#' @param ds_low_tide_elevation 
#'
#' @return
#' @export
#'
#' @examples
#' 
tidal_range_ratio <- function(us_hwi_stain,
                              ds_hwi_stain,
                              us_low_tide_elevation,
                              ds_low_tide_elevation){
  ratio <- ((us_hwi_stain - us_low_tide_elevation) / (ds_hwi_stain - ds_low_tide_elevation))
  return(ratio)
}

# Criteria Score Tidal Range Ratio ====
#' crit_tidal_range
#'
#' @param tide_r_ratio 
#'
#' @return numeric tidal range ratio score
#' @export
#'
#' @examples
#' 
crit_tidal_range <- function(tide_r_ratio){
  val <- tide_r_ratio
  dplyr::case_when(
    val >= 0.90 ~ 1, # TODO: This needs to be built out more. Other variables feed into this in NH docs
    val >= 0.80 & val < 0.90 ~ 2,
    val >= 0.70 & val < 0.80 ~ 3,
    val >= 0.50 & val < 0.70 ~ 4,
    val < 0.50 ~ 5
  )
}


# Crossing Ratio ====
#' Crossing Ratio 
#'  calculate the raw upstream or downstream crossing ratio for use in evaluating score and criteria.
#'
#' @param channelWidth UP or DOWN stream channel widths, da_ChannelWidth_dwnStream or da_ChannelWidth_upStream
#' @param dimA UP or DOWN stream structure dimension A; CrosDim_dwnA or CrosDim_upA
#' @param dimC UP or DOWN stream structure dimension C
#'
#' @description Must be referring to consistent position within stream, UP or DOWN.
#' 
crossing_ratio <- function(channelWidth, dimA, dimC){
  cr <- channelWidth/mean(dimA, dimC, na.rm = TRUE, trim = 0)
  
  return(cr)
}
  
# Crossing Ratio Criteria Score ====
#' crit_crossing_ratio
#' Crossing Ratio Criteria Score
#'
#' @param crossing.ratio 
#'
#' @return factor numeric score
#' @export 
#'
#' @examples
crit_crossing_ratio <- function(crossing.ratio){
  score <- cut(x = crossing.ratio, 
               # Adjust the breaks for as needed 
               breaks = c(-Inf, 0, 1, 1.2, 2.5, 5, Inf), 
               # Resulting scored value based on NH document pg. 22
               labels = c(0, 1, 2, 3, 4, 5))
  
}


#*****************
# Erosion Classification ----
#' erosion_class
#'
#' @param scour_pool 
#' @param channel_width 
#'
#' @return numeric ratio of scour pool width to channel width. Meant to be calculated for up and downstreams.
#' @export
#'
#' @examples
#' 
erosion_class <- function(scour_pool, channel_width){
  eclass <- scour_pool / channel_width
  
  return(eclass)
}

#' crit_erosion_class
#'
#' @param us_eclass 
#' @param ds_eclass 
#'
#' @return
#' @export
#'
#' @examples
crit_erosion_class <- function(us_eclass, ds_eclass){
  crit <- function(er){
    case_when(
      er <= 1 ~ 1,
      er > 1 & er <= 1.2 ~ 2, 
      er > 1.2 & er <= 2.0 ~ 3,
      er > 2.0 & er <= 3 ~ 4,
      er > 3 ~ 5
    )
  }
  crit_e_class <- max(crit(us_eclass), crit(ds_eclass), na.rm = TRUE)
  return(crit_e_class)
}

#**********************************************
# TAOP Tidal Aquatic Organism Passage ----
#' 
#' TAOP is calculated using the same methodology as tidal range ratio
#' as.per NH Resilient Tidal Crossings 
#' TODO: Add citation to NH documentation.
taop <- tidal_range_ratio 
crit_taop <- crit_tidal_range

#*****************
#' E3 final score
#' 
#' 
DTOR_TAOP <- function(tidal_rng_score, crossing_ratio_score, erosion_class_score, taop){
  scores <- c(tidal_rng_score, crossing_ratio_score, erosion_class_score, taop)
  if(all(scores %in% 1:5)){
    finalscore <- mean(scores, na.rm = TRUE)
    return(finalscore)
  } else{
    return("Scores not valid. Check individual components to ensure scores are between 1 and 5.")
  }
  
}


