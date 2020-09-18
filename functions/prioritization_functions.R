## Tidal Crossing Assessment and Prioritizations
# 
# Submetric scores and Final Score calculated in tidalCrossing_Prioritizations.Rmd using these functions.

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
# TODO: Revise and update documentation 

#**********************************************
# E1. Salt Marsh Complex Size ----

#' Scoring assigned by supervised classifciation of
#' crossings based on proximity to marsh complexes. Crossings near large complexes
#' (>~15 acres in size) scored 5, crossings adjacent to smaller complexes were
#' scored a 3, and crossings disconnected from any marsh complex were scored a 1.


#**********************************************
# E2. Salt Marsh Size upstream (whole watershed)----

#' Scores binned in tidalCrossing_Prioritization.Rmd
#' Catchment deliniated around each crossing and acreage 
#' extracted from SLAMM current conditions raster. 
#' # TODO: Ask Karen for confirmation and write up of 
#' methods (likely to be similar to FW)
#'


#**********************************************
# E3. Degree of Tidal Restriction and AOP ----

#' The final score for E3 will be the mean of tidal range ratio score, crossing ratio score,
#' and erosion classification score. Each component will be calculated individually and then rolled 
#' up into a single function for the result of E3.
#'


# E3(a) Tidal Range Ratio ----
#' 

#' tidal_range_ratio
#'
#' @param us_hwi_stain Upsream stain high water indicator (HWI)
#' @param ds_hwi_stain Downstream stain high water indicator
#' @param us_low_tide_elevation Upstream low tide elevation corrected to NAVD88 (ft)
#' @param ds_low_tide_elevation Downstream low tide elevation corrected to NAVD88 (ft)
#'
#' @return numeric value 0-1
#' @export
#' @description Tidal range ratio is a measure of the ratio of the tidal range upstream to the tidal range downstream
#' of the crossing. A ratio approaching 1 signifies that tides rise and fall equally on both sides, indicating that 
#' the structure is not significantly impeding tidal flow. Small tidal range ratios indicate that there is a larger tidal range 
#' downstream (tideward) of the structure than upstream (landward). 
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
  sc <- dplyr::case_when(
    val >= 0.90 ~ 1L, 
    val >= 0.80 & val < 0.90 ~ 2L,
    val >= 0.70 & val < 0.80 ~ 3L,
    val >= 0.50 & val < 0.70 ~ 4L,
    val < 0.50 ~ 5L
  )
  return(sc)
}


# 3(b) Crossing Ratio ----
#' 
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
  
  score <- dplyr::case_when(
    crossing.ratio >= 5 ~ 5L, 
    crossing.ratio >= 2.5 ~ 4L,
    crossing.ratio >= 1.25 ~ 3L,
    crossing.ratio >= 1 ~ 2L,
    crossing.ratio < 1 ~ 1L
  )
  return(score)
}


#*****************
# 3(c) Erosion Classification ----
#' 
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
      er > 3 ~ 5L,
      er > 2.0 ~ 4L, 
      er > 1.2 ~ 3L,
      er > 1 ~ 2L,
      er > 0 ~ 1L
    )
  }
  crit_e_class <- pmax(crit(us_eclass), crit(ds_eclass), na.rm = TRUE)
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
# E3: Final Score ----
#' 
#' 
degtidalrestr <- function(tidal_rng_score, crossing_ratio_score, erosion_class_score){
  scores <- c(tidal_rng_score, crossing_ratio_score, erosion_class_score)
  finalscore <- mean(scores, na.rm = TRUE) 
    return(finalscore)
}



#****************************************
# E4. Vegetation Matrix ----
#'
#' Vegetation matrix scoring
#' Used 'Resilient Tidal Crossings' documentation as guide
#' for scoring. 
#' 
#' @param vegMatChoice 
#' 

vegetationScore <- function(vegMatChoice, formerlyConnected){
  score <- case_when(
    formerlyConnected == 1 ~ 5L, # If formerly connected score a 5, else get result of field assessment.
    vegMatChoice == "1A" ~ 1L, # native only, same both sides
    vegMatChoice == "1B" ~ 3L, # native only, different species on either side but appear similar (high marsh - low marsh)
    vegMatChoice == "1C" ~ 5L, # native only, tidal species one side, fresh species the other.
    vegMatChoice == "2A" ~ 1L, # invasives through out, same both sides
    vegMatChoice == "2B" ~ 2L, # invasives through out, up and down stream communtites are slightly different
    vegMatChoice == "2C" ~ 4L, # invasives through out, up and down stream communities represent different marsh types
    vegMatChoice == "3A" ~ 3L, # invasives on one side, tidal marsh on both sides
    vegMatChoice == "3B" ~ 4L, # invasives on one side, similar species/marsh type on either side 
    vegMatChoice == "3C" ~ 5L # invasices on one side, up and down stream different species
  )
  return(score)
}


# C1: Crossing Condition ----
#' crossingConditionScore
#'
#' @param overallCond 
#' @param hwall_upCond 
#' @param wwall_upCond 
#' @param hwall_dwnCond 
#' @param wwall_dwnCond 
#'
#' @return
#' @export
#' @details had run the crossing conditon using NH methods, 
#' NH methods with scour pulled out and TNC method. 
#' TNC method resulted in the best distribution for scoring. 
#' 
#' @examples
crossingConditionScore <- function(overallCond, 
                                      hwall_upCond, 
                                      wwall_upCond, 
                                      hwall_dwnCond,
                                      wwall_dwnCond,
                                      roadCond){
  # tally the number of scores given across the 5 areas condition is assessed at.
  rescore <- function(input){
    
    val <- case_when(
      input == 2 ~ 1,
      input == 3 ~ 3,
      input == 4 ~ 5
    )
    return(val)
  }
  
  finalscore <- mean(rescore(overallCond), 
                     rescore(hwall_upCond), 
                     rescore(hwall_dwnCond), 
                     rescore(wwall_dwnCond), 
                     rescore(wwall_upCond), 
                     rescore(roadCond),
                     na.rm = TRUE, trim = 0)
  
  return(finalscore)
}



# C3: highwater_risk_lack_of _clearance ---------------------------------------------------------
#' 
#' 
#' See: https://streamcontinuity.org/sites/streamcontinuity.org/files/pdf-doc-ppt/Scoring%20System%20for%20Tidal%20Crossings%2003-19-19.pdf

#' highwater_clearance
#'
#'
#' @param us_hwi_stain 
#' @param ds_hwi_stain 
#' @param us_ceiling 
#' @param ds_ceiling 
#' @param us_hwi_wrack 
#' @param ds_hwi_wrack 
#' @param us_road 
#' @param ds_road 
#'
#' @return
#' @export
#'
#' @examples
highwater_clearance <-
  function(us_hwi_stain = `HWI Stain US`,
           ds_hwi_stain =  `HWI Stain DS`,
           us_ceiling = `Ceiling of Structure US`,
           ds_ceiling = `Ceiling of Structure DS`,
           us_hwi_wrack = `HWI Wrack US`,
           ds_hwi_wrack = `HWI Wrack DS`,
           us_road = `Road Surface US`,
           ds_road = `Road Surface DS`) {
    # risk of high water - upstream side
    # wrack and road
    # if wrack is below the stain than we want to use the stain to avoid unintended bias based on wrack left by an outgoing lower tide- 
    
    highwater_dif_US <-  us_road -  us_hwi_wrack
    # scored
    score_highwater_dif_US <-
      as.numeric(
        cut(x = highwater_dif_US, breaks = c(-Inf, 0, 1.5, 3, 6, Inf)),
        labels = 5:1,
        ordered_result = FALSE
      )
    # risk of high water - downstream side
    highwater_dif_DS <-  ds_road - ds_hwi_wrack
    # scored
    score_highwater_dif_DS = as.numeric(
      cut(x = highwater_dif_DS, breaks = c(-Inf, 0, 1.5, 3, 6, Inf)),
      labels = 5:1,
      ordered_result = FALSE
    )
    
    # clearance - downstream side
    clearance_dif_DS <- ds_ceiling - ds_hwi_stain
    # scored
    score_clearance_dif_DS <-  as.numeric(
      cut(x = clearance_dif_DS, breaks = c(-Inf, 0, 1, 2, 3, Inf)),
      labels = 5:1,
      ordered_result = FALSE
    )
    # clearance - upstream side
    clearance_dif_US <-  us_ceiling - us_hwi_stain
    # scored
    score_clearance_dif_US = as.numeric(
      cut(x = clearance_dif_US, breaks = c(-Inf, 0, 1, 2, 3, Inf)),
      labels = 5:1,
      ordered_result = FALSE
    )
    # return max of clearances if they are available-
    clearance_Sc <-  max(score_clearance_dif_DS,
                         score_clearance_dif_US,
                         na.rm = T)
    highwater_Sc <- max(score_highwater_dif_DS,
                        score_highwater_dif_US,
                        na.rm = T)
    if (is.na(clearance_Sc)) {
      return(highwater_Sc)
    } else {
      return(clearance_Sc)
    }
  }

# Resilience Benefit ----
#'
#' Scores are calculated in tidalCrossing_Prioritizations.Rmd



# Total Benefit Score
# From Stephen- All benefit scores were rescaled using this break down.
# Dim EB
# If  [Total_EcoBenefit] > 16 Then
# EB = 5
# elseif  [Total_EcoBenefit] > 12 Then
# EB = 4
# elseif  [Total_EcoBenefit] > 8 Then
# EB = 3
# elseif  [Total_EcoBenefit] > 4 Then
# EB = 2
# elseif  [Total_EcoBenefit] > 0 Then
# EB = 1
# else
# EB = -99
# end if
# From Stephen- For scroing resilience benefit in FW prioritizations
# Dim RB
# If  [Total_ResilienceBenefit] > 12 Then
# RB = 5
# elseif  [Total_ResilienceBenefit] > 9 Then
# RB = 4
# elseif  [Total_ResilienceBenefit] > 6 Then
# RB = 3
# elseif  [Total_ResilienceBenefit] > 3 Then
# RB = 2
# elseif  [Total_ResilienceBenefit] > 0 Then
# RB = 1
# else
# RB = -99
# end if
benefit_score <- function(total.benefit.score, resilience.score = F, transporation.score = F){
  if(resilience.score == F){
    score <- cut(x = total.benefit.score, breaks = c(-Inf, 4, 8, 12, 16, Inf), labels = c(1:5))
    score <- as.numeric(score)
  }
  if(transporation.score == T){
    score <- cut(x = total.benefit.score, breaks = c(-Inf, 2, 4, 6, 8, Inf), labels = c(1:5))
    score <- as.numeric(score)
  }
  else{
    score <- cut(x = total.benefit.score, breaks = c(-Inf, 3, 6, 9, 12, Inf), labels = c(1:5))
    score <- as.numeric(score)
  }
  return(score)
}

# Total Prioritization Score


