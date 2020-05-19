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

#' #CHANGED:
#' Scoring assigned by supervised classifciation of
#' crossings based on proximity to marsh complexes. Crossings near large complexes
#' (>~15 acres in size) scored 5, crossings adjacent to smaller complexes were
#' scored a 3, and crossings disconnected from any marsh complex were scored a 1.


#**********************************************
# E2. Salt Marsh Size upstream (whole watershed)----

#' #REVIEW: Confirm what field this is referencing from Karen's model outputs.
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
    val >= 0.90 ~ 1L, 
    val >= 0.80 & val < 0.90 ~ 2L,
    val >= 0.70 & val < 0.80 ~ 3L,
    val >= 0.50 & val < 0.70 ~ 4L,
    val < 0.50 ~ 5L
  )
}


# Crossing Ratio ----
#' 3(b)
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
# Erosion Classification ----
#' 3(c)
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
  # if(all(scores %in% 1:5)){
  #   finalscore <- mean(scores, na.rm = TRUE) #REVIEW: Confirm that we would want to drop any NAs
  #   return(finalscore)
  # } else{
  #   return("Scores not valid. Check individual components to ensure scores are between 1 and 5.")
  # }
  # 
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

vegetationScore <- function(vegMatChoice){
  score <- case_when(
    vegMatChoice == "1A" ~ 1L,
    vegMatChoice == "1B" ~ 3L,
    vegMatChoice == "1C" ~ 5L,
    vegMatChoice == "2A" ~ 0L,
    vegMatChoice == "2B" ~ 0L,
    vegMatChoice == "2C" ~ 0L,
    vegMatChoice == "3A" ~ 3L,
    vegMatChoice == "3B" ~ 4L,
    vegMatChoice == "3C" ~ 5L
  )
  score2 <- case_when(
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
  return(score2)
}

#' # Current Crossing Condition ----
#' 
#' #**********************************************
#' # C1.v1 Crossing Condition ----
#' #' Replicating the NH protocol 
#' #' [29 April 2020] Team confirmed no scour used in this metric. Captured in C4. Erosion class below.
#' #' 
#' #' TODO: agree on scoring method- 1-5; 1-3-5.
#' #'
#' #' 
#' #' @param overallCond 
#' #' @param hwall_upCond 
#' #' @param wwall_upCond 
#' #' @param hwall_dwnCond 
#' #' @param wwall_dwnCond 
#' #' @param scourUp 
#' #' @param scourDwn 
#' #' @param scourIn 
#' 
#' crossingConditionScore_v3 <- function(overallCond, 
#'                                    hwall_upCond, 
#'                                    wwall_upCond, 
#'                                    hwall_dwnCond,
#'                                    wwall_dwnCond,
#'                                    scourUp, 
#'                                    scourDwn, 
#'                                    scourIn){
#'   # scour tally for determining number of 'high' scour scores.
#'   # 'High' scour is ranked a 5 in the workbooks, therefore sum of 5
#'   highscours <- sum(scourUp == 5, scourDwn == 5, scourIn == 5, na.rm = TRUE) # get tally of 'high' scour scores.
#'   poorConditions <- sum(hwall_upCond == 4, hwall_dwnCond == 4, wwall_dwnCond == 4, wwall_dwnCond == 4, overallCond == 4, na.rm = TRUE) # tally 'poor' conditions
#'   combinedScores <- sum(highscours, poorConditions, na.rm = TRUE)
#'   goods <- sum(hwall_upCond == 2, hwall_dwnCond == 2, wwall_dwnCond == 2, wwall_dwnCond == 2, overallCond == 2, na.rm = TRUE)
#'   fairs <- sum(hwall_upCond == 3, hwall_dwnCond == 3, wwall_dwnCond == 3, wwall_dwnCond == 3, overallCond == 3, na.rm = TRUE)
#'   goodfairDiff <- goods - fairs # from NH protocol: Difference between tallied number of good and fair condition scores (presumable across sturcture)
#'   
#'   # Scoring section
#'   finalscore <- dplyr::case_when(
#'     overallCond == 1 ~ NA_integer_, # 1 is coded as blank, or unassessed in workbook.
#'     combinedScores >= 3 ~ 5L, # note 2 ways to score a 5. If there are 3 or more high scours and poor conditions
#'     overallCond == 3 | hwall_upCond == 5 & wwall_upCond == 5 & hwall_dwnCond == 5 & wwall_dwnCond == 5 ~ 5L, # note 2 ways to score a 5. If overall a poor and there's no wingwalls or headwalls
#'     combinedScores == 2 | overallCond == 4 ~ 4L,
#'     poorConditions == 1 | highscours == 1 ~ 3L,
#'     poorConditions == 0 & highscours == 0 & goodfairDiff < 1 ~ 2L,
#'     poorConditions == 0 & highscours == 0 & goodfairDiff > 1 ~ 1L
#'     
#'   )
#'  
#'   return(finalscore)
#' }
#' 
#' 
#' #**********************************************
#' # C1.v2 Crossing Condition V2----
#' #'
#' #' TODO: agree on scoring method .
#' #' This method splits out just the scoring of condition, _does not include scouring scores_.
#' #' From Teams:
#' #' 4-5 good conditions  => 1 
#' #' 'mostly fair' conditions (no poors, and more fairs than goods)  => 2
#' #' one poor condition => 3
#' #' 2 poor conditions => 4
#' #' '> = ' 3 poor conditions  => 5 (NH also scores crossings with out wing and headwalls a 5 if the overall condition is poor)
#' #' 
#' #' @param overallCond 
#' #' @param hwall_upCond 
#' #' @param wwall_upCond 
#' #' @param hwall_dwnCond 
#' #' @param wwall_dwnCond 
#' 
#' crossingConditionScore_v2 <- function(overallCond, 
#'                                       hwall_upCond, 
#'                                       wwall_upCond, 
#'                                       hwall_dwnCond,
#'                                       wwall_dwnCond){
#'   # tally the number of scores given across the 5 areas condition is assessed at.
#'   goods <- sum(hwall_upCond == 2, hwall_dwnCond == 2, wwall_dwnCond == 2, wwall_dwnCond == 2, overallCond == 2, na.rm = TRUE) # tally of 'Good'
#'   fairs <- sum(hwall_upCond == 3, hwall_dwnCond == 3, wwall_dwnCond == 3, wwall_dwnCond == 3, overallCond == 3, na.rm = TRUE) # tally of 'Fair'
#'   poors <- sum(hwall_upCond == 4, hwall_dwnCond == 4, wwall_dwnCond == 4, wwall_dwnCond == 4, overallCond == 4, na.rm = TRUE) # tally 'poor' conditions
#'   goodfairDiff <- goods - fairs # from NH protocol: Difference between tallied number of good and fair condition scores (presumable across sturcture)
#'   
#'   # Scoring section
#'   finalscore <- dplyr::case_when(
#'     overallCond == 1 ~ NA_integer_, # 1 is coded as blank, or unassessed in workbook.
#'     overallCond == 3 | hwall_upCond == 5 & wwall_upCond == 5 & hwall_dwnCond == 5 & wwall_dwnCond == 5 ~ 5L, # note 2 ways to score a 5. If overall a poor and there's no wingwalls or headwalls
#'     poors == 2 | overallCond == 4 ~ 4L,
#'     poors == 1  ~ 3L,
#'     poors == 0 & fairs > goods ~ 2L,
#'     poors == 0 & goods >= 4 ~ 1L
#'     
#'   )
#'   
#'   return(finalscore)
#' }

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



# C3: highwater_clearance_ratio ---------------------------------------------------------
#' an attempt at scoring based on a ratio of elevations (high water and road) 
#' to avoid issue with a static scoring method across different tidal 
#' regime/heights. 
#' 
#' See: https://streamcontinuity.org/sites/streamcontinuity.org/files/pdf-doc-ppt/Scoring%20System%20for%20Tidal%20Crossings%2003-19-19.pdf

#' highwater_ratio
#'
#' @param hwi High water indicator- one of either wrack line or stain on structure
#' @param poi Road height as acquired by LIDAR via desktop assessment portion of assessment
#'
#' @return
#' @export
#'
#' @examples
highwater_ratio <- function(hwi, poi){
  
  highwaterRescale <- function(a,b){
    minVal <- min(a,b, na.rm = T)
    rescaleVal <- abs(minVal) + .00001 # Make it very small but above zero
    return(rescaleVal)
  }
  
   ratio <- (highwaterRescale(hwi, poi) + hwi) / (highwaterRescale(hwi, poi) + poi)
  
  return(ratio)
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
benefit_score <- function(total.benefit.score, resilience.score = F){
  if(resilience.score == F){
    score <- cut(x = total.benefit.score, breaks = c(-Inf, 4, 8, 12, 16, Inf), labels = c(1:5))
    score <- as.numeric(score)
  }else{
    score <- cut(x = total.benefit.score, breaks = c(-Inf, 3, 6, 9, 12, Inf), labels = c(1:5))
    score <- as.numeric(score)
  }
  return(score)
}

# Total Prioritization Score


