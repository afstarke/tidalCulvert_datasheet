Tidal spreadsheet workflow
================

## Take 2:

After some digging into the task of extracting data from these
spreadsheets, and the prospects of additional analyses (prioritization
sensitivity, perhaps) as well as easing the management of field and
desktop assessment needs, a new(ish) approach is laid out below. In
general a reboot of the teams initial proposal of using a sort of look
up key to identify the locations of important data from within the
spreadsheets.

To begin with, this process relies heavily on the tidyxl package, as
well as the tidyverse packages more broadly.

``` r
library(readr)
library(readxl)
library(tidyxl)
library(unpivotr)
library(tidyverse)

keysheet <- read_excel("../../../Box Sync/Culvert Assessment/Tidal Assessments/key.xlsx") #Use the working more complete version

source("functions/culvert_tidy.R")
source("functions/culvert_extract.R")    
```

Using tidyxl’s xlsx\_cells() function we pull in all the cells from a
given excel file. The cells arrive in a tidy data fashion where each row
holds the data value, some formating information, data type, and the
location in which it lives (the sheet and cell, specifically).  
Below is a snippet of a single sheet read into R using
tidxl::xlsx\_cells()

``` r
# TODO Start with basic read in of spreadsheet and extracting the proper cells.
sheet <- "spreadsheets/Ex/Copy of Tidal Field Data blank.xlsm" 
# Tidy up the sheets. 
cells <- xlsx_cells(sheet) %>% 
  filter(is_blank == FALSE, sheet != "Data Sheet - BLANK") 
  
cells
```

    ## # A tibble: 1,045 x 21
    ##    sheet address   row   col is_blank data_type error logical numeric
    ##    <chr> <chr>   <int> <int> <lgl>    <chr>     <chr> <lgl>     <dbl>
    ##  1 Data~ A1          1     1 FALSE    character <NA>  NA           NA
    ##  2 Data~ A5          5     1 FALSE    character <NA>  NA           NA
    ##  3 Data~ G7          7     7 FALSE    character <NA>  NA           NA
    ##  4 Data~ A9          9     1 FALSE    character <NA>  NA           NA
    ##  5 Data~ V9          9    22 FALSE    character <NA>  NA           NA
    ##  6 Data~ V10        10    22 FALSE    character <NA>  NA           NA
    ##  7 Data~ A11        11     1 FALSE    character <NA>  NA           NA
    ##  8 Data~ V11        11    22 FALSE    character <NA>  NA           NA
    ##  9 Data~ A12        12     1 FALSE    character <NA>  NA           NA
    ## 10 Data~ V12        12    22 FALSE    character <NA>  NA           NA
    ## # ... with 1,035 more rows, and 12 more variables: date <dttm>,
    ## #   character <chr>, character_formatted <list>, formula <chr>, is_array <lgl>,
    ## #   formula_ref <chr>, formula_group <int>, comment <chr>, height <dbl>,
    ## #   width <dbl>, style_format <chr>, local_format_id <int>

The next step is to scale this up to capture all the datasheets on hand.
We do this by creating two functions that work together. The first,
**culvert\_fetch()**, fetches the tidyxl cells when given a file path
name. The second function, **culvert\_tidy**, produces a data frame
containing all the files within the supplied folder, a few helpful bits
of info and a column containing the tidyxl cells from the output of
**culvert\_fetch()** for each data sheet.

``` r
culvert_fetch
```

    ## function (filepath) 
    ## {
    ##     cellNeeds <- c("I164", "I165", "I169", "P164", "P165", "P169", 
    ##         "W164", "W165", "W169")
    ##     cells <- tidyxl::xlsx_cells(filepath) %>% filter(sheet != 
    ##         "Data Sheet - BLANK") %>% select(sheet, address, data_type:formula) %>% 
    ##         mutate(date = as.character(date)) %>% gather(error:character, 
    ##         key = "dataType", value = "value") %>% filter(!is.na(value) | 
    ##         address %in% cellNeeds & dataType == "logical") %>% mutate(same = ifelse(data_type == 
    ##         dataType, 1, 0))
    ##     return(cells)
    ## }

``` r
culvert_tidy
```

    ## function (folder) 
    ## {
    ##     culvertFolder <- file.path(folder)
    ##     culvertFiles <- list.files(culvertFolder) %>% as.tibble()
    ##     culvertFiles <- culvertFiles %>% rename(filenames = value)
    ##     culvertFiles <- culvertFiles %>% mutate(lastChanges = as.POSIXct(map_dbl(.x = filenames, 
    ##         .f = ~file.info(paste(folder, .x, sep = "/"))$mtime), 
    ##         origin = "1970-01-01"), filePath = paste(folder, filenames, 
    ##         sep = "/"))
    ##     culvertFiles <- culvertFiles %>% mutate(tidycells = map(.x = filePath, 
    ##         .f = culvert_fetch))
    ##     return(culvertFiles)
    ## }

``` r
test1 <- culvert_tidy("spreadsheets/Ex")

glimpse(test1)
```

    ## Observations: 3
    ## Variables: 4
    ## $ filenames   <chr> "Copy of Tidal Field Data blank.xlsm", "Tidal Field Dat...
    ## $ lastChanges <dttm> 2018-08-14 11:38:50, 2018-08-14 11:38:50, 2018-09-18 1...
    ## $ filePath    <chr> "spreadsheets/Ex/Copy of Tidal Field Data blank.xlsm", ...
    ## $ tidycells   <list> [<tbl_df[1050 x 8]>, <tbl_df[1050 x 8]>, <tbl_df[1050 ...

``` r
glimpse(test1$tidycells[[1]])
```

    ## Observations: 1,050
    ## Variables: 8
    ## $ sheet               <chr> "Data Sheet - SUMMARY", "Data Sheet - SUMMARY",...
    ## $ address             <chr> "N14", "N15", "P15", "N16", "P16", "N17", "N19"...
    ## $ data_type           <chr> "error", "error", "error", "error", "error", "e...
    ## $ character_formatted <list> [NULL, NULL, NULL, NULL, NULL, NULL, NULL, NUL...
    ## $ formula             <chr> "IF(AND(Calculations!C9>=0.9,'Data Sheet - SITE...
    ## $ dataType            <chr> "error", "error", "error", "error", "error", "e...
    ## $ value               <chr> "#DIV/0!", "#DIV/0!", "#DIV/0!", "#DIV/0!", "#D...
    ## $ same                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...

From this we can start extracting the parts we want from the data sheet.
To ease that process we’ll rely on a pair of functions and a ***key***
spreadsheet for guiding the extraction of the bits we want. To follow
the process behind these next few operations we need to understand how
things are coming together here. Essentially each row in the tidy
culvert data frame contains a column with all the data from the original
tidal culvert field assessment datasheets. Given the complexity of these
datasheets and the lack of consistent orientations between data values
and data keys (or data and variable name if you will) each item of
interest needs to be mappped out individually using a key that provides
the location within that spreadsheet that that value of interest is
found (which if luck is on our side is found consistently through out
all these files…Should be…) and a name that we want to give that value.
Once the key is built (note the code will run at anytime and return only
those values that are mapped out in the key) it’s fed into a function
that will pull out the ‘raw’ data form the spreadsheet (which now is
that tidycells column) and pop it into an orderly, labelled, decoded
sheet. And to ensure that nothing is mixed up in the process we’ll add
that new decoded data frame (or spreadsheet for sake of argument) as a
new column to our already existing data frame. The result is a data
frame that contains a row for each file that is found within the
specified folder, the last time that file was altered, the path to the
file, all the raw values from that spreadsheet, and lastly the decoded
values from each culvert data sheet.  
From that we can then select the decoded values, and the filenames
column (to act as the means of IDing which values came from which files
if there’s values that need to be QA’ed etc.) and flatten it out so that
each row contains all the values of interest (which are identified in
the key) into a tidy spreadsheet for downstream processing.

``` r
decodedSheet2 <- test1 %>% mutate(decoded = map(.x = tidycells, .f = ~decodeSheet(.x, keysheet))) # this is where the sausage is made. 


decodedSheet2 %>% 
  select(filenames, decoded) %>% 
  unnest() %>% select(filenames, dataName, values) %>% 
  spread(key = dataName, value = values) -> a

glimpse(a)
```

    ## Observations: 3
    ## Variables: 115
    ## $ filenames                       <chr> "Copy of Tidal Field Data blank.xls...
    ## $ AsmtEndTime                     <chr> NA, NA, NA
    ## $ AsmtStartTime                   <chr> NA, NA, NA
    ## $ ChannelPoolWidth_comments       <chr> NA, NA, NA
    ## $ ChannelWidth_dwnStream          <chr> NA, NA, NA
    ## $ ChannelWidth_upStream           <chr> NA, NA, NA
    ## $ CrosDim_dwnA                    <chr> NA, NA, NA
    ## $ CrosDim_dwnBCB                  <chr> NA, NA, NA
    ## $ CrosDim_dwnBLT                  <chr> NA, NA, NA
    ## $ CrosDim_dwnC                    <chr> NA, NA, NA
    ## $ CrosDim_dwnD                    <chr> NA, NA, NA
    ## $ CrosDim_upA                     <chr> NA, NA, NA
    ## $ CrosDim_upBCB                   <chr> NA, NA, NA
    ## $ CrosDim_upBLT                   <chr> NA, NA, NA
    ## $ CrosDim_upC                     <chr> NA, NA, NA
    ## $ CrosDim_upD                     <chr> NA, NA, NA
    ## $ crossingID                      <chr> NA, NA, NA
    ## $ CrossingLat_fieldValue          <chr> NA, NA, NA
    ## $ CrossingLon_fieldValue          <chr> NA, NA, NA
    ## $ crossingOutlet_Atlantic_N       <chr> "FALSE", "FALSE", "FALSE"
    ## $ crossingOutlet_Atlantic_Y       <chr> "FALSE", "FALSE", "FALSE"
    ## $ crossingOutlet_Subtidal_N       <chr> "FALSE", "FALSE", "FALSE"
    ## $ crossingOutlet_Subtidal_Y       <chr> "FALSE", "FALSE", "FALSE"
    ## $ CrossingType                    <chr> "1", "1", "1"
    ## $ CrossingType_conditionComts     <chr> NA, NA, NA
    ## $ crossSection_hts_coms           <chr> "Comments:", "Comments:", "Comments:"
    ## $ dateAssessed                    <chr> NA, NA, NA
    ## $ DesktopAssessmentComplete       <chr> NA, NA, NA
    ## $ FieldAssessmentComplete         <chr> NA, NA, NA
    ## $ FullAssessmentComplete          <chr> NA, NA, NA
    ## $ GeneralAssessmentNotes          <chr> NA, NA, NA
    ## $ HeadwallCondition_dwnStream     <chr> "1", "1", "1"
    ## $ HeadwallCondition_upStream      <chr> "1", "1", "1"
    ## $ HeadwallMaterial_dwnStream      <chr> "1", "1", "1"
    ## $ HeadwallMaterial_upStream       <chr> "1", "1", "1"
    ## $ HeightOfRoadCenter              <chr> NA, NA, NA
    ## $ HighTidePerch_dwnStream         <chr> NA, NA, NA
    ## $ HighTidePerch_upStream          <chr> "N/A", "N/A", "N/A"
    ## $ HtofControlPt_D                 <chr> NA, NA, NA
    ## $ HtofControlPt_R                 <chr> NA, NA, NA
    ## $ HtofControlPt_U                 <chr> NA, NA, NA
    ## $ HtofControlPt_X                 <chr> NA, NA, NA
    ## $ InundationRisk_roadway_N        <chr> "FALSE", "FALSE", "FALSE"
    ## $ InundationRisk_roadway_N_1p     <chr> "FALSE", "FALSE", "FALSE"
    ## $ InundationRisk_roadway_Y        <chr> "FALSE", "FALSE", "FALSE"
    ## $ InundationRisk_roadway_Y_1p     <chr> "FALSE", "FALSE", "FALSE"
    ## $ InundRisk_devlp_Comts           <chr> NA, NA, NA
    ## $ InundRisk_NumInfrsuct           <chr> NA, NA, NA
    ## $ InundRisk_NumInfrsuct_1p        <chr> NA, NA, NA
    ## $ InundRisk_rdwy_Comts            <chr> NA, NA, NA
    ## $ InvasiveSppsPresent_dwnStream   <chr> "1", "1", "1"
    ## $ InvasiveSppsPresent_upStream    <chr> "1", "1", "1"
    ## $ LiDarHt_CL                      <chr> NA, NA, NA
    ## $ LowTidePerch_dwnStream          <chr> NA, NA, NA
    ## $ LowTidePerch_upStream           <chr> NA, NA, NA
    ## $ LowTideWaterElev_dwnStream      <chr> NA, NA, NA
    ## $ LowTideWaterElev_dwnStreamCode  <chr> NA, NA, NA
    ## $ LowTideWaterElev_upStream       <chr> NA, NA, NA
    ## $ LowTideWaterElev_upStreamCode   <chr> NA, NA, NA
    ## $ MarshCondition_dwnStream        <chr> "1", "1", "1"
    ## $ MarshCondition_upStream         <chr> "1", "1", "1"
    ## $ MarshMigrPot_acres              <chr> NA, NA, NA
    ## $ MarshMigrPot_evalUnit           <chr> NA, NA, NA
    ## $ MaxPoolWidth_dwnStream          <chr> NA, NA, NA
    ## $ MaxPoolWidth_upStream           <chr> NA, NA, NA
    ## $ MissingFromAssessment           <chr> NA, NA, NA
    ## $ Municipality                    <chr> NA, NA, NA
    ## $ NatCommClass_Comments           <chr> "(note invasives if present on road...
    ## $ NatCommClassification_dwnStream <chr> NA, NA, NA
    ## $ NatCommClassification_upStream  <chr> "1", "1", "1"
    ## $ NumbTidalCrossings_dwnStream    <chr> NA, NA, NA
    ## $ NWI_class_dwnStream             <chr> NA, NA, NA
    ## $ NWI_class_upStream              <chr> NA, NA, NA
    ## $ observers                       <chr> NA, NA, NA
    ## $ OtherInfrastructure_dwnStream   <chr> "1", "1", "1"
    ## $ OtherInfrastructure_upStream    <chr> "1", "1", "1"
    ## $ photoDeviceUsed                 <chr> NA, NA, NA
    ## $ photoStorageLocation            <chr> NA, NA, NA
    ## $ roadName                        <chr> NA, NA, NA
    ## $ RoadSurfaceCondition            <chr> "1", "1", "1"
    ## $ roadWidth                       <chr> NA, NA, NA
    ## $ saltMarshArea                   <chr> NA, NA, NA
    ## $ Scour_inStructure               <chr> "1", "1", "1"
    ## $ ScourSeverity_dwnStream         <chr> "1", "1", "1"
    ## $ ScourSeverity_inStructure       <chr> "1", "1", "1"
    ## $ ScourSeverity_upStream          <chr> "1", "1", "1"
    ## $ ScourStructure_dwnStream        <chr> "7", "7", "7"
    ## $ ScourStructure_upStream         <chr> "0", "0", "0"
    ## $ streamName                      <chr> NA, NA, NA
    ## $ StructureConditionOverall       <chr> "1", "1", "1"
    ## $ StructureMaterial               <chr> "1", "1", "1"
    ## $ TideChartLocation               <chr> NA, NA, NA
    ## $ TideGate                        <chr> NA, NA, NA
    ## $ TidePredictElevationHigh        <chr> NA, NA, NA
    ## $ TidePredictElevationLow         <chr> NA, NA, NA
    ## $ TidePredictTimeHigh             <chr> NA, NA, NA
    ## $ TidePredictTimeLow              <chr> NA, NA, NA
    ## $ vegMatrix_1A                    <chr> "FALSE", "FALSE", "FALSE"
    ## $ vegMatrix_1B                    <chr> NA, NA, NA
    ## $ vegMatrix_1C                    <chr> "FALSE", "FALSE", "FALSE"
    ## $ vegMatrix_2A                    <chr> NA, NA, NA
    ## $ vegMatrix_2B                    <chr> NA, NA, NA
    ## $ vegMatrix_2C                    <chr> "FALSE", "FALSE", "FALSE"
    ## $ vegMatrix_3A                    <chr> NA, NA, NA
    ## $ vegMatrix_3B                    <chr> "FALSE", "FALSE", "FALSE"
    ## $ vegMatrix_3C                    <chr> NA, NA, NA
    ## $ WatershedArea_upStream          <chr> NA, NA, NA
    ## $ WatershedLndCover_developed     <chr> NA, NA, NA
    ## $ WatershedLndCover_forested      <chr> NA, NA, NA
    ## $ WatershedLndCover_impervious    <chr> NA, NA, NA
    ## $ WatershedLndCover_wetland       <chr> NA, NA, NA
    ## $ WingwallCondition_dwnStream     <chr> "1", "1", "1"
    ## $ WingwallCondition_upStream      <chr> "1", "1", "1"
    ## $ WingwallMaterials_dwnStream     <chr> "1", "1", "1"
    ## $ WingwallMaterials_upStream      <chr> "1", "1", "1"

Approaching this problem in this way requires a few extra steps to
recreate the formula and the assessment values that are calculated in
the spreadsheet, but by tidying up the data in this way we can quickly
alter-test-review the outputs of the assessment across ALL culverts
assessed without having to individually reprogram each file.  
Below are some of my original ideas on the ups and downs of doing this
which I’ll leave in here for now.

#### Pros

  - More freedom and flexibility of up and down stream data
    management/analysis approaches, not limited to embedded excel
    formulas  
  - If any sensitivity analysis or alterations to the prioritization
    calculations are to be explored those calculations can easily be
    altered; whereas now they exist in each individual excel file.
  - At least in R, we can quickly find missing information from the
    ‘raw’ data sheet. Currently some formulas just yield more or
    less useless errors and are imported into R as ‘\#Div/O\!’

#### Cons

  - Will require a *fair amount* of work re-coding Excel formulas into R
    code (or other) \*\* as mentioned above though most of the formulas
    are simple, single cell references, many even referencing cells that
    reference cells, that..etc. In fact of the 443 cells that contain
    some sort of formula only 139 of the cells contain references to
    more than one cell. And only a small proportion reference many cells
    (i.e. a bit messier / time consuming to dig into). In addition, many
    are simple *If(CELL = 0, " ", CELL)*- essentially changing just the
    way excel displays the information (blank vs 0)

### Potential pifalls

There’s a few issues (potential) that I’ve come across.

  - cells are merged which makes it hard to determine which cell
    actually contains the value of interest. Seems as though it’s the
    upper left-most cell in the set of merged cells that gives the
    proper address.  
  - There are values that are selected using a formatted control (like a
    drop-down) which can be very easily altered.
      - accessing these values is best done through the *Data Sheet -
        SUMMARY* tab. Or perhaps just locked upon entering the data from
        the field sheets?
  - There appear to be cells with just spaces possibly? They are showing
    up as blank, but NOT empty. I believe this is caused by IF(cell = 0,
    " ", cell) formulas.
  - May have been mentioned above, dates are formatted differently it
    seems across and within spreadsheets. Maybe a setting in the
    author’s version of excel?

### TO DO’s:

  - Figure out how to properly translate the dates and times from the
    format excel is using to R

### This could be handy if there’s any cryptic values or formulas.

``` r
# Helper function to look up values in a cell of interest.
# 

get.cell.value <- function(tidysheet, cellOfInterest){
  
  val <- tidysheet %>% 
    filter(address == cellOfInterest) %>% 
    pull(formula)
  return(val)
  
}
#   
get.cell.value(cells, "AA65")
```

    ## [1] "IF(AA111=0,\"\", AA111)"

``` r
get.cell.formula <- function(tidysheet, cellOfInterest){
  
  val <- tidysheet %>% 
    filter(address == cellOfInterest) %>% 
    select(formula) %>% as.character() %>% 
    xlex()
  return(val)
  
}
#   
get.cell.formula(cells, "AA64")
```

    ## root           
    ## ¦-- IF         function
    ## °-- (          fun_open
    ##     ¦-- AA110  ref
    ##     ¦-- =      operator
    ##     ¦-- 0      number
    ##     ¦-- ,      separator
    ##     ¦-- ""     text
    ##     ¦-- ,      separator
    ##     ¦--        operator
    ##     °-- AA110  ref
    ## °-- )          fun_close
