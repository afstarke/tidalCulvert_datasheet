Tidal Spreadsheet fun
================

Take 2:
-------

After some digging into the task of extracting data from these spreadsheets, and the prospects of additional analyses (prioritization sensitivity, perhaps) as well as easing the management of field and desktop assessment needs, a new(ish) approach is laid out below. In general a reboot of the teams initial proposal of using a sort of look up key to identify the locations of important data from the spreadsheets.

To begin with, this process relies heavily on the tidyxl package, as well as the tidyverse packages more broadly.

``` r
library(readr)
library(readxl)
library(tidyxl)
library(unpivotr)
library(tidyverse)

keysheet <- read_excel("spreadsheets/key.xlsx")

source("functions/culvert_tidy.R")
source("functions/culvert_extract.R")    
```

Using tidyxl's xlsx\_cells() function we pull in all the cells from a given excel file. The cells arrive in a tidy data fashion where each row holds the data value, some formating information, data type, and the location in which it lives (the sheet and cell, specifically).
Below is a snippet of a single sheet read into R using tidxl::xlsx\_cells()

``` r
# TODO Start with basic read in of spreadsheet and extracting the proper cells.
sheet <- "spreadsheets/Copy of Culvert37.xlsm" 
# Tidy up the sheets. 
cells <- xlsx_cells(sheet) %>% 
  filter(is_blank == FALSE, sheet != "Data Sheet - BLANK") 
  
cells
```

    ## # A tibble: 1,162 x 21
    ##    sheet      address   row   col is_blank data_type error logical numeric
    ##    <chr>      <chr>   <int> <int> <lgl>    <chr>     <chr> <lgl>     <dbl>
    ##  1 Data Shee~ A1          1     1 FALSE    character <NA>  NA          NA 
    ##  2 Data Shee~ A5          5     1 FALSE    character <NA>  NA          NA 
    ##  3 Data Shee~ G7          7     7 FALSE    character <NA>  NA          NA 
    ##  4 Data Shee~ L7          7    12 FALSE    numeric   <NA>  NA          37.
    ##  5 Data Shee~ A9          9     1 FALSE    character <NA>  NA          NA 
    ##  6 Data Shee~ G9          9     7 FALSE    character <NA>  NA          NA 
    ##  7 Data Shee~ V9          9    22 FALSE    character <NA>  NA          NA 
    ##  8 Data Shee~ Z9          9    26 FALSE    date      <NA>  NA          NA 
    ##  9 Data Shee~ V10        10    22 FALSE    character <NA>  NA          NA 
    ## 10 Data Shee~ Z10        10    26 FALSE    date      <NA>  NA          NA 
    ## # ... with 1,152 more rows, and 12 more variables: date <dttm>,
    ## #   character <chr>, character_formatted <list>, formula <chr>,
    ## #   is_array <lgl>, formula_ref <chr>, formula_group <int>, comment <chr>,
    ## #   height <dbl>, width <dbl>, style_format <chr>, local_format_id <int>

The next step is to scale this up to capture all the datasheets on hand. We do this by creating two functions that work together. The first, **culvert\_fetch()**, fetches the tidyxl cells when given a file path name. The second function, **culvert\_tidy**, produces a data frame containing all the files within the supplied folder, a few helpful bits of info and a column containing the tidyxl cells from the output of **culvert\_fetch()** for each data sheet.

``` r
culvert_fetch
```

    ## function (filepath) 
    ## {
    ##     cells <- xlsx_cells(filepath) %>% filter(sheet != "Data Sheet - BLANK") %>% 
    ##         select(sheet, address, data_type:character) %>% gather(error:character, 
    ##         key = "dataType", value = "value") %>% filter(!is.na(value)) %>% 
    ##         mutate(same = ifelse(data_type == dataType, 1, 0))
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
    ## $ filenames   <chr> "Copy of Tidal Field Data blank.xlsm", "Tidal Fiel...
    ## $ lastChanges <dttm> 2018-08-14 11:38:50, 2018-08-14 11:38:50, 2018-09...
    ## $ filePath    <chr> "spreadsheets/Ex/Copy of Tidal Field Data blank.xl...
    ## $ tidycells   <list> [<# A tibble: 1,045 x 6,    sheet                ...

``` r
glimpse(test1$tidycells[[1]])
```

    ## Observations: 1,045
    ## Variables: 6
    ## $ sheet     <chr> "Data Sheet - SUMMARY", "Data Sheet - SUMMARY", "Dat...
    ## $ address   <chr> "N14", "N15", "P15", "N16", "P16", "N17", "N19", "N3...
    ## $ data_type <chr> "error", "error", "error", "error", "error", "error"...
    ## $ dataType  <chr> "error", "error", "error", "error", "error", "error"...
    ## $ value     <chr> "#DIV/0!", "#DIV/0!", "#DIV/0!", "#DIV/0!", "#DIV/0!...
    ## $ same      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...

From this we can start extracting the parts we want from the data sheet. To ease that process we'll rely on a pair of functions and a ***key*** spreadsheet for guiding the extraction of the bits we want. To follow the process behind these next few operations we need to understand how things are coming together here. Essentially each row in the tidy culvert data frame contains a column with all the data from the original tidal culvert field assessment datasheets. Given the complexity of these datasheets and the lack of consistent orientations between data values and data keys (or data and variable name if you will) each item of interest needs to be mappped out individually using a key that provides the location within that spreadsheet that that value of interest is found (which if luck is on our side is found consistently through out all these files...Should be...) and a name that we want to give that value. Once the key is built (note the code will run at anytime and return only those values that are mapped out in the key) it's fed into a function that will pull out the 'raw' data form the spreadsheet (which now is that tidycells column) and pop it into an orderly, labelled, decoded sheet. And to ensure that nothing is mixed up in the process we'll add that new decoded data frame (or spreadsheet for sake of argument) as a new column to our already existing data frame. The result is a data frame that contains a row for each file that is found within the specified folder, the last time that file was altered, the path to the file, all the raw values from that spreadsheet, and lastly the decoded values from each culvert data sheet.
From that we can then select the decoded values, and the filenames column (to act as the means of IDing which values came from which files if there's values that need to be QA'ed etc.) and flatten it out so that each row contains all the values of interest (which are identified in the key) into a tidy spreadsheet for downstream processing.

``` r
decodedSheet2 <- test1 %>% mutate(decoded = map(.x = tidycells, .f = ~decodeSheet(.x))) # this is where the sausage is made. 


decodedSheet2 %>% 
  select(filenames, decoded) %>% 
  unnest() %>% select(filenames, dataName, values) %>% 
  spread(key = dataName, value = values) -> a

glimpse(a)
```

    ## Observations: 3
    ## Variables: 26
    ## $ filenames             <chr> "Copy of Tidal Field Data blank.xlsm", "...
    ## $ CrosDim_dwnA          <chr> NA, NA, NA
    ## $ CrosDim_dwnBCB        <chr> NA, NA, NA
    ## $ CrosDim_dwnBLT        <chr> NA, NA, NA
    ## $ CrosDim_dwnC          <chr> NA, NA, NA
    ## $ CrosDim_dwnD          <chr> NA, NA, NA
    ## $ CrosDim_upA           <chr> NA, NA, NA
    ## $ CrosDim_upBCB         <chr> NA, NA, NA
    ## $ CrosDim_upBLT         <chr> NA, NA, NA
    ## $ CrosDim_upC           <chr> NA, NA, NA
    ## $ CrosDim_upD           <chr> NA, NA, NA
    ## $ crossingID            <chr> NA, NA, NA
    ## $ CrossingType          <chr> "1", "1", "1"
    ## $ dateAssessed          <chr> NA, NA, NA
    ## $ dwnstreamChannelwidth <chr> NA, NA, NA
    ## $ dwnstreammaxPoolwidth <chr> NA, NA, NA
    ## $ EndTime               <chr> NA, NA, NA
    ## $ headwallMat_up        <chr> "1", "1", "1"
    ## $ LiDarHt_CL            <chr> NA, NA, NA
    ## $ observers             <chr> NA, NA, NA
    ## $ roadName              <chr> NA, NA, NA
    ## $ StartTime             <chr> NA, NA, NA
    ## $ streamName            <chr> NA, NA, NA
    ## $ StructureMaterial     <chr> "1", "1", "1"
    ## $ upstreamChannelwidth  <chr> NA, NA, NA
    ## $ upstreammaxPoolwidth  <chr> NA, NA, NA

Approaching this problem in this way requires a few extra steps to recreate the formula and the assessment values that are calculated in the spreadsheet, but by tidying up the data in this way we can quickly alter-test-review the outputs of the assessment across ALL culverts assessed without having to individually reprogram each file.
Below are some of my original ideas on the ups and downs of doing this which I'll leave in here for now.

#### Pros

-   More freedom and flexibility of up and down stream data management/analysis approaches, not limited to embedded excel formulas
-   If any sensitivity analysis or alterations to the prioritization calculations are to be explored those calculations can easily be altered; whereas now they exist in each individual excel file.
-   At least in R, we can quickly find missing information from the 'raw' data sheet. Currently some formulas just yield more or less useless errors and are imported into R as '\#Div/O!'

#### Cons

-   Will require a *fair amount* of work re-coding Excel formulas into R code (or other) \*\* as mentioned above though most of the formulas are simple, single cell references, many even referencing cells that reference cells, that..etc. In fact of the 443 cells that contain some sort of formula only 139 of the cells contain references to more than one cell. And only a small proportion reference many cells (i.e. a bit messier / time consuming to dig into). In addition, many are simple *If(CELL = 0, " ", CELL)*- essentially changing just the way excel displays the information (blank vs 0)

### Potential pifalls

There's a few issues (potential) that I've come across.

-   cells are merged which makes it hard to determine which cell actually contains the value of interest.
-   There are values that are selected using a formatted control (like a drop-down) which can be very easily altered.
-   accessing these values is best done through the *Data Sheet - SUMMARY* tab. Or perhaps just locked upon entering the data from the field sheets?
-   There appear to be cells with just spaces possibly? They are showing up as blank, but NOT empty. I believe this is caused by IF(cell = 0, " ", cell) formulas.

### This could be handy if there's any cryptic values or formulas.

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
