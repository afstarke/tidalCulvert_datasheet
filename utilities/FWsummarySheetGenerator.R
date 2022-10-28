#
#' Freshwater Summary Sheet auto generator.
#' 
#' User Guide:
#' This code chunk helps streamline the process of making summary sheets for the freshwater site (tidal likely to use same design)
#' The premise is as follows:
#' First we generate a list of crossings (by crossing code) that needs summary sheets created. 
#' These crossings will be loaded in the summarySheetSetup.R code. 
source(here::here("summarySheetSetup.R"))
source(here::here("utilities/photoMatchTable.R"))
#' Initially this would be all the crossings that we want.
#' Then an HTML output is created for each crossing, and stored in an ouput folder here:
htmlOutputFolder <- "D:/culvert_project/html_outputs/"
#' From this folder a list of completed HTML outputs can be generated. Removal of an HTML file from this folder will 
#' trigger a replacement to be made on the next run of this code.
#' 

#' Freshwater
# fw_data <- read_rds(here::here("data/fwaterPrioritizations_RERUNS.rds"))
crossing_list <- fw_data %>% filter(Total_Benefit != -99) %>%
  pull(CrosCode)



# A bit of house cleaning was needed. There were a few  crossings
# that initially were going to be included in the freshwater set but were dropped 
# after sheets were made up. Those were tracked down and removed. Additionally 
# there was a batch of about 40 crossings that had missing data that precluded 
# a full score from being calculated BUT it was decided that they would be included 
# so that partners could add to the data as they saw fit.
# 
# freshSheets contains a list of all the crossings that SHOULD have sheets.
freshSheets <- fw_data %>% mutate(
  needsheet = if_else(
    {condition = Eval == "no score - missing data" &
      Total_Benefit == -99 &
      Crossing_Condition %in% c("New", "OK", "Poor")}|{Total_Benefit != -99},
    true = 1,
    false = 2
  )) %>% filter(needsheet == 1) %>% pull(CrosCode) %>% droplevels() %>% as.character()

#' 
#' ###################################
#'  26Apr2021 -- checked the two lists to one another and they matched 
#'  so we only have the sheets that we need.
#' 
#' #' Freshwater pdf list from initial runs in 2020
#' #' Use this as a baseline for rerunning the sheets missing photos.
# freshPDF <-
#   list.files(
#     "D:/culvert_project/pdf_outputs",
#     # "M:/Projects/LI/Culvert_Assessment/summarysheets",
#     pattern = "*.pdf",
#     full.names = T
#   ) %>%
#   str_remove(pattern = "D:/culvert_project/pdf_outputs") %>% 
#                # "M:/Projects/LI/Culvert_Assessment/summarysheets/" ) %>%
#   str_remove(".pdf")
#'   
#'   ##################################


# Step #1 -
# Create HTML versions of all the crossing sheets needed.
# Render Rmd to HTML.
# 
makeFreshwaterDocs <- function(crossings) {
  # Pandoc errors occaisonally throwing off loop.
  # Create a log of the pdf files that have been produced and rerun above loop removing the ones already created.
  # Successes are crossings that have been converted to pdf.
  crossing_list <- crossings
  successes <-
    # list.files("../../../Box Sync/LI_Road_Stream_Tidal_Crossing_Project_Resources/Documents/SummarySheets", pattern = ".pdf") %>% str_remove(pattern = ".pdf")
    # list.files("D:/culvert_project/html_outputs", pattern = ".pdf") %>% strstr_remove(pattern = ".pdf")
    list.files("D:/culvert_project/html_outputs", pattern = ".html") %>% str_remove(pattern = ".html") # DElete bad copies of the htmls here to reproduce.
  # list.files("D:/culvert_project/pdf_outputs", pattern = ".pdf") %>% str_remove(pattern = ".pdf") # DElete bad copies of the pdfs here to control whats rerun.
  print(paste(
    length(successes),
    "successfully printed, just",
    length(crossing_list) - length(successes),
    "more to go."
  ))
  
  todos <- crossing_list[!crossing_list %in% successes]
  
  purrr::walk(
    .x = todos,
    .f = ~ rmarkdown::render(
      "summarySheets/summarySheet_fw.Rmd",
      poster_jacobs(css = "summary.css"),
      params = list(crossingCode = .x),
      output_file = paste0(.x, ".html"),
      output_dir = "D:/culvert_project/html_outputs",
      output_options = list(self_contained = FALSE)
    )
  )


}

# makeFreshwaterDocs(freshSheets) ## <---- runs the full set of crossings.
# Run makeDocs to render HTMLs to the output folder
# 
reruns_3May2021 <- c("xy4089996872591505", "xy4092557372650041")
makeFreshwaterDocs(reruns_3May2021)
# As of May 2020 automating chrome_print was throwing errors to work around is to open and print to pdf individually.

# Manually printing these things...
# Here's a few helpers-
# Open the files that need to be printed still.



# list the HTMLs that are in the working output directory. These are considered done- 
allhtmls <- list.files(path = htmlOutputFolder, # Remove any files from this directory that are UNWANTED.
                       pattern = ".html",
                       full.names = F) %>% str_remove(".html")
# create a list of successes to pick up in case code breaks. These are pdfs created from the htmls that are listed from above. 
pdfcheck <- function()
{ # returns a pep talk and the list of crossings that are in the pdf output folder.
  pdfs_completed <-
    list.files(
      "D:/culvert_project/pdf_outputs/" # Similar to above remove pdf versions that are UNWANTED. i.e. page breaks wrong etc. 
    ) %>%
    str_remove(pattern = ".pdf")
  print(paste0(
    "Hang in there champ, you've got ",
    length(pdfs_completed), " done already"))
  return(pdfs_completed)
}

pdfcheck()

# filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]

statuscheck <- function()
{
  pdfs_completed <- pdfcheck()
  allhtmls <- list.files(path = htmlOutputFolder,
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]
  print(paste0(
    "Hang in there big guy, only ",
    length(filestoconvert), " to go!")
  )}
statuscheck()

# # # Automating the opening of remaining documents.
# get_to_work <- function(){
#   allhtmls <- list.files(path = htmlOutputFolder,
#                          pattern = ".html",
#                          full.names = F) %>% str_remove(".html")
#   pdfs_completed <- pdfcheck() # check which pdf's have been created
#   # statuscheck()
#   filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed] # Create a list of crossings that need to be converted from HTML to PDF
#  filestoconvert %>% paste0("file://", htmlOutputFolder, ., ".html") %>%
#     # sample(20, replace = F) %>%
#    walk(.x = ., ~chrome_print(.x)) # Walk through and print each one.
# 
# }
# 
# 
# get_to_work()


## UPDATE Nov 2020
# try at chrome_print interations

# Automating the opening of remaining documents.
let_me_work <- function(){
  allhtmls <- list.files(path = htmlOutputFolder,
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  pdfs_completed <- pdfcheck() # check which pdf's have been created
  # statuscheck()
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed] # Create a list of crossings that need to be converted from HTML to PDF
  
  files <- filestoconvert %>% paste0("file://", htmlOutputFolder, ., ".html") 
  if(length(files) < 20){
    files %>% 
      walk(.x = ., ~browseURL(.x))
  }else{
    files %>% 
      sample(20, replace = F) %>%
      walk(.x = ., ~browseURL(.x))
  }
  # Walk through each file and open in browser
  
}


let_me_work()

# 
# checkPic <- function(crossing) {
#   picnumbz <-
#     pics %>% filter(crossingID == crossing) %>% pull(No.pics)
#   if (length(picnumbz) == 0) {
#     print("There are no pics for this crossing")
#   } else{
#     print(glue::glue("There are {picnumbz} pictures available"))
#   }
#   
# }
# 


#
# 
# # PDF file crosswalk table
xwalk <- tibble(paths = list.files("M:/Projects/LI/Culvert_Assessment/summarysheets", pattern = "*.pdf", full.names = T),
                crosCode = str_remove(paths, pattern = "M:/Projects/LI/Culvert_Assessment/summarysheets/") %>% str_remove(".pdf")) %>%
  mutate(paths = str_replace(paths, pattern = "M:/", replacement = "D:/gisdata/"))

xwalk %>% write_xlsx(path = "M:/Projects/LI/Culvert_Assessment/summarysheets/crosswalkTable.xlsx")

# 
# individualReruns %>% paste0("file://D:/culvert_project/html_outputs/", ., ".html") %>%
#   # sample(20, replace = F) %>%
#   walk(.x = ., ~browseURL(.x))

 

