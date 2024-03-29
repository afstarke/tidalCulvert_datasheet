#
#'
#' User Guide:
#' This code chunk helps streamline the process of making summary sheets for the freshwater site (tidal likely to use same design)
#' The premise is as follows:
#' First we generate a list of crossings (by crossing code) that needs summary sheets created. 
#' These crossings will be loaded in the summarySheetSetup.R code. 
source(here::here("summarySheetSetup.R"))
#' Initially this would be all the crossings that we want.
#' Then an HTML output is created for each crossing, and stored in an ouput folder here:
htmlOutputFolder <- "D:/culvert_project/tidalCrossings/html_outputs/"
#' From this folder a list of completed HTML outputs can be generated. Removal of an HTML file from this folder will 
#' trigger a replacement to be made on the next run of this code.
#' 


# Step #1 -
# Create HTML versions of all the crossing sheets needed.
# Render Rmd to HTML.
# 


makeDocs <- function(crossings) {
  # Pandoc errors occaisonally throwing off loop.
  # Create a log of the pdf files that have been produced and rerun above loop removing the ones already created.
  # Successes are crossings that have been converted to pdf.
  crossing_list <- crossings
   successes <-
    # list.files("../../../Box Sync/LI_Road_Stream_Tidal_Crossing_Project_Resources/Documents/SummarySheets", pattern = ".pdf") %>% str_remove(pattern = ".pdf")
    # list.files("D:/culvert_project/html_outputs", pattern = ".pdf") %>% strstr_remove(pattern = ".pdf")
    list.files("D:/culvert_project/tidalCrossings/html_outputs", pattern = ".html") %>% str_remove(pattern = ".html") # DElete bad copies of the htmls here to reproduce.
    # list.files("D:/culvert_project/pdf_outputs", pattern = ".pdf") %>% str_remove(pattern = ".pdf") # DElete bad copies of the pdfs here to control whats rerun.
  print(paste(
    length(successes),
    "successfully printed, just",
    length(crossing_list) - length(successes),
    "more to go."
  ))
  
  todos <- crossing_list[!crossing_list %in% successes]
  # Uncomment for fresh reruns.
  # purrr::walk(
  #   .x = todos,
  #   .f = ~ rmarkdown::render(
  #     "summarySheets/summarySheet_tidal.Rmd",
  #     poster_jacobs(css = "summary.css"),
  #     params = list(crossingCode = .x),
  #     output_file = paste0(.x, ".html"),
  #     output_dir = "D:/culvert_project/html_outputs",
  #     output_options = list(self_contained = FALSE)
  #   )
  # )
  # render tidal sheets.
  purrr::walk(
    .x = todos,
    .f = ~ rmarkdown::render(
      "summarySheets/summarySheet_tidal.Rmd",
      poster_jacobs(css = "summary_v2.css"),
      params = list(crossingCode = .x),
      output_file = paste0(.x, ".html"),
      output_dir = "D:/culvert_project/tidalCrossings/html_outputs/",
      output_options = list(self_contained = FALSE)
    )
  )
}

# Run makeDocs to render HTMLs to the output folder
# 

# Filter out the crossings where we don't want a summary sheet. 
# generating summary sheet for all crossings that have a valid score.
#' Tidal 
#' to start there's a lot of crossing that are missing bits that prohibit a final score from being calculated. Start with those.
#' All the tidal sites
tidalsites <- tidalPrioritization %>% filter(!is.na(Total_Benefit)) %>% pull(crossingID) 
#' Priority (top 10 sites)
# priorityCrossings <- LIculvertPrioritization %>% filter(priorityCrossing) %>% pull(crossingID)
#' Mattituck Creek project area
# mattituckCrossings <- c(108:110, 426, 427)
#' Accabonac Area
# accabonacCrossings <- c(115, 118)
#'
# both <- c(mattituckCrossings, accabonacCrossings)
## 
# All the tidal crossings
# makeDocs(tidalsites)

# makeDocs(118)

# makeDocs(priorityCrossings)
# makeDocs(c(415, 444, 35, 36, 37, 40))
makeDocs(tidalsites)

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
      "D:/culvert_project/tidalCrossings/pdf_outputs/" # Similar to above remove pdf versions that are UNWANTED. i.e. page breaks wrong etc. 
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


checkPic <- function(crossing) {
  picnumbz <-
    pics %>% filter(crossingID == crossing) %>% pull(No.pics)
  if (length(picnumbz) == 0) {
    print("There are no pics for this crossing")
  } else{
    print(glue::glue("There are {picnumbz} pictures available"))
  }
  
}

checkSurvey <- function(crossing){
          test2longitudinalProfile <-
            LIculvertAssessments %>% filter(crossingID == crossing) %>% select(longProfile) %>% unnest(cols = longProfile)
          # View(test2longitudinalProfile)
          test2crossHeight <-
            LIculvertAssessments %>% filter(crossingID == crossing) %>% select(crossSectionProfile) %>% unnest(cols = crossSectionProfile)
          # View(test2crossHeight)
          df <- list(test2longitudinalProfile, test2crossHeight)
          df #%>% purrr::walk(View)
          # df %>% purrr::map(View)
}

check <- function(crossing){
  checkPic(crossing = crossing)
  checkSurvey(crossing = crossing)
}

########################################## 
#' PDF file crosswalk table
#' write an excel file for use in attaching summary sheets to the features in ArcPro. CAUTION-- READS & WRITES TO NYSPATIAL. 
#'
tidalXwalk <- function() {
  xwalk <- tibble(paths = list.files("M:/Projects/LI/Culvert_Assessment/summarysheets/tidalSheets",
        pattern = "*.pdf",
        full.names = T
      ),
      crosCode = str_remove(paths, pattern = "M:/Projects/LI/Culvert_Assessment/summarysheets/tidalSheets/tidal_crossing_") %>% str_remove(".pdf")
    ) %>%
    mutate(paths = str_replace(paths, pattern = "M:/", replacement = "D:/gisdata/")) # create colum of paths.
  # Write to the final directory.
  xwalk %>% write_xlsx(path = "M:/Projects/LI/Culvert_Assessment/summarysheets/tidalSheets/tidalcrosswalkTable.xlsx")
}
tidalXwalk()
 
# 

