

source(here::here("summarySheetSetup.R"))

# Filter out the crossings where we don't want a summary sheet. 
# TODO: Confirm with team on HOW to do this.
# generating summary sheet for all crossings that have a valid score.
crossing_list <- fw_data %>% filter(Total_Benefit != -99) %>% pull(CrosCode)



makeDocs <- function() {
  # Pandoc errors occaisonally throwing off loop.
  # Create a log of the pdf files that have been produced and rerun above loop removing the ones already created.
  # Successes are crossings that have been converted to pdf.
  successes <-
    list.files("../../../Box Sync/LI_Road_Stream_Tidal_Crossing_Project_Resources/Documents/SummarySheets", pattern = ".pdf") %>% str_remove(pattern = ".pdf")
    # list.files("D:/culvert_project/html_outputs", pattern = ".html") %>% str_remove(pattern = ".html")
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
      "summarySheets/summarySheet_fw_postersimpler.Rmd",
      poster_jacobs(css = "summary.css"),
      params = list(crossingCode = .x),
      output_file = paste0(.x, ".html"),
      output_dir = "D:/culvert_project/html_outputs",
      output_options = list(self_contained = FALSE)
    )
  )
}

makeDocs()




# Then loop through the html files and create them as pdfs.
# OR---
# Add knit: pagedown::chrome_print to yaml - which isn't working....

# 
# printDocs <- function() {
#   # Create the list of files to convert
#   files <-
#     list.files(path = "D:/culvert_project/html_outputs/",
#                pattern = ".html",
#                full.names = F)
#   # create a list of successes to pick up in case code breaks. These are pdfs
#   successes <-
#     list.files("D:/culvert_project/html_outputs/") %>% str_remove(pattern = ".pdf")
#   filestoconvert <-
#     files %>% str_remove(pattern = ".html") # vector of file names(crossingnames)
#   
#   todos <- filestoconvert[!filestoconvert %in% successes]
#   
#   print(paste(
#     length(successes),
#     "successfully printed, just",
#     length(crossing_list) - length(successes),
#     "more to go."
#   ))
#   # for (i in files) {
#   #   chrome_print(
#   #     verbose = 0,
#   #     input = i,
#   #     timeout = 30,
#   #     output = here::here("summarySheets/pdf_outputs/"),
#   #     wait = 3,
#   #     # browser = "C:/Program Files (x86)/Google/Chrome Beta/Application/chrome.exe",
#   #     extra_args = c("--disable-gpu", "--no-sandbox", "--headless"),
#   #     options = list(margins = "none")
#   #   )
#   #
#   # }
#   
#   purrr::walk(
#     .x = todos,
#     .f = ~ chrome_print(
#       input = paste0("D:/culvert_project/html_outputs/", files),
#       wait = 3,
#       # work_dir = here::here("summarySheets/html_outputs/tmp"),
#       timeout = 30,
#       verbose = 1,
#       # browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
#       # extra_args = c("--disable-gpu", "--no-sandbox", "--headless"),
#       options = list(margins = "none"),
#       extra_args = c("--headless", "--no-sandbox", "--disable-gpu")
#     )
#   )
#   
# }
# printDocs()
# # 
# pagedown::chrome_print(input = "D:/culvert_project/html_outputs/xy4066780473404765.html",
#               wait = 10,
#              timeout = 20,
#              verbose = 2,
#              extra_args = c("--no-sandbox"),
#              browser = "C:/Program Files (x86)/Google/Chrome Beta/Application/chrome.exe",
#              # extra_args = c("--disable-gpu", "--no-sandbox", "--headless"),
#              options = list(margins = "none"))#,
#              # extra_args = c("--headless", "--no-sandbox", "--disable-gpu"))
# # # # 
# # # pagedown options: 
# chrome_print(input = "summarySheets/summarySheet_fw_postersimpler.Rmd",
#                        output = "",
#                        # format = "pdf",
#                        # timeout = 30,
#                        # verbose = 1,
#                        wait = 5,
#                        extra_args = c("--disable-gpu", "--no-sandbox", "--headless"),
#                        browser = "C:/Program Files (x86)/Google/Chrome Beta/Application/chrome.exe")
# 

# 





# Manually printing these things...
# Here's a few helpers-
# Open the files that need to be printed still.
 



allhtmls <- list.files(path = "D:/culvert_project/html_outputs/",
             pattern = ".html",
             full.names = F) %>% str_remove(".html")
# create a list of successes to pick up in case code breaks. These are pdfs created from the htmls
pdfcheck <- function()
{ # returns a pep talk and the list of crossings that are in the pdf output folder.
  pdfs_completed <-
    list.files(
      "../../../Box Sync/LI_Road_Stream_Tidal_Crossing_Project_Resources/Documents/SummarySheets"
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
  allhtmls <- list.files(path = "D:/culvert_project/html_outputs/",
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]
  print(paste0(
    "Hang in there big guy, only ",
    length(filestoconvert), " to go!")
  )}
statuscheck()

get_to_work <- function(){
  allhtmls <- list.files(path = "D:/culvert_project/html_outputs/",
                         pattern = ".html",
                         full.names = F) %>% str_remove(".html")
  pdfs_completed <- pdfcheck()
  # statuscheck()
  filestoconvert <- allhtmls[!allhtmls %in% pdfs_completed]
 filestoconvert %>% paste0("file://D:/culvert_project/html_outputs/", ., ".html") %>% 
    # sample(20, replace = F) %>% 
   walk(.x = ., ~browseURL(.x))
  
}


get_to_work()


# PDF file crosswalk table
xwalk <- tibble(paths = list.files("M:/Projects/LI/Culvert_Assessment/summarysheets", pattern = "*.pdf", full.names = T), 
                crosCode = str_remove(paths, pattern = "M:/Projects/LI/Culvert_Assessment/summarysheets/") %>% str_remove(".pdf")) %>% 
  mutate(paths = str_replace(paths, pattern = "M:/", replacement = "D:/gisdata/"))

xwalk %>% write_xlsx(path = "M:/Projects/LI/Culvert_Assessment/summarysheets/crosswalkTable.xlsx")

