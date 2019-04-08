# ONE OFF TOOL TO ADD CELLS TO CULVERT WORKBOOKS



newCulvertDataFolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/TidalAssessmentWorkbooks_writeTO"
existingDatafolder <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/DATASHEETBACKUPS/Culvert Excel Sheets"
existingBlanks <- "../../../Box Sync/Culvert Assessment/Tidal Assessments/BLANK_TIDAL_DATASHEETS"


existingfiles <- list.files(existingDatafolder, full.names = FALSE) # list of files of datasheets. Use the short name here for renaming. Paste prefix for readins.
existingfileBlank <- list.files(existingBlanks, full.names = FALSE)

# Run all the files ----
# For now I’ll add the following:
# MARKED- noted in Field Data_TRACKER sheet where the addition will be made.
# DONE - Added to recreated workbooks and key sheet.
# DONE: Assessment Complete: Y/N  Data Sheet -SITE Pg. 1 around cell AG6
# DONE: Field Assessment Complete: Y/N  Data Sheet – SITE pg 5 around cell A205 
# DONE: Desktop Assessment Complete: Y/N  Data Sheet -SITE on page 6 around AG227. 
# DONE: Lat/Lon  Data Sheet -SITE page 1 cells G14:H25
# Photo Storage Device  Data Sheet – SITE A41
# Insert cells (on page 2) to indicate whether it is a tide gate and if so: type, condition, material, other comments


# for each of the file names listed above...
# TODO: Get teams input on what other variables to add. Lat/Lon, Ecological indicators, etc.
# FIX: Move the cells that are giving me grief to a new cell that is not NULL from from the workbook.
for(wbfile in existingfileBlank){

    readPath <- paste(existingBlanks, wbfile, sep = "/") # paste full path to file name
    writePath <- paste(newCulvertDataFolder, wbfile, sep = "/") # Create full path name for writing file out.
      # browser()
      tmp <- xlsx::loadWorkbook(readPath) # read in file
      cs <- xlsx::CellStyle(tmp) + # read in cells from file.
        Font(tmp, heightInPoints=10, isBold=TRUE, isItalic=TRUE, # Set formatting for cells that are added.
             name="Courier New") +
        Alignment(h="ALIGN_RIGHT", wrapText = FALSE)
      csL <- xlsx::CellStyle(tmp) + # read in cells from file.
        Font(tmp, heightInPoints=10, isBold=FALSE, isItalic=TRUE, # Set formatting for cells that are added.
             name="Courier New") +
        Alignment(h="ALIGN_LEFT", wrapText = FALSE)
      tmpsheet <- xlsx::getSheets(tmp)[[2]] # Read sheets of which we're only interested in the SITE sheet (sheet 2)
      tmprows <- xlsx::getRows(sheet = tmpsheet, rowIndex = 1:278)
      
      tmpcells <- xlsx::getCells(row = tmprows, colIndex = 1:34)
      
    # Get blank cell (tmpcells$`225.26`) as object and assign it to NULL cells that are giving me errors. 
      # blankSpace <- tmpcells$`225.26`
    # browser() # used for error tracking - Something is throwing out errors . Likely the address for the setCellValue call.
    # Assessment Complete ----
      xlsx::setCellValue(cell = tmpcells[["6.33"]], value = "Full Assessment Complete:") 
      xlsx::setCellStyle(cell = tmpcells[["6.33"]], cellStyle = cs) # Set the Cell style for the value just insterted.
      xlsx::setCellValue(cell = tmpcells[["6.34"]], value = "N")
      xlsx::setCellStyle(cell = tmpcells[["6.34"]], cellStyle = cs)
    # Field Assess Complete ----
    # WORKED!
      xlsx::setCellValue(cell = tmpcells[["206.33"]], value = "Field Assessment Complete:") 
      xlsx::setCellStyle(cell = tmpcells[["206.33"]], cellStyle = cs) # Set the Cell style for the value just insterted.
      xlsx::setCellValue(cell = tmpcells[["206.34"]], value = "N")
      xlsx::setCellStyle(cell = tmpcells[["206.34"]], cellStyle = cs)
    # What's missing?
      xlsx::setCellValue(cell = tmpcells[["16.21"]], value = "Missing from assessment:")
      xlsx::setCellStyle(cell = tmpcells[["16.21"]], cellStyle = cs) # Set the Cell style for the value just insterted.
      xlsx::setCellValue(cell = tmpcells[["16.22"]], value = "Details on why assessment is not complete.")
      xlsx::setCellStyle(cell = tmpcells[["16.22"]], cellStyle = csL)
    # Photo ---- Did not add data values for these.
    # WORKED!
      xlsx::setCellValue(cell = tmpcells[["41.8"]], value = "Stored Photos (path):") 
      xlsx::setCellStyle(cell = tmpcells[["41.8"]], cellStyle = cs) # Set the Cell style for the value just insterted.  
    # Tide Gate ----
    # WORKED! BUT ask Nicole if we should instead append it to the dropdown in the Crossing Type or Crossing Material
      xlsx::setCellValue(cell = tmpcells[["57.11"]], value = "Tide Gate:") 
      xlsx::setCellStyle(cell = tmpcells[["57.11"]], cellStyle = cs) # Set the Cell style for the value just insterted.
      xlsx::setCellValue(cell = tmpcells[["57.15"]], value = "None")  # Can't seem to create a drop down which would be better. Insert NONE as default for now...
      xlsx::setCellStyle(cell = tmpcells[["57.15"]], cellStyle = cs) # Set the Cell style for the value just insterted.
    # ERROR: Commented out sections are throwing errors. Might need to createCell first. Read up on this
    # Lat ---- Did not add data values for these.
    # setCellValue(cell = tmpcells[["14.6"]], value = "Latitude:")
    # setCellStyle(cell = tmpcells[["14.6"]], cellStyle = cs) # Set the Cell style for the value just insterted.
    #     # tmpcells$`226.33` <- tmpcells[["6.33"]]
    # tmpcells$`226.34` <- tmpcells[["6.34"]]
      xlsx::setCellValue(cell = tmpcells[["224.33"]], value = "Desktop Assessment Complete:")
      xlsx::setCellStyle(cell = tmpcells[["224.33"]], cellStyle = cs) # Set the Cell style for the value just insterted.
      xlsx::setCellValue(cell = tmpcells[["224.34"]], value = "N")
      xlsx::setCellStyle(cell = tmpcells[["224.34"]], cellStyle = cs)
    # setRowHeight(rows = tmprows[[225]], multiplier = 2)
    # # Lon ---- Did not add data values for these.
    # setCellValue(cell = tmpcells[["15.6"]], value = "Longitude:")
    # setCellStyle(cell = tmpcells[["15.6"]], cellStyle = cs) # Set the Cell style for the value just insterted.
    # # Desktop Assess Complete ---- Add cell locations and names below...


      xlsx::saveWorkbook(tmp, writePath)
    rm(tmp, tmpcells, tmprows, tmpsheet)
  }

# 
# # USING XLCONNECT
# library(XLConnect)
# # for each of the file names listed above...
# # TODO: Get teams input on what other variables to add. Lat/Lon, Ecological indicators, etc.
# # FIX: Move the cells that are giving me grief to a new cell that is not NULL from from the workbook.
# for(wbfile in existingfiles[3]){
#   
#   readPath <- paste(existingDatafolder, wbfile, sep = "/") # paste full path to file name
#   writePath <- paste(newCulvertDataFolder, wbfile, sep = "/") # Create full path name for writing file out.
#   # browser()
#   tmp <- loadWorkbook(readPath) # read in file
#   # cs <- CellStyle(tmp) + # read in cells from file.
#   #   Font(tmp, heightInPoints=10, isBold=TRUE, isItalic=TRUE, # Set formatting for cells that are added.
#   #        name="Courier New") +
#   #   Alignment(h="ALIGN_RIGHT", wrapText = FALSE)
#   tmpsheet <- readWorksheet(tmp, sheet = 2, startRow = 1, startCol = 1, endRow = 34, endCol = 278) # Read sheets of which we're only interested in the SITE sheet (sheet 2)
#  
#   
#   # Get blank cell (tmpcells$`225.26`) as object and assign it to NULL cells that are giving me errors. 
#   # blankSpace <- tmpcells$`225.26`
#   # browser() # used for error tracking - Something is throwing out errors . Likely the address for the setCellValue call.
#   # Assessment Complete ----
#   setCellValue(cell = tmpcells[["6.33"]], value = "Full Assessment Complete:") 
#   setCellStyle(cell = tmpcells[["6.33"]], cellStyle = cs) # Set the Cell style for the value just insterted.
#   setCellValue(cell = tmpcells[["6.34"]], value = "N")
#   setCellStyle(cell = tmpcells[["6.34"]], cellStyle = cs)
#   # Field Assess Complete ----
#   # WORKED!
#   setCellValue(cell = tmpcells[["205.33"]], value = "Field Assessment Complete:") 
#   setCellStyle(cell = tmpcells[["205.33"]], cellStyle = cs) # Set the Cell style for the value just insterted.
#   setCellValue(cell = tmpcells[["205.34"]], value = "N")
#   setCellStyle(cell = tmpcells[["205.34"]], cellStyle = cs)
#   # Photo ---- Did not add data values for these.
#   # WORKED!
#   setCellValue(cell = tmpcells[["41.8"]], value = "Stored Photos (path):") 
#   setCellStyle(cell = tmpcells[["41.8"]], cellStyle = cs) # Set the Cell style for the value just insterted.  
#   # Tide Gate ----
#   # WORKED! BUT ask Nicole if we should instead append it to the dropdown in the Crossing Type or Crossing Material
#   setCellValue(cell = tmpcells[["57.11"]], value = "Tide Gate:") 
#   setCellStyle(cell = tmpcells[["57.11"]], cellStyle = cs) # Set the Cell style for the value just insterted.
#   setCellValue(cell = tmpcells[["57.15"]], value = "None")  # Can't seem to create a drop down which would be better. Insert NONE as default for now...
#   setCellStyle(cell = tmpcells[["57.15"]], cellStyle = cs) # Set the Cell style for the value just insterted.
#   # ERROR: Commented out sections are throwing errors. Might need to createCell first. Read up on this
#   # Lat ---- Did not add data values for these.
#   # setCellValue(cell = tmpcells[["14.6"]], value = "Latitude:")
#   # setCellStyle(cell = tmpcells[["14.6"]], cellStyle = cs) # Set the Cell style for the value just insterted.
#   #     # tmpcells$`226.33` <- tmpcells[["6.33"]]
#   # tmpcells$`226.34` <- tmpcells[["6.34"]]
#   setCellValue(cell = tmpcells[["224.33"]], value = "Desktop Assessment Complete:")
#   setCellStyle(cell = tmpcells[["224.33"]], cellStyle = cs) # Set the Cell style for the value just insterted.
#   setCellValue(cell = tmpcells[["224.34"]], value = "N")
#   setCellStyle(cell = tmpcells[["224.34"]], cellStyle = cs)
#   setRowHeight(rows = tmprows[[225]], multiplier = 2)
#   # # Lon ---- Did not add data values for these.
#   # setCellValue(cell = tmpcells[["15.6"]], value = "Longitude:")
#   # setCellStyle(cell = tmpcells[["15.6"]], cellStyle = cs) # Set the Cell style for the value just insterted.
#   # # Desktop Assess Complete ---- Add cell locations and names below...
#   
#   
#   saveWorkbook(tmp, writePath)
#   rm(tmp, tmpcells, tmprows, tmpsheet)
# }
# 
