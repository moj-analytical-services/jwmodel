# Produce XLSX report using results in jwmodel object.

source("R/outputs/utils.R")

report_xlsx <- function (obj) {
  UseMethod("report_xlsx")
}

report_xlsx.default <- function(obj) {
  cat("'report_xlsx' function can only be used on jwmodel objects")
}

report_xlsx.jwmodel <- function(obj) {

  # Initialise  XLSX file.
  wb <- openxlsx::createWorkbook()
  
  # Set global format for numbers. 
  options(openxlsx.numFmt = "#,##0")
  
  # Format for sheet titles.
  sheetheader <- createStyle(fontName = "Arial",
                             fontSize = 13,
                             halign = "left",
                             valign = "center",
                             textDecoration = "bold",
                             wrapText = FALSE)
  
  # Format for pivot headings.
  cellheader <- createStyle(fontName = "Arial",
                            fontSize = 12,
                            border = c("top", "bottom", "left", "right"),
                            borderStyle = "thin",
                            fgFill = "#EAEAEA",
                            halign = "center",
                            valign = "center",
                            textDecoration = "bold",
                            wrapText = TRUE)
  
  # Names of outputs.
  outputs <- names(obj$outputs$pivots)
  
  # Walk through each output and write to file.
  for (i in seq_along(outputs)) {
    
    # Create a sheet.
    sheet <- gsub(pattern = "_", 
                  replacement = " ",
                  outputs[[i]])
    openxlsx::addWorksheet(wb = wb, 
                           sheetName = sheet)
    
    # Get pivot.
    pivot <- obj$outputs$pivots[[i]]
    
    # Get graph for pivot.
    graph <- obj$outputs$graphs[[i]]
    
    # Identify columns (in pviot table) which contain a number.
    numeric_cols <- colnames(pivot)[grepl("202|Current", colnames(pivot))]
    
    # Convert numbers to numeric.
    pivot <- pivot %>% dplyr::mutate_at(numeric_cols,
                                        remove_comma)
    
    
    # Add title to worksheet.
    openxlsx::writeData(wb = wb,
                        sheet = sheet,
                        x = paste("S&D model results", strftime(Sys.Date(), format = "%d-%m-%Y", sep = " ")),
                        startRow = 1,
                        startCol = 1)
    
    # Format sheet title.
    openxlsx::addStyle(wb = wb, 
                       sheet = sheet, 
                       style = sheetheader, 
                       rows = 1, 
                       cols = 1)
    
    # Add pivot to worksheet.
    openxlsx::writeData(wb = wb,
                        sheet = sheet,
                        x = pivot,
                        startRow = 3,
                        startCol = 1,
                        borders = "all",
                        borderStyle = "thin",
                        headerStyle = cellheader)
    
    # Add graph to the right of the pivot.
    print(graph)
    openxlsx::insertPlot(wb = wb,
                         sheet = sheet,
                         width = 8,
                         height = 7,
                         startRow = 1,
                         startCol = ncol(pivot) + 2,
                         fileType ="png",
                         units = "in")
    
  }

  # Save file.
  report <- openxlsx::saveWorkbook(wb, 
                         file = paste("data/S&D results", strftime(Sys.Date(), format = "%d-%m-%Y"), ".xlsx", sep = ""), 
                         overwrite = TRUE)
                         
  return(paste("S&D model results wirrten to  file: data/S&D results ", strftime(Sys.Date(), format = "%d-%m-%Y"), ".xlsx", sep = ""))
  
}