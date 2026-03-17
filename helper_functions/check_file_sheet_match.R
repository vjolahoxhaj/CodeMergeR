
check_file_sheet_match <- function(folder_path) {
  # Get folder name
  folder_name <- basename(folder_path)
  
  # List all .xlsx and .xls files in the folder
  xlsx_files <- list.files(folder_path, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  
  # If there are no Excel files, return Invalid
  if (length(xlsx_files) == 0) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "No Excel file found"))
  }
  
  # Iterate over each Excel file in the folder
  results <- lapply(xlsx_files, function(file) {
    file_name <- tools::file_path_sans_ext(basename(file))  # Get file name without extension
    
    # Try to read sheet names, handle errors
    sheet_names <- tryCatch(
      excel_sheets(file),
      error = function(e) return(NULL)
    )
    
    if (is.null(sheet_names)) {
      return(data.table(Folder = folder_name, Result = "Invalid", Comment = "Error reading Excel file"))
    }
    
    # Check if the file name matches any sheet name
    if (file_name %in% sheet_names) {
      return(data.table(Folder = folder_name, Result = "Valid", Comment = "File name matches a sheet name"))
    } else {
      return(data.table(Folder = folder_name, Result = "Invalid", Comment = "File name does not match any sheet name"))
    }
  })
  
  # Combine results into a single data.table
  return(rbindlist(results))
}
