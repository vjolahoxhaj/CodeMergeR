
check_excel_mandatory_columns <- function(folder_path, cols) {
  # Ensure the folder exists
  if (!dir.exists(folder_path)) {
    return(data.table(Folder = basename(folder_path), Result = "Invalid", Comment = "Folder does not exist."))
  }
  
  # Get the folder name
  folder_name <- basename(folder_path)
  
  # Extract the first 3 elements of the folder name
  folder_parts <- strsplit(folder_name, "_")[[1]]
  if (length(folder_parts) < 3) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "Folder name does not have at least three elements."))
  }
  expected_filename <- paste(folder_parts[1:3], collapse = "_")
  
  # List Excel files inside the folder
  excel_files <- list.files(folder_path, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
  
  if (length(excel_files) == 0) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "No Excel file found in the folder."))
  }
  
  # Find the Excel file that matches the expected name
  matching_files <- excel_files[basename(excel_files) == paste0(expected_filename, ".xlsx") | 
                                  basename(excel_files) == paste0(expected_filename, ".xls")]
  
  if (length(matching_files) == 0) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "No matching Excel file found."))
  }
  
  # Load the Excel file
  excel_file <- matching_files[1]  # Take the first match if multiple exist
  
  # Read the sheet names
  sheet_names <- tryCatch(excel_sheets(excel_file), error = function(e) return(NULL))
  
  if (is.null(sheet_names)) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "Error reading Excel file."))
  }
  
  # Check if the sheet with the same name as the Excel file exists
  if (!(expected_filename %in% sheet_names)) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "Sheet with expected name not found in the Excel file."))
  }
  
  # Load the sheet
  sheet_data <- tryCatch(read_excel(excel_file, sheet = expected_filename, col_types = "text"), 
                         error = function(e) return(NULL))
  
  if (is.null(sheet_data)) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "Error loading expected sheet."))
  }
  
  # Convert to data.table
  sheet_data <- as.data.table(sheet_data)
  
  # Define mandatory columns
  mandatory_columns <- cols
  
  # Check if all mandatory columns exist
  missing_columns <- setdiff(mandatory_columns, colnames(sheet_data))
  
  if (length(missing_columns) > 0) {
    return(data.table(Folder = folder_name, Result = "Invalid", 
                      Comment = paste("Missing mandatory columns:", paste(missing_columns, collapse = ", "))))
  }
  
  # If all checks pass
  return(data.table(Folder = folder_name, Result = "Valid", Comment = "All checks passed successfully."))
}

