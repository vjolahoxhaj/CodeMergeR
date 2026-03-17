
check_folder_file_match <- function(folder_path) {
  # Get folder name
  folder_name <- basename(folder_path)
  
  # Extract the subset of the folder name (first three elements split by "_")
  folder_subset <- paste(strsplit(folder_name, "_")[[1]][1:3], collapse = "_")
  
  # List all .xlsx and .xls files in the folder
  xlsx_files <- list.files(folder_path, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  
  # If there are no Excel files, return Invalid
  if (length(xlsx_files) == 0) {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "No Excel file found"))
  }
  
  # Check if the file name matches the folder subset
  file_matches <- sapply(xlsx_files, function(file) {
    file_name <- tools::file_path_sans_ext(basename(file))  # Get file name without extension
    file_name == folder_subset
  })
  
  # Determine result and comment
  if (any(file_matches)) {
    return(data.table(Folder = folder_name, Result = "Valid", Comment = "Name does match"))
  } else {
    return(data.table(Folder = folder_name, Result = "Invalid", Comment = "Name doesn't match"))
  }
}

