# Create error directory if not exists
error_dir <- file.path(projectFolder, "Errors")
# Function to validate folder contents
validate_folders <- function(folder_path, file_extension = "\\.(xlsx|xls)$") {
  invalid_folders <- list()
  
  if (length(folders) > 0) {
    for (folder in folders) {
      files <- list.files(folder, pattern = file_extension, full.names = T)
      if (length(files) != 1) {
        invalid_folders <- append(invalid_folders, folder)
      }
    }
  }
  
  return(invalid_folders)
}

# Handle deletion and logging
process_folders <- function(folders, folder_type) {
  invalid_folders <- validate_folders(folders)
  
  if (length(invalid_folders) > 0) {
    # Extract folder names without full path
    invalid_folder_names <- lapply(invalid_folders, basename)
    invalid_folder_names<-do.call(rbind,invalid_folder_names)
    # Log invalid folder names
    error_log <- data.table(Invalid_Folder = invalid_folder_names)
    names(error_log)<-"Invalid_Folder"
    fwrite(error_log, file.path(error_dir, paste0("non_compliant_excel_", folder_type, ".csv")))
    
    # Remove invalid folders
    unlink(invalid_folders, recursive = TRUE)
  }
}



