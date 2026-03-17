# Function to check one xlsx file per folder
check_excel_in_folder <- function(folder_path) {
  # List all .xlsx files in the folder
  xlsx_files <- list.files(folder_path, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  
  # Check if there is exactly one .xlsx file
  if (length(xlsx_files) == 1) {
    return(data.table(Folder=basename(folder_path), Result="Valid", Comment= "1 Excel file found"))
  } else if (length(xlsx_files) == 0) {
    return(data.table(Folder=basename(folder_path), Result="Invalid", Comment= "No Excel file found"))
  } else {
    return(data.table(Folder=basename(folder_path), Result="Invalid", Comment= "Multiple Excel files found"))
  }
}


