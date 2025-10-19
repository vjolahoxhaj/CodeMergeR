# Function to check one xlsx file per folder
check_excel_in_folder <- function(folder_path) {
  # List all .xlsx files in the folder
  xlsx_files <- list.files(folder_path, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  
  # Check if there is exactly one .xlsx file
  if (length(xlsx_files) == 1) {
    return(data.table(Folder=basename(folder_path), Result="Valid", Comment= "All concepts’ folders contain exactly one Excel file"))
  } else if (length(xlsx_files) == 0) {
    return(data.table(Folder=basename(folder_path), Result="Invalid", Comment= "The following folders contain zero Excel files. Create the clinical concept Excel file."))
  } else {
    return(data.table(Folder=basename(folder_path), Result="Invalid", Comment= "The following folders contain multiple Excel files. Keep only one Excel file in case of multiple files."))
  }
}


