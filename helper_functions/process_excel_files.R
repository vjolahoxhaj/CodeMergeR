
process_excel_files <- function(base_dir, selected_columns, rename_columns) {
  
  # Get all folders in the base directory
  folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  
  # Initialize list to store processed data
  data_list <- list()
  
  for (folder in folders) {
    folder_name <- basename(folder)
    
    # Extract variable_name (first 3 elements of folder name)
    folder_parts <- strsplit(folder_name, "_")[[1]]
    if (length(folder_parts) < 4) next  # Skip if folder name is malformed
    
    variable_name <- paste(folder_parts[1:3], collapse = "_")
    event_definition <- folder_parts[4]  # Extract the 4th element
    
    # Get all Excel files in the folder
    excel_files <- list.files(folder, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
    
    for (file in excel_files) {
      tryCatch({
        var_sheet<-strsplit(basename(file), ".xls")[[1]][1]
        # Read the first sheet (modify if you need a specific sheet)
        sheet_data <- read_excel(file, sheet=var_sheet, col_types = "text")
        setDT(sheet_data)  # Convert to data.table
        
        # Keep only necessary columns
        sheet_data <- sheet_data[, ..selected_columns, with = FALSE]
        
        # Rename columns based on rename_columns mapping
        setnames(sheet_data, old = names(rename_columns), new = rename_columns)
        
        # Add new columns
        sheet_data[, variable_name := variable_name]
        sheet_data[, event_definition := event_definition]
        
        # Store processed data
        data_list[[paste0(folder_name, "_", basename(file))]] <- sheet_data
        
      }, error = function(e) {
        message(paste("Error processing file:", file, "-", e$message))
      })
    }
  }
  
  # Combine all extracted data
  if (length(data_list) > 0) {
    final_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
  } else {
    final_data <- data.table()
  }
  
  return(final_data)
}


