not_present_study_data <- function(mother_excel, study_col, alg_dir, cond_dir) {
  
  ###  Step 1: Extract Relevant Data from Mother Excel ###
  study_data <- tryCatch({
    excel_data <- read_excel(mother_excel, col_types = "text", sheet = "CDM_EVENTS")
    setDT(excel_data)  # Convert to data.table
    
    # Ensure required columns exist
    if (!all(c("Event_abbreviation / Variable Name", study_col) %in% colnames(excel_data))) {
      stop("Required columns not found in the Mother Excel file.")
    }
    
    # Select and rename columns
    study_data <- excel_data[, .(variable_name = `Event_abbreviation / Variable Name`, study_name = get(study_col))]
    
    # Filter rows where study_name is "YES"
    study_data <- study_data[tolower(study_name) == "yes"]
    
    return(study_data)
    
  }, error = function(e) {
    stop("Error reading the Mother Excel file: ", e$message)
  })
  
  ###  Step 2: Extract Codesheet Names from Algorithms ###
  if (!dir.exists(alg_dir)) stop("Algorithms folder not found.")
  alg_folders <- list.dirs(alg_dir, recursive = FALSE, full.names = TRUE)
  
  alg_codesheet_names <- list()
  
  for (folder in alg_folders) {
    folder_name <- basename(folder)
    
    # Extract first 3 elements of the folder name
    folder_parts <- strsplit(folder_name, "_")[[1]]
    if (length(folder_parts) < 3) next
    expected_filename <- paste(folder_parts[1:3], collapse = "_")
    
    # Find the matching Excel file
    excel_files <- list.files(folder, pattern = paste0("^", expected_filename, "\\.xlsx$|^", expected_filename, "\\.xls$"), full.names = TRUE)
    
    if (length(excel_files) == 0) next
    
    tryCatch({
      sheet_names <- excel_sheets(excel_files[1])
      if (!(expected_filename %in% sheet_names)) next
      
      sheet_data <- read_excel(excel_files[1], sheet = expected_filename, col_types = "text")
      setDT(sheet_data)
      
      if ("Codesheet_name" %in% colnames(sheet_data)) {
        alg_codesheet_names[[folder]] <- unique(sheet_data[, .(variable_name = Codesheet_name)])
      }
    }, error = function(e) { next })
  }
  
  # Combine extracted Codesheet names
  if (length(alg_codesheet_names) > 0) {
    alg_data <- unique(rbindlist(alg_codesheet_names, use.names = FALSE, fill = TRUE))
  } else {
    alg_data <- data.table(variable_name = character(0))
  }
  
  ###  Step 3: Extract Folder Names from Conceptsets ###
  if (!dir.exists(cond_dir)) stop("Conceptsets folder not found.")
  cond_folders <- list.dirs(cond_dir, recursive = FALSE, full.names = TRUE)
  
  # Extract subset names (first 3 elements of folder names)
  cond_data <- data.table(variable_name = sapply(cond_folders, function(folder) {
    folder_name <- basename(folder)
    folder_parts <- strsplit(folder_name, "_")[[1]]
    if (length(folder_parts) < 3) return(NA)
    return(paste(folder_parts[1:3], collapse = "_"))
  }, USE.NAMES = FALSE))
  
  cond_data <- cond_data[!is.na(variable_name)]  # Remove NAs
  
  
  ###  Step 4: Find Study Variables **Not Present** ###
  # Combine all variable names from both sources
  combined_vars <- rbindlist(list(alg_data, cond_data), use.names = TRUE, fill = TRUE)
  combined_vars <- unique(combined_vars)
  combined_vars[,present_folder:=1]
  
  missing_study_vars <- merge.data.table(study_data, combined_vars, by="variable_name", all.x=T)
  missing_study_vars<-missing_study_vars[is.na(present_folder)]
  
  ###  Step 5: Return Output ###
  if (nrow(missing_study_vars) == 0) {
    return(data.table(variable_name = "All study Conceptsets are present in either Algorithms or Conceptsets"))
  } else {
    return(missing_study_vars)
  }
}
