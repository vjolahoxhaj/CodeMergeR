
process_study_data <- function(mother_excel, study_col, alg_dir, cond_dir, not_pres) {
  
study_data<-mother_excel
  
  ### Step 2: Extract Codesheet Names from Algorithms ###
  if (!dir.exists(alg_dir)) stop("Algorithms folder not found.")
  alg_folders <- list.dirs(alg_dir, recursive = FALSE, full.names = TRUE)
  alg_data <- data.table(variable_name = sapply(alg_folders, function(folder) {
    folder_name <- basename(folder)
    folder_parts <- strsplit(folder_name, "_")[[1]]
    if (length(folder_parts) < 3) return(NA)
    return(paste(folder_parts[1:3], collapse = "_"))
  }, USE.NAMES = FALSE))
  alg_data[,type:="Algorithm"]
  
  
  # Combine extracted Codesheet names
  if (alg_data[,.N] > 0){
    alg_data <- data.table(variable_name = character(0), type= character(0))
  }
  
  ### Step 3: Extract Folder Names from Conceptsets ###
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
  cond_data[,type:="Conceptsets"]
  ### Step 4: Filter Algorithms & Conceptsets Using Study Data ###
  alg_filtered <- merge.data.table(alg_data, study_data, by="variable_name")
  cond_filtered <- merge.data.table(cond_data, study_data, by="variable_name")
  

  # Combine all variable names from both sources
  combined_vars <- rbindlist(list(alg_filtered, cond_filtered), use.names = TRUE, fill = TRUE)
  combined_vars <- unique(combined_vars)
  # If no values exist, ensure an empty table is returned
  if (nrow(combined_vars) == 0) {
    combined_vars <- data.table(variable_name = character(0))
  }

   ###  Step 5: Return Structured List ###
  return(combined_vars)
  
  
  
}



