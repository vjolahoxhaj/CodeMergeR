compare_algorithm_conceptsets_names <- function(alg_dir, cond_dir) {
  ### Step 1: Extract "Codesheet_name" Values from Algorithm Files ###
  if (!dir.exists(alg_dir)) {
    return(data.table(Category = "Error", Name = "None", Comment = "Algorithms folder not found."))
  }
  
  # Get all algorithm folders
  alg_folders <- list.dirs(alg_dir, recursive = FALSE, full.names = TRUE)
  if (length(alg_folders) == 0) {
    return(data.table(Category = "Error", Name = "None", Comment = "No folders in Algorithms directory."))
  }
  
  #use to filter the alg folders later
  alg_data <- data.table(variable_name = sapply(alg_folders, function(folder) {
    folder_name <- basename(folder)
    folder_parts <- strsplit(folder_name, "_")[[1]]
    if (length(folder_parts) < 3) return(NA)
    return(paste(folder_parts[1:3], collapse = "_"))
  }, USE.NAMES = FALSE))
  alg_data[,full_name:=alg_folders]
  alg_data[,alg:=1]
  
  # Extract "Codesheet_name" from all algorithm files
  alg_codesheet_names <- list()
  
  for (folder in alg_folders) {
    folder_name <- basename(folder)
    
    # Extract subset name (first 3 elements)
    folder_parts <- strsplit(folder_name, "_")[[1]]
    if (length(folder_parts) < 3) next
    expected_filename <- paste(folder_parts[1:3], collapse = "_")
    
    # Find the Excel file with the same name as expected
    excel_files <- list.files(folder, pattern = paste0("^", expected_filename, "\\.xlsx$|^", expected_filename, "\\.xls$"), full.names = TRUE)
    
    if (length(excel_files) == 0) next  # Skip if no matching file is found
    
    # Read the sheet with the same name as the Excel file
    tryCatch({
      sheet_names <- excel_sheets(excel_files[1])
      if (!(expected_filename %in% sheet_names)) next
      
      sheet_data <- read_excel(excel_files[1], sheet = expected_filename, col_types = "text")
      sheet_data <- as.data.table(sheet_data)
      
      if ("Codesheet_name" %in% colnames(sheet_data)) {
        alg_codesheet_names[[folder]] <- unique(sheet_data[, .(Codesheet_name)])
      }
    }, error = function(e) { next })
  }
  
  # Combine all extracted "Codesheet_name" values
  if (length(alg_codesheet_names) > 0) {
    alg_codesheet_names<-rbindlist(alg_codesheet_names)
    names(alg_codesheet_names)<-"variable_name"
    #identify algorithms
    alg_codesheet_names<-merge.data.table(alg_codesheet_names, alg_data,by="variable_name", all.x=T)
    alg_codesheet_names<-alg_codesheet_names[!is.na(variable_name)]
    alg_codesheet_names[is.na(alg), alg:=0]
   
    
    alg_codesheet_names_extract<-alg_codesheet_names[alg==1]
    if(alg_codesheet_names_extract[,.N]>0){
    alg_codesheet_names_extract<-alg_codesheet_names_extract[,full_name]
    alg_codesheet_names<-alg_codesheet_names[alg==0]
    alg_codesheet_names<-alg_codesheet_names[,variable_name]
    
    alg_codesheet_names_extract_list <- list()
    
    for (folder in alg_codesheet_names_extract) {
      folder_name <- basename(folder)
      
      # Extract subset name (first 3 elements)
      folder_parts <- strsplit(folder_name, "_")[[1]]
      if (length(folder_parts) < 3) next
      expected_filename <- paste(folder_parts[1:3], collapse = "_")
      
      # Find the Excel file with the same name as expected
      excel_files <- list.files(folder, pattern = paste0("^", expected_filename, "\\.xlsx$|^", expected_filename, "\\.xls$"), full.names = TRUE)
      
      if (length(excel_files) == 0) next  # Skip if no matching file is found
      
      # Read the sheet with the same name as the Excel file
      tryCatch({
        sheet_names <- excel_sheets(excel_files[1])
        if (!(expected_filename %in% sheet_names)) next
        
        sheet_data <- read_excel(excel_files[1], sheet = expected_filename, col_types = "text")
        sheet_data <- as.data.table(sheet_data)
        
        if ("Codesheet_name" %in% colnames(sheet_data)) {
          alg_codesheet_names_extract_list[[folder]] <- unique(sheet_data[, .(Codesheet_name)])
        }
      }, error = function(e) { next })
    }
    alg_codesheet_names_extract_list<-rbindlist(alg_codesheet_names_extract_list)
    names(alg_codesheet_names_extract_list)<-"variable_name"
    alg_codesheet_names_extract_list<-alg_codesheet_names_extract_list[,variable_name]
    
    alg_codesheet_names<-c(alg_codesheet_names, alg_codesheet_names_extract_list)
    }
    
    if (inherits(alg_codesheet_names, c("data.table", "data.frame"))) {
      stopifnot(is.character(variable_name), length(variable_name) == 1,
                variable_name %in% names(alg_codesheet_names))
      # extract the column by name and deduplicate its values
      alg_names <- unique(alg_codesheet_names[[variable_name]])
    } else {
      # assume it's already a vector-like object
      alg_names <- unique(alg_codesheet_names)
    }
    
    
    
    #Check if alg are still present
    alg_names_new<-data.table(variable_name = alg_names)
    #identify algorithms
    alg_names_new<-merge.data.table(alg_names_new, alg_data,by="variable_name", all.x=T)
    alg_names_new<-alg_names_new[!is.na(variable_name)]
    alg_names_new[is.na(alg), alg:=0]
    alg_names_new_extract<-alg_names_new[alg==1]
    if(alg_names_new_extract[,.N]>0){
    alg_names_new_extract<-alg_names_new_extract[,full_name]
    }
    alg_names_new<-alg_names_new[alg==0]
    if(alg_names_new[,.N]>0){
    alg_names_new<-alg_names_new[,variable_name]
    }else{alg_names_new<-NULL}
    
    if(alg_names_new_extract[,.N]>0){
    alg_names_new_extract_list <- list()
    
    for (folder in alg_names_new_extract) {
      folder_name <- basename(folder)
      
      # Extract subset name (first 3 elements)
      folder_parts <- strsplit(folder_name, "_")[[1]]
      if (length(folder_parts) < 3) next
      expected_filename <- paste(folder_parts[1:3], collapse = "_")
      
      # Find the Excel file with the same name as expected
      excel_files <- list.files(folder, pattern = paste0("^", expected_filename, "\\.xlsx$|^", expected_filename, "\\.xls$"), full.names = TRUE)
      
      if (length(excel_files) == 0) next  # Skip if no matching file is found
      
      # Read the sheet with the same name as the Excel file
      tryCatch({
        sheet_names <- excel_sheets(excel_files[1])
        if (!(expected_filename %in% sheet_names)) next
        
        sheet_data <- read_excel(excel_files[1], sheet = expected_filename, col_types = "text")
        sheet_data <- as.data.table(sheet_data)
        
        if ("Codesheet_name" %in% colnames(sheet_data)) {
          alg_codesheet_names_extract_list[[folder]] <- unique(sheet_data[, .(Codesheet_name)])
        }
      }, error = function(e) { next })
    }
    alg_names_new_extract_list<-rbindlist(alg_names_new_extract_list)
    names(alg_names_new_extract_list)<-"variable_name"
    alg_names_new_extract_list<-alg_names_new_extract_list[,variable_name]
    }else{
      alg_names_new_extract_list<-NULL
    }
    
    if(!is.null(alg_names_new) | !is.null(alg_names_new_extract_list)){
    alg_names_new<-c(alg_names_new, alg_names_new_extract_list)
    
    alg_names <- unique(alg_names_new)
  } else{ alg_names <- character(0)}
    
    #identify all 
  } else {
    alg_names <- character(0)
  }
  
  ### 2 Step 2: Extract Condition Folder Names ###
  if (!dir.exists(cond_dir)) {
    return(data.table(Category = "Error", Name = "None", Comment = "Conceptsets folder not found."))
  }
  
  # Get all conceptsets folder names
  cond_folders <- list.dirs(cond_dir, recursive = FALSE, full.names = TRUE)
  if (length(cond_folders) == 0) {
    return(data.table(Category = "Error", Name = "None", Comment = "No folders in Conceptsets directory."))
  }
  
  # Extract subset names (first 3 elements of folder names)
  cond_names <- unique(sapply(cond_folders, function(folder) {
    folder_name <- basename(folder)
    folder_parts <- strsplit(folder_name, "_")[[1]]
    if (length(folder_parts) < 3) return(NA)
    return(paste(folder_parts[1:3], collapse = "_"))
  }, USE.NAMES = FALSE))
  
  cond_names <- cond_names[!is.na(cond_names)]  # Remove NAs
  
  ### Step 3: Compare Algorithm and Condition Names ###
  missing_in_conceptsets <- setdiff(alg_names, cond_names)
  missing_in_conceptsets<-missing_in_conceptsets[!is.na(missing_in_conceptsets)]
  
  if(length(missing_in_conceptsets)>0){
    miss<-data.table(variable_name=missing_in_conceptsets)
    fwrite(miss,paste0(projectFolder, "/Errors/missing_codesheet.csv"))
  
  # Create comparison results
  comparison_results <-data.table(Category = "Missing in Conceptsets", Name = missing_in_conceptsets, Comment = "Present in Algorithms but not in Conceptsets")
  comparison_results<-comparison_results[!is.na(Name)]
}
  if (nrow(comparison_results) == 0) {
    comparison_results <- data.table(Category = "Match", Name = "All Matched", Comment = "All names are present in both Algorithms and Conceptsets")
  }
  
  return(comparison_results)
}