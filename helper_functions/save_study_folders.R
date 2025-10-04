# Function to create the "Available" folder and copy necessary folders into it
save_study_folders <- function(results_present, cond_dir) {
  available_dir <- file.path(dirname(cond_dir), "Available")  # Define "Available" folder path
  
  # Ensure the "Available" folder exists; recreate it if already present
  if (dir.exists(available_dir)) {
    unlink(available_dir, recursive = TRUE, force = TRUE)  # Delete if exists
  }
  dir.create(available_dir, recursive = TRUE, showWarnings = FALSE)  # Create fresh "Available" folder
  
  # Get the list of required variable names from results_present
  required_variables <- results_present$variable_name
  
  # Get all folder names in Conditions directory
  condition_folders <- list.dirs(cond_dir, recursive = FALSE, full.names = FALSE)
  
  # Copy from Conditions and Algorithms folders if they exist
  for (variable_name in required_variables) {
    
    # Find matching folders in Conditions (subset match)
    matching_cond_folders <- condition_folders[grepl(paste0("^", variable_name, "_"), condition_folders)]
    
    for (folder in matching_cond_folders) {
      cond_path <- file.path(cond_dir, folder)
      #available_path <- file.path(available_dir, folder)
      
      # Copy the matched folder
      if (dir.exists(cond_path)) {
        file.copy(cond_path, available_dir, recursive = TRUE)
      }
    }
    
  }
  
  return(paste("Available folders copied to:", available_dir))
}

