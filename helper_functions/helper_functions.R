# Define helper functions here
check_folder_names <- function(folders) {
  invalid <- basename(folders)[str_count(basename(folders), "_") != 3]
  if (length(invalid) == 0) {
    return("All folder names are valid.")
  } else {
    return(invalid)
  }
}

check_study_name <- function(excel_path, study_name) {
  tryCatch({
    data <- read_excel(excel_path, sheet = "CDM_EVENTS")
    if (study_name %in% colnames(data)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    return(FALSE)
  })
}
