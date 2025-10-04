merge_tsv_files <- function(folder_path) {
  # List all .tsv files in the folder
  tsv_files <- list.files(folder_path, pattern = "\\.tsv$", full.names = TRUE)
  
  if (length(tsv_files) == 0) {
    stop("No .tsv files found in the specified folder.")
  }
  
  # Read all .tsv files into a list
  data_list <- lapply(tsv_files, function(file) {
    fread(file, sep = "\t", na.strings = c("NA"), quote = "", strip.white = T)
  })
  
  # Ensure all tables have the same columns
  common_cols <- Reduce(intersect, lapply(data_list, names))
  data_list <- lapply(data_list, function(dt) dt[, ..common_cols])
  
  # Combine all data.tables into one
  merged_data <- rbindlist(data_list, fill = TRUE)
  
  return(merged_data)
}
