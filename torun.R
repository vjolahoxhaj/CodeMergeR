rm(list=ls())
get_project_folder <- function() {
  # 1) Preferred: RStudio editor context
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && !is.null(ctx$path) && nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path, winslash = "/", mustWork = FALSE)))
    }
  }
  
  # 2) Fallback: current working directory
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

projectFolder <- get_project_folder()
setwd(projectFolder)

message("Project folder: ", projectFolder)


# -----------------------------
# Helper: reset folders safely
# -----------------------------
reset_directories <- function(base_dir, folders) {
  for (folder_name in folders) {
    folder_path <- file.path(base_dir, folder_name)
    
    if (dir.exists(folder_path)) {
      unlink(folder_path, recursive = TRUE, force = TRUE)
      message("Deleted folder: ", folder_path)
    } else {
      message("Folder not found (skipped): ", folder_path)
    }
    
    dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    message("Created folder: ", folder_path)
  }
}

# -----------------------------
# Load package setup
# -----------------------------
packages_file <- file.path(projectFolder, "packages.R")
if (!file.exists(packages_file)) {
  stop("packages.R not found in project folder: ", packages_file)
}
source(packages_file)

# -----------------------------
# Define app paths
# -----------------------------
app_file <- file.path(projectFolder, "app.R")
if (!file.exists(app_file)) {
  stop("app.R not found in project folder: ", app_file)
}

folders_to_reset <- c(
  "Algorithms",
  "Available",
  "Cleaned_Codelist",
  "Conceptsets",
  "Errors",
  "Report_files",
  "temp_unzip",
  "Untouched Codelist"
)

# -----------------------------
# Clean and recreate folders
# -----------------------------
reset_directories(projectFolder, folders_to_reset)

# -----------------------------
# Run app
# -----------------------------
shiny::runApp(
  appDir = app_file,
  launch.browser = TRUE,
  display.mode = "normal"
)

#Output_folder
#Cleaned_codelist

#After all data has been shown in the webpage press the button Capture Screenshot in each of the tabs and 
#save the downloaded images inside the output folder.

