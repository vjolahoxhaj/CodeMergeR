
# Helper function to clear and recreate directories
initialize_directory <- function(dir_path) {
  if (dir.exists(dir_path)) {
    unlink(dir_path, recursive = TRUE)  # Clear directory if it exists
  }
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)  # Create directory
}

# Define paths for directories
temp_dir <- file.path(projectFolder, "temp_unzip")
cond_dir <- file.path(projectFolder, "Conditions")
alg_dir <- file.path(projectFolder, "Algorithms")
error_dir<- file.path(projectFolder, "Errors")
report_dir<- file.path(projectFolder, "Report_files")

# Initialize directories
initialize_directory(temp_dir)
initialize_directory(cond_dir)
initialize_directory(alg_dir)
initialize_directory(error_dir)
initialize_directory(report_dir)
options(shiny.maxRequestSize = 1000 * 1024^2)

source(paste0(projectFolder,"/helper_functions/check_excel_in_folder.R"))
source(paste0(projectFolder,"/helper_functions/check_folder_file_match.R"))
source(paste0(projectFolder,"/helper_functions/check_file_sheet_match.R"))
source(paste0(projectFolder,"/helper_functions/check_excel_mandatory_columns.R"))
source(paste0(projectFolder,"/helper_functions/compare_algorithm_condition_names.R"))
source(paste0(projectFolder,"/helper_functions/process_study_data.R"))
source(paste0(projectFolder,"/helper_functions/not_present_study_data.R"))
source(paste0(projectFolder,"/helper_functions/save_study_folders.R"))
source(paste0(projectFolder,"/helper_functions/process_excel_files.R"))
source(paste0(projectFolder,"/helper_functions/merge_tsv_files.R"))
source(paste0(projectFolder,"/helper_functions/clean_codelist.R"))


ui <- navbarPage(
  
  title = "Codelist Creation",
  id = "tabs",  # This is necessary to detect which tab is active!
  useShinyjs(),
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/sweetalert2@11"),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('alertMessage', function(message) {
        Swal.fire({
          title: message.title,
          html: message.html,
          icon: message.icon,
          confirmButtonText: 'Got it!',
          confirmButtonColor: '#3085d6',
          background: '#f9f9f9',
          width: '700px' //
        });
      });"
    ))
  ),    
  tabPanel("Conformance and coherence", 
           value = "tab1",
           sidebarLayout(
             sidebarPanel(
               textInput("string_input", "Study Name (as in Mother Excel):", placeholder = "Type here..."),
               fileInput("zip_file", "Upload Codelist Folder (.zip):", accept = ".zip"),
               fileInput("xlsx_file1", "Upload Mother Excel File (.xlsx):", accept = c(".xlsx")),
               actionButton("step3", "Run Validation"),
               width = 3
             ),
             mainPanel(
               fluidPage(
                 actionButton("capture", "Capture Screenshot"),
                 h4("Legend", style = "color: blue;"),
                 wellPanel(
                   p(tags$a(href = "#name_check", "✅ Conditions and Algorithms Folder Name Check")),
                   p(tags$a(href = "#cov_no_codes", "✅ Conditions and Algorithms with No Codes Check")),
                   p(tags$a(href = "#study_name_check", "✅ Study Name Presence Check")),
                   p(tags$a(href = "#file_count_check", "✅ Excel File Count Check")),
                   p(tags$a(href = "#folder_file_match", "✅ Folder & Excel File Name Match Check")),
                   p(tags$a(href = "#excel_sheet_match", "✅ Excel Sheet Name Check")),
                   p(tags$a(href = "#excel_col_match", "✅ Mandatory Columns Check")),
                   p(tags$a(href = "#alg_cond_match", "✅ Conditions and Algorithms Match Check"))
                 )
               )
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             h4("Rules and Guidelines", style = "color: blue;"),
             wellPanel(
               p("1. Folder names must follow the '<System>_<Condition>_<Type>_<Description>' convention."),
               p("2. Each folder must contain only one Excel file (.xlsx or .xls)."),
               p("3. The first three components of the folder name must match the Excel filename."),
               p("4. The Excel file and its sheet names must match."),
               p("5. Mandatory columns: Coding system, Tags, Code name, Concept, Concept name."),
               p("6. Folders marked as 'Covariates with no codes' are excluded."),
               p("7. Allowed tags: narrow, possible, exclude, and ignore. Currently, 'possible' and 'exclude' are used interchangeably.")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, h4("Uploaded Files Details:", style = "color: blue;"), tableOutput("file_details")),
             column(6, h4("Folder Information:", style = "color: blue;"), tableOutput("folder_info")),
             column(6, h4("Date of Execution:", style = "color: blue;"), textOutput("run_date")),
             column(6, h4("Output Files:", style = "color: blue;"), tableOutput("output_details"))
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "name_check", "Conditions Folder Name Check:", style = "color: green;"),
                    verbatimTextOutput("conditions_name_check")
             ),
             column(6, 
                    h4("Algorithms Folder Name Check:", style = "color: green;"),
                    verbatimTextOutput("algorithms_name_check")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "cov_no_codes", "Covariates with No Codes Check:", style = "color: green;"),
                    DTOutput("cov_no_codes_cond")
             ),
             column(6, 
                    h4("Algorithms with No Codes Check:", style = "color: green;"),
                    DTOutput("cov_no_codes_alg")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "study_name_check", "Study Name Presence Check:", style = "color: green;"),
                    verbatimTextOutput("study_name_check")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "file_count_check", "Conditions Excel File Check:", style = "color: green;"),
                    DTOutput("step3_results_cond")
             ),
             column(6, 
                    h4("Algorithms Excel File Check:", style = "color: green;"),
                    DTOutput("step3_results_alg")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "folder_file_match", "Conditions Folder & Excel File Name Match Check:", style = "color: green;"),
                    DTOutput("step3_results_name_cond")
             ),
             column(6, 
                    h4("Algorithms Folder & Excel File Name Match Check:", style = "color: green;"),
                    DTOutput("step3_results_name_alg")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "excel_sheet_match", "Conditions Excel Sheet Name Check:", style = "color: green;"),
                    DTOutput("step3_results_sheet_cond")
             ),
             column(6, 
                    h4("Algorithms Excel Sheet Name Check:", style = "color: green;"),
                    DTOutput("step3_results_sheet_alg")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "excel_col_match", "Conditions Mandatory Columns Check:", style = "color: green;"),
                    DTOutput("results_col_cond")
             ),
             column(6, 
                    h4("Algorithms Mandatory Columns Check:", style = "color: green;"),
                    DTOutput("results_col_alg")
             )
           ),
           
           tags$hr(),
           
           fluidRow(
             column(6, 
                    h4(id = "alg_cond_match", "Conditions and Algorithms Match Check:", style = "color: green;"),
                    DTOutput("results_alg_cond")
             )
           )
  ),
  
  
  # Second Page - Logs and Reports
  tabPanel("Cleaning and standardization",
           value = "tab2",
           sidebarPanel(
             # Adding Clickable Legend
             h4("Legend", style = "color: blue;"),
             wellPanel(
               p(tags$a(href = "#av_data", "✅ Available Conditions")),
               p(tags$a(href = "#not_av_data", "✅ Missing necessary conditions/algorithms for the study")),
               p(tags$a(href = "#issues", "✅ Issues with codes: rages, scientific notations, rounding")),
               p(tags$a(href = "#summary", "✅ Codelist summary")),
               p(tags$a(href = "#tags", "✅ Tags summary"))
             )
             
           ),
           
           fluidPage(
             actionButton("capture_2", "Capture Screenshot")
           ), 
           
           
           fluidPage(
             h3("Validation Logs & Reports", style = "color: blue;"),
             actionButton("step4", "Run Presence and Absence of Variables"),
             actionButton("step5", "Show Presence of Variables"),
             actionButton("step7", "Show Absence of Variables"),
             
             
             fluidRow(
               column(6, 
                      h4(id="av_data", "✅ Study Conditions Available:", style = "color: green;"),
                      DTOutput("results_av")
               ),
               column(6, 
                      h4(id="not_av_data", "❌ Study Conditions Missing:", style = "color: red;"),
                      DTOutput("results_not_present")
               )
             )
           ),
           tags$hr(),
           
           fluidPage(
             h3("Process Excel Files", style = "color: blue;"),
             actionButton("process_files", "Run Processing", class = "btn btn-primary"),
             
             tags$hr(),
             
             verbatimTextOutput("process_status"),
             
             tags$hr(),
             
             h3("Final Codelist", style = "color: blue;"),
             
             # Button to trigger processing (Initially Disabled)
             actionButton("clean_files", "Run Cleaning", class = "btn btn-primary"),
             
             tags$hr(),
             
             actionButton("step6", "Show results"),
             
             fluidRow(
               column(6, 
                      h4(id ="issues", "❌ Codes containing scientific notation (E+):", style = "color: red;"),
                      DTOutput("incorrect_codes")
               ),
               column(6, 
                      h4("❌ Codes containing ranges:", style = "color: red;"),
                      DTOutput("range_codes")
               )
             ),
             
             tags$hr(),
             
             fluidRow(
               column(6, 
                      h4("❌ Missing Data:", style = "color: red;"),
                      DTOutput("missing_values_report")
               )
             ),
             
             tags$hr(),
             
             fluidRow(
               column(6, 
                      h4("❌ ️Rounding Issues:", style = "color: red;"),
                      DTOutput("rounding")
               ),
               column(6, 
                      h4("⚠️ Possible Rounding Issues:", style = "color: orange;"),
                      DTOutput("rounding_p")
               )
             ),
             
             tags$hr(),
             
             verbatimTextOutput("process_status_2")
           ),
           
           tags$hr(),
           
           fluidPage(
             h3("Codelist summary", style = "color: blue;"),
             
             tags$hr(),
             
             fluidRow(
               column(12, 
                      h4(id ="summary", "Codelist summary:", style = "color: green;"),
                      DTOutput("summary")
               )
             ),
             
             fluidRow(
               column(12, 
                      h4(id ="tags", "Tags summary:", style = "color: green;"),
                      DTOutput("tags")
               )
             )
           ),
           tags$hr()
  )
  
  # fluidPage(
  #   h3("Download", style = "color: blue;"),
  # # **Download Button for UI as HTML**
  # downloadButton("download_report", "Download Report as PDF")
  # )
)


server <- function(input, output, session) {
  
  observeEvent(input$tabs, {
    if (input$tabs == "tab1") {
      session$sendCustomMessage("alertMessage", list(
        title = "📌 Welcome to the Conformance and coherence Tab!",
        html = "<div style='text-align: left; font-size: 13px;'>
                <p><strong>Instructions:</strong></p>
                <ul>
                  <li>📂 <b>Upload</b> each file and <b>wait</b> until upload is complete.</li>
                  <li>▶️ Press <b>Run Validation</b> and <b>scroll</b> to the end of the page.</li>
                  <li>⏳ Wait until <b>all results</b> are displayed.</li>
                  <li>📸 Press <b>Capture Screenshot</b> at the top.</li>
                  <li>💾 Save screenshot inside the <b>Cleaned Codelist</b> folder.</li>
                </ul>
              </div>",
        icon = "info"
      ))
    } else if (input$tabs == "tab2") {
      session$sendCustomMessage("alertMessage", list(
        title = "📌 Welcome to the Cleaning and standardization Tab!",
        html = "<div style='text-align: left; font-size: 13px;'>
                <p><strong>Instructions:</strong></p>
                <ul>
                  <li>▶️ Press <b>Run Presence and Absence of Variables</b>.</li>
                  <li>⏳ Wait <b>10 seconds </b>.</li>
                  <li>▶️ Press <b>Show Presence of Variables</b>.</li>
                  <li>▶️ Press <b>Show Absence of Variables</b>.</li>
                  <li>▶️ Press <b>Run Processing</b>.</li>
                  <li>⏳ Wait until the comment <b>✅ Processing Complete! Results saved in 'Untouched Codelist'.</b> shows.</li>
                  <li>▶️ Press <b>Run Cleaning</b>.</li>
                  <li>⏳ Wait until the comment <b>✅ Cleaning Complete! Results saved in 'Cleaned Codelist'.</b> shows.</li>
                  <li>▶️ Press <b>Show results</b>.</li>
                  <li>📸 Press <b>Capture Screenshot</b> at the top.</li>
                  <li>💾 Save screenshot inside the <b>Cleaned Codelist</b> folder.</li>
                </ul>
              </div>",
        icon = "info"
      ))
    }
  })
  
  
  
  # **Reactive variable to store the date**
  run_date <- reactiveVal(format(Sys.Date(), "%Y-%m-%d"))
  
  # **Update date when files are uploaded**
  observeEvent(input$xlsx_file1, {
    run_date(format(Sys.Date(), "%Y-%m-%d"))
  })
  
  # **Display the date in UI**
  output$run_date <- renderText({
    run_date()
  })
  folder_names <- reactiveVal(NULL)  # Reactive value to store folder names
  algorithms_folders <- reactiveVal(NULL)  # Reactive value to store folders inside "Algorithms"
  algorithms_exists <- reactiveVal(FALSE)  # Reactive value to store if Algorithms exist
  other_folders_exist <- reactiveVal(FALSE)  # Reactive value to store if other folders exist
  study_name_exists <- reactiveVal(FALSE)
  
  #### Output info box ####
  uploaded_files <- reactive({
    files <- list()
    
    if (!is.null(input$zip_file)) {
      files[["ZIP File"]] <- input$zip_file$name
    }
    
    if (!is.null(input$xlsx_file1)) {
      files[["Mother Excel File"]] <- input$xlsx_file1$name
    }
    
    files
    
  })
  
  output$file_details <- renderTable({
    files <- uploaded_files()
    if (length(files) == 0) {
      return(data.frame(
        File_Type = character(),
        File_Name = character()
      ))
    }
    
    data.frame(
      File_Type = names(files),
      File_Name = unlist(files),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)
  
  output$folder_info <- renderTable({
    folders <- folder_names()
    if (is.null(folders)) {
      return(data.frame(
        Algorithms = NA,
        Conditions = NA
      ))
    }
    data.frame(
      Algorithms = ifelse(algorithms_exists(), "Yes", "No"),
      Conditions = ifelse(other_folders_exist(), "Yes", "No"),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)
  
  output$parameter <- renderText({
    input$string_input
  })
  
  output$output_details <- renderTable({
    req(input$string_input)
    study_name<-input$string_input
    codelist<-paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"), "_", study_name, "_full_codelist.csv")
    preg_codelist<-paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"), "_", study_name, "_pregnancy_algorithm_codelist.csv")
    
    data.frame(
      Codelist = codelist,
      PrA_Codelist = preg_codelist
    )
  }, rownames = FALSE)
  
  #### Step 1: Folders check ####
  #Check for invalid names in the conditions and algorithm folders
  observeEvent(input$zip_file, {
    req(input$zip_file)
    unzip(input$zip_file$datapath, exdir = temp_dir)  # Unzip the file
    
    # Remove folders not matching a specific pattern
    pattern <- unlist(strsplit(input$zip_file$name, ".zip"))  # Replace with your pattern
    folders <- list.files(temp_dir)  # Get all folder names
    matching_folders <- grep(pattern, folders, value = TRUE)  # Filter matching folders
    # Delete folders not matching the pattern
    non_matching_folders <- setdiff(folders, matching_folders)
    unlink(paste0(temp_dir, non_matching_folders), recursive = TRUE, force = TRUE)
    
    files_all <- list.files(paste0(temp_dir, "/", matching_folders, "/"))
    print(files_all)
    
    # Check if the Algorithm folder exists
    if ("Algorithms" %in% files_all) {
      algorithms_exists(TRUE)
      algorithms_folders(list.files(paste0(temp_dir, "/", matching_folders, "/Algorithms/")))
      
      algo_files <- list.files(paste0(temp_dir, "/", matching_folders, "/Algorithms/"), full.names = TRUE)
      valid_algorithms <- algo_files[sapply(basename(algo_files), function(name) {
        str_count(name, "_") == 3  # Only include files with exactly 3 underscores
      })]
      
      # Copy valid Algorithms files to alg_dir
      if (length(valid_algorithms) > 0) {
        file.copy(valid_algorithms, alg_dir, recursive = TRUE)
      }      
      unlink(paste0(temp_dir, "/", matching_folders, "/Algorithms"), recursive = TRUE, force = TRUE)
    } else {
      algorithms_folders(NULL)
      algorithms_exists(FALSE)
    }
    
    folder_names(files_all[!files_all %in% "Algorithms"])
    
    if (length(folder_names()) > 0) {
      other_folders_exist(TRUE)
      
      cond_files <- list.files(paste0(temp_dir, "/", matching_folders, "/"), full.names = TRUE)
      valid_cond <- cond_files[sapply(basename(cond_files), function(name) {
        str_count(name, "_") == 3  # Only include folders with exactly 3 underscores
      })]
      
      # Copy valid Conditions folders to cond_dir
      if (length(valid_cond) > 0) {
        file.copy(valid_cond, cond_dir, recursive = TRUE)
      }
      
    } else {
      folder_names(NULL)
      other_folders_exist(FALSE)
    }
    
    # Delete temp folder
    unlink(temp_dir, recursive = TRUE, force = TRUE)
    
  })
  
  
  #Output all files that do not comply with 3 underscores rule
  output$conditions_name_check <- renderPrint({
    folders <- folder_names()
    if (is.null(folders)) {
      return("No folders in Conditions to check.")
    }
    
    invalid_folders <- folders[sapply(folders, function(name) {
      str_count(name, "_") != 3
    })]
    
    if (length(invalid_folders) > 0) {
      print("Conditions folders with invalid names:")
      invalid_folder_rep<-data.table(wrong_folder_names=invalid_folders)
      fwrite(invalid_folder_rep, paste0(report_dir, "/conditions_name_check.csv"), row.names = F)
      
      return(invalid_folders)
      
    } else {
      return("All Conditions folders are named correctly.")
    }
    rm(folders)
    
    
    
  })
  
  output$algorithms_name_check <- renderPrint({
    folders <- algorithms_folders()
    if (is.null(folders)) {
      return("No folders in Algorithms to check.")
    }
    
    invalid_folders <- folders[sapply(folders, function(name) {
      str_count(name, "_") != 3
    })]
    
    if (length(invalid_folders) > 0) {
      print("Algorithms files with invalid names:")
      invalid_folder_rep<-data.table(wrong_folder_names=invalid_folders)
      fwrite(invalid_folder_rep, paste0(report_dir, "/algorithms_name_check.csv"), row.names = F)
      
      return(invalid_folders)
    } else {
      return("All Algorithms files are named correctly.")
    }
    rm(folders)
    
  })  
  
  #### covariates with no codes ####
  cov_no_codes_cond <- eventReactive(input$xlsx_file1,{
    req(input$xlsx_file1)
    
    # Read the Excel file
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
    
    # Process Data
    excel_data <- as.data.table(excel_data)
    if (!all(c("Event_abbreviation / Variable Name", "Covariates with no codes") %in% colnames(excel_data))) {
      return(data.table(Folder = "None", Result = "Invalid", Comment = "Required columns not found in the Mother Excel file."))
    }
    
    # Rename and filter
    setnames(excel_data, old = c("Event_abbreviation / Variable Name", "Covariates with no codes"),
             new = c("variable_name", "covariates_no_code"))
    excel_data[, covariates_no_code := tolower(covariates_no_code)]
    excel_data <- excel_data[covariates_no_code == "yes"]
    
    if (nrow(excel_data) > 0) excel_data[, covariates_no_code := 1]
    
    # Check Conditions Folder
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conditions folder found."))
    condition_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(condition_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conditions directory."))
    
    # Extract relevant folder details
    folder_details <- lapply(condition_folders, function(folder) {
      excel_files <- list.files(folder, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
      folder_name <- basename(folder)
      data.table(variable_name = paste(strsplit(folder_name, "_")[[1]][1:3], collapse = "_"),
                 has_excel_files = length(excel_files) > 0)
    })
    
    folder_data <- rbindlist(folder_details)
    setnames(folder_data, c("variable_name", "has_excel"))
    
    # Merge with Mother Excel data
    folder_data <- merge(folder_data, excel_data, by = "variable_name", all.x = TRUE)
    folder_data[is.na(covariates_no_code), covariates_no_code := 0]
    folder_data <- folder_data[covariates_no_code == 1]
    
    if (nrow(folder_data) > 0) {
      result<-data.table(Folder = folder_data$variable_name, Result = "Invalid",
                         Comment = "Folder has been specified as 'Covariates with no codes' but contains an Excel file.")
      fwrite(result, paste0(report_dir, "/no_code_covariates_conditions.csv"), row.names = F)
      
      return(result)
    } else {
      result<-data.table(Folder = "None", Result = "Valid",
                         Comment = "'Covariates with no codes' folders are correctly specified.")
      fwrite(result, paste0(report_dir, "/no_code_covariates_conditions.csv"), row.names = F)
      
      return(result)
    }
    
  })
  
  # Output Table
  output$cov_no_codes_cond <- renderDT({
    datatable(cov_no_codes_cond(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })  
  
  cov_no_codes_alg <- eventReactive(input$xlsx_file1,{
    req(input$xlsx_file1)
    
    # Read the Excel file
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
    
    # Process Data
    excel_data <- as.data.table(excel_data)
    if (!all(c("Event_abbreviation / Variable Name", "Covariates with no codes") %in% colnames(excel_data))) {
      return(data.table(Folder = "None", Result = "Invalid", Comment = "Required columns not found in the Mother Excel file."))
    }
    
    # Rename and filter
    setnames(excel_data, old = c("Event_abbreviation / Variable Name", "Covariates with no codes"),
             new = c("variable_name", "covariates_no_code"))
    excel_data[, covariates_no_code := tolower(covariates_no_code)]
    excel_data <- excel_data[covariates_no_code == "yes"]
    
    if (nrow(excel_data) > 0) excel_data[, covariates_no_code := 1]
    
    # Check Algorithms Folder
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    alg_folders <- list.dirs(alg_dir, recursive = FALSE)
    if (length(alg_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Algorithms directory."))
    
    # Extract relevant folder details
    folder_details <- lapply(alg_folders, function(folder) {
      excel_files <- list.files(folder, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
      folder_name <- basename(folder)
      data.table(variable_name = paste(strsplit(folder_name, "_")[[1]][1:3], collapse = "_"),
                 has_excel_files = length(excel_files) > 0)
    })
    
    folder_data <- rbindlist(folder_details)
    setnames(folder_data, c("variable_name", "has_excel"))
    
    # Merge with Mother Excel data
    folder_data <- merge(folder_data, excel_data, by = "variable_name", all.x = TRUE)
    folder_data[is.na(covariates_no_code), covariates_no_code := 0]
    folder_data <- folder_data[covariates_no_code == 1]
    
    if (nrow(folder_data) > 0) {
      result<-data.table(Folder = folder_data$variable_name, Result = "Invalid",
                         Comment = "Folder has been specified as 'Covariates with no codes' but contains an Excel file.")
      fwrite(result, paste0(report_dir, "/no_code_covariates_conditions.csv"), row.names = F)
      
      return(result)
    } else {
      result<-data.table(Folder = "None", Result = "Valid",
                         Comment = "'Covariates with no codes' folders are correctly specified.")
      fwrite(result, paste0(report_dir, "/no_code_covariates_conditions.csv"), row.names = F)
      
      return(result)
    }
  })
  
  # Output Table
  output$cov_no_codes_alg <- renderDT({
    datatable(cov_no_codes_alg(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })  
  
  
  #### delete covariates with no codes####
  # Delete Covariates with No Codes - Conditions
  cov_no_codes_delete_cond <- eventReactive(input$step3, {
    req(input$xlsx_file1)
    
    # Read the Excel file safely
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
    
    # Convert to data.table
    excel_data <- as.data.table(excel_data)
    
    # Ensure required columns exist
    if (!all(c("Event_abbreviation / Variable Name", "Covariates with no codes") %in% colnames(excel_data))) {
      return(data.table(Folder = "None", Result = "Invalid", Comment = "Required columns not found in the Mother Excel file."))
    }
    
    # Rename and filter
    setnames(excel_data, old = c("Event_abbreviation / Variable Name", "Covariates with no codes"),
             new = c("variable_name", "covariates_no_code"))
    excel_data[, covariates_no_code := tolower(covariates_no_code)]
    excel_data <- excel_data[covariates_no_code == "yes"]
    
    # Check Conditions Folder
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conditions folder found."))
    condition_folders <- list.dirs(cond_dir, recursive = FALSE, full.names = TRUE)
    if (length(condition_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conditions directory."))
    
    # Extract relevant folder details
    folder_details <- lapply(condition_folders, function(folder) {
      folder_name <- basename(folder)
      data.table(variable_name = paste(strsplit(folder_name, "_")[[1]][1:3], collapse = "_"),
                 folder_name = folder_name)
    })
    
    folder_data <- rbindlist(folder_details)
    setnames(folder_data, c("variable_name", "folder"))
    
    # Merge with Mother Excel data
    folder_data <- merge(folder_data, excel_data, by = "variable_name", all.x = TRUE)
    folder_data <- folder_data[!is.na(covariates_no_code)]
    
    # Return folders marked for deletion
    if (nrow(folder_data) > 0) {
      return(data.table(Folder = folder_data$folder, Result = "Valid"))
    } else {
      return(data.table(Folder = "None", Result = "Invalid"))
    }
  })
  
  # Delete invalid folders based on results
  observeEvent(input$step3, {
    req(cov_no_codes_delete_cond())  # Ensure function is not NULL
    
    invalid_cond_folders <- cov_no_codes_delete_cond()[Result == "Valid", Folder]
    
    if (length(invalid_cond_folders) > 0 && all(invalid_cond_folders != "None")) {
      sapply(invalid_cond_folders, function(folder) {
        folder_path <- file.path(cond_dir, folder)
        if (dir.exists(folder_path)) {
          unlink(folder_path, recursive = TRUE, force = TRUE)
          print(paste("Deleted folder:", folder_path))
        }
      })
    }
  })  
  
  
  cov_no_codes_delete_alg <- eventReactive(input$step3, {
    req(input$xlsx_file1)
    
    # Read the Excel file safely
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
    
    # Convert to data.table
    excel_data <- as.data.table(excel_data)
    
    # Ensure required columns exist
    if (!all(c("Event_abbreviation / Variable Name", "Covariates with no codes") %in% colnames(excel_data))) {
      return(data.table(Folder = "None", Result = "Invalid", Comment = "Required columns not found in the Mother Excel file."))
    }
    
    # Rename and filter
    setnames(excel_data, old = c("Event_abbreviation / Variable Name", "Covariates with no codes"),
             new = c("variable_name", "covariates_no_code"))
    excel_data[, covariates_no_code := tolower(covariates_no_code)]
    excel_data <- excel_data[covariates_no_code == "yes"]
    
    # Check Conditions Folder
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    alg_folders <- list.dirs(alg_dir, recursive = FALSE, full.names = TRUE)
    if (length(alg_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Algorithms directory."))
    
    # Extract relevant folder details
    folder_details <- lapply(alg_folders, function(folder) {
      folder_name <- basename(folder)
      data.table(variable_name = paste(strsplit(folder_name, "_")[[1]][1:3], collapse = "_"),
                 folder_name = folder_name)
    })
    
    folder_data <- rbindlist(folder_details)
    setnames(folder_data, c("variable_name", "folder"))
    
    # Merge with Mother Excel data
    folder_data <- merge(folder_data, excel_data, by = "variable_name", all.x = TRUE)
    folder_data <- folder_data[!is.na(covariates_no_code)]
    
    # Return folders marked for deletion
    if (nrow(folder_data) > 0) {
      return(data.table(Folder = folder_data$folder, Result = "Valid"))
    } else {
      return(data.table(Folder = "None", Result = "Invalid"))
    }
  })
  
  # Delete invalid folders based on results
  observeEvent(input$step3, {
    req(cov_no_codes_delete_alg())  # Ensure function is not NULL
    
    invalid_alg_folders <- cov_no_codes_delete_alg()[Result == "Valid", Folder]
    
    if (length(invalid_alg_folders) > 0 && all(invalid_alg_folders != "None")) {
      sapply(invalid_alg_folders, function(folder) {
        folder_path <- file.path(alg_dir, folder)
        if (dir.exists(folder_path)) {
          unlink(folder_path, recursive = TRUE, force = TRUE)
          print(paste("Deleted folder:", folder_path))
        }
      })
    }
  })  
  #### Step 2: Check if the study name mentioned is part of the mother excel ####
  observeEvent(input$xlsx_file1,{
    req(input$xlsx_file1)
    study_name <- input$string_input
    req(study_name)  # Ensure study name is entered
    
    # Read the Excel file
    tryCatch({
      excel_data <- as.data.table(read_excel(input$xlsx_file1$datapath, col_types="text", sheet ="CDM_EVENTS"))
      col_names <- colnames(excel_data)
      # Check if study name exists as a column
      study_name_exists(study_name %in% col_names)
    }, error = function(e) {
      study_name_exists(FALSE)
    })
  })
  
  output$study_name_check <- renderText({
    if (study_name_exists()) {
      return("The study name is present as a column in the Mother Excel file.")
    } else {
      return("The study name is NOT present as a column in the Mother Excel file.")
    }
  })
  
  #### Step 3: ####
  #Grab folder names inside the Conditions directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conditions folder found."))
    
    # Get all folder names in the Conditions directory
    condition_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(condition_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conditions directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(condition_folders, check_excel_in_folder)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) > 0) {
      fwrite(results, paste0(report_dir, "/no_files_excel_conditions.csv"), row.names = F)
      
      return(results)
    } else {
      return(data.table(Folder = "All", Result = "Valid", Comment = "All folders contain only one Excel file."))
    }  })
  
  alg_excel_results <- eventReactive(input$step3, {
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    
    # Get all folder names in the Conditions directory
    alg_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(alg_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Algorithms directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(alg_folders, check_excel_in_folder)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if(nrow(results)>0){
      fwrite(results, paste0(report_dir, "/no_files_excel_algorithms.csv"), row.names = F)
    }
    if (nrow(results) == 0) {
      results<-data.table(Folder = "All", Result = "Valid", Comment = "All folders contain only one Excel file.")
    }
    return(results)
  })
  
  # Delete invalid folders based on results
  observeEvent(input$step3, {
    # Delete invalid Conditions folders
    invalid_cond_folders <- con_excel_results()[Result == "Invalid", Folder]
    if (length(invalid_cond_folders) > 0 && invalid_cond_folders != "None") {
      sapply(invalid_cond_folders, function(folder) {
        unlink(file.path(cond_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
    
    # Delete invalid Algorithms folders
    invalid_alg_folders <- alg_excel_results()[Result == "Invalid", Folder]
    if (length(invalid_alg_folders) > 0 && invalid_alg_folders != "None") {
      sapply(invalid_alg_folders, function(folder) {
        unlink(file.path(alg_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
  })
  
  
  # Output Step 3 Results as scrollable tables
  output$step3_results_cond <- renderDT({
    datatable(con_excel_results(), options = list(scrollX = TRUE, scrollY = "300px", paging=T, pageLength = Inf))
  })
  
  output$step3_results_alg <- renderDT({
    datatable(alg_excel_results(), options = list(scrollX = TRUE, scrollY = "300px", paging=T, pageLength = Inf))
  })
  
  #### Step 4: ####
  #Grab folder names inside the Conditions directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_name_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conditions folder found."))
    
    # Get all folder names in the Conditions directory
    condition_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(condition_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conditions directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(condition_folders, check_folder_file_match)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) > 0) {
      return(results)
    } else {
      return(data.table(Folder = "All", Result = "Valid", Comment = "All folders and Excel Files names match."))
    }  })
  
  alg_excel_name_results <- eventReactive(input$step3, {
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    
    # Get all folder names in the Conditions directory
    alg_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(alg_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Algorithms directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(alg_folders, check_folder_file_match)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) == 0) {
      results<-data.table(Folder = "All", Result = "Valid", Comment = "All folders and Excel Files names match.")
    }
    return(results)
  })
  
  # Delete invalid folders based on results
  observeEvent(input$step3, {
    # Delete invalid Conditions folders
    invalid_cond_name_folders <- con_excel_name_results()[Result == "Invalid", Folder]
    if (length(invalid_cond_name_folders) > 0 && invalid_cond_name_folders != "None") {
      sapply(invalid_cond_name_folders, function(folder) {
        unlink(file.path(cond_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
    
    # Delete invalid Algorithms folders
    invalid_alg_name_folders <- alg_excel_name_results()[Result == "Invalid", Folder]
    if (length(invalid_alg_name_folders) > 0 && invalid_alg_name_folders != "None") {
      sapply(invalid_alg_name_folders, function(folder) {
        unlink(file.path(alg_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
  })
  
  
  # Output Step 3 Results as scrollable tables
  output$step3_results_name_cond <- renderDT({
    datatable(con_excel_name_results(), options = list(scrollX = TRUE, scrollY = "300px", paging=T, pageLength = Inf))
  })
  
  output$step3_results_name_alg <- renderDT({
    datatable(alg_excel_name_results(), options = list(scrollX = TRUE, scrollY = "300px", paging=T, pageLength = Inf))
  })
  
  
  #### Step 5: ####
  #Grab folder names inside the Conditions directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_sheet_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conditions folder found."))
    
    # Get all folder names in the Conditions directory
    condition_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(condition_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conditions directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(condition_folders, check_file_sheet_match)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) > 0) {
      return(results)
    } else {
      return(data.table(Folder = "All", Result = "Valid", Comment = "There is at least one sheet with the correct name."))
    }  })
  
  alg_excel_sheet_results <- eventReactive(input$step3, {
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    
    # Get all folder names in the Conditions directory
    alg_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(alg_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Algorithms directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(alg_folders, check_file_sheet_match)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) == 0) {
      results<-data.table(Folder = "All", Result = "Valid", Comment = "There is at least one sheet with the correct name.")
    }
    return(results)
  })
  
  # Delete invalid folders based on results
  observeEvent(input$step3, {
    # Delete invalid Conditions folders
    invalid_cond_sheet_folders <- con_excel_sheet_results()[Result == "Invalid", Folder]
    if (length(invalid_cond_sheet_folders) > 0 && invalid_cond_sheet_folders != "None") {
      sapply(invalid_cond_sheet_folders, function(folder) {
        unlink(file.path(cond_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
    
    # Delete invalid Algorithms folders
    invalid_alg_sheet_folders <- alg_excel_sheet_results()[Result == "Invalid", Folder]
    if (length(invalid_alg_sheet_folders) > 0 && invalid_alg_sheet_folders != "None") {
      sapply(invalid_alg_sheet_folders, function(folder) {
        unlink(file.path(alg_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
  })
  
  
  # Output Step 3 Results as scrollable tables
  output$step3_results_sheet_cond <- renderDT({
    datatable(con_excel_sheet_results(), options = list(scrollX = TRUE, scrollY = "300px", paging=T, pageLength = Inf))
  })
  
  output$step3_results_sheet_alg <- renderDT({
    datatable(alg_excel_sheet_results(), options = list(scrollX = TRUE, scrollY = "300px", paging=T, pageLength = Inf))
  })
  
  
  #### Step 6: Mandatory columns####
  #Grab folder names inside the Conditions directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_col_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conditions folder found."))
    
    # Get all folder names in the Conditions directory
    condition_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(condition_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conditions directory."))
    
    # Define mandatory columns
    cols <- c("Coding system", "Code", "Code name", "Concept", "Concept name", "Tags")
    
    # Apply the function to each folder
    results <- lapply(condition_folders, function(folder) check_excel_mandatory_columns(folder, cols))
    results <- rbindlist(results, fill = TRUE)
    
    # Filter invalid results
    results <- results[Result == "Invalid"]
    
    if (nrow(results) > 0) {
      return(results)
    } else {
      return(data.table(Folder = "All", Result = "Valid", Comment = "All checks passed successfully."))
    }
  })
  
  
  alg_excel_col_results <- eventReactive(input$step3, {
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    
    # Get all folder names in the Algorithms directory
    alg_folders <- list.dirs(alg_dir, recursive = FALSE)
    if (length(alg_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Algorithms directory."))
    
    # Define mandatory columns for algorithms
    cols <- c("Codesheet_name")
    
    # Apply the function to each folder
    results <- lapply(alg_folders, function(folder) check_excel_mandatory_columns(folder, cols))
    results <- rbindlist(results, fill = TRUE)
    
    # Filter invalid results
    results <- results[Result == "Invalid"]
    
    if (nrow(results) == 0) {
      return(data.table(Folder = "All", Result = "Valid", Comment = "All checks passed successfully."))
    }
    return(results)
  })
  
  
  # Delete invalid folders based on results
  observeEvent(input$step3, {
    # Delete invalid Conditions folders
    invalid_cond_col_folders <- con_excel_col_results()[Result == "Invalid", Folder]
    if (length(invalid_cond_col_folders) > 0 && all(invalid_cond_col_folders != "None")) {
      sapply(invalid_cond_col_folders, function(folder) {
        folder_path <- file.path(cond_dir, folder)
        if (dir.exists(folder_path)) {
          unlink(folder_path, recursive = TRUE, force = TRUE)
          print(paste("Deleted folder:", folder_path))
        }
      })
    }
    
    # Delete invalid Algorithms folders
    invalid_alg_col_folders <- alg_excel_col_results()[Result == "Invalid", Folder]
    if (length(invalid_alg_col_folders) > 0 && all(invalid_alg_col_folders != "None")) {
      sapply(invalid_alg_col_folders, function(folder) {
        folder_path <- file.path(alg_dir, folder)
        if (dir.exists(folder_path)) {
          unlink(folder_path, recursive = TRUE, force = TRUE)
          print(paste("Deleted folder:", folder_path))
        }
      })
    }
  })
  
  
  # Output Step 3 Results as scrollable tables
  output$results_col_cond <- renderDT({
    datatable(con_excel_col_results(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })
  
  output$results_col_alg <- renderDT({
    datatable(alg_excel_col_results(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })  
  #### Step 7: Algorithms vs Conditions ####
  #Grab folder names inside the Conditions directory as a reactive value
  # Reactive to retrieve and validate folders
  con_alg_results <- eventReactive(input$step3, {
    
    # Apply the function to each folder
    results <- compare_algorithm_condition_names(alg_dir, cond_dir)
    return(results)
  })
  
  
  #### Step 8: Present data ####
  # Output Step 3 Results as scrollable tables
  output$results_alg_cond <- renderDT({
    datatable(con_alg_results(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })
  
  ### Needed folders (study conditions that are present) ####
  #wite the stdudy data inside Errors
  observeEvent(input$step4, {
    req(input$xlsx_file1)
    study_name <- input$string_input
    req(study_name)  # Ensure study name is entered
    
    study_data <- tryCatch({
      excel_data <- read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
      setDT(excel_data)  # Convert to data.table
      
      # Ensure required columns exist
      if (!all(c("Event_abbreviation / Variable Name", study_name) %in% colnames(excel_data))) {
        stop("Required columns not found in the Mother Excel file.")
      }
      
      # Select and rename columns
      study_data <- excel_data[, .(variable_name = `Event_abbreviation / Variable Name`, study_status = get(study_name))]
      
      # Filter rows where study_status is "YES"
      study_data <- study_data[tolower(study_status) == "yes"]
      
      
    }, error = function(e) {
      showNotification(paste("Error reading Mother Excel file:", e$message), type = "error")
      return(NULL)
    })
    
    fwrite(study_data, paste0(projectFolder, "/Errors/study_data.csv"))  # Save the study data
    # Store data reactively for next steps
    
  })
  
  observeEvent(input$step4, {
    study_data<-fread( paste0(projectFolder, "/Errors/study_data.csv"))
    
    
    ### Step 2: Extract Codesheet Names from Algorithms ###
    if (!dir.exists(alg_dir)) stop("Algorithms folder not found.")
    alg_folders <- list.dirs(alg_dir, recursive = FALSE, full.names = TRUE)
    alg_data <- data.table(variable_name = sapply(alg_folders, function(folder) {
      folder_name <- basename(folder)
      folder_parts <- strsplit(folder_name, "_")[[1]]
      if (length(folder_parts) < 3) return(NA)
      return(paste(folder_parts[1:3], collapse = "_"))
    }, USE.NAMES = FALSE))
    alg_data[,full_name:=alg_folders]
    
    # alg_data<-merge.data.table(alg_data, study_data, by="variable_name", all.x=T)
    # alg_data<-alg_data[!duplicated(variable_name)]
    # alg_data<-alg_data[study_status == "yes"]
    
    alg_folders<-alg_data[,full_name]
    
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
    alg_codesheet_names<-do.call(rbind,alg_codesheet_names) #codesheet of algorithms needed for the study
    alg_codesheet_names<-alg_codesheet_names[!is.na(variable_name)]
    alg_codesheet_names<-unique(alg_codesheet_names)
    
    ### Step 3: Extract Folder Names from Conditions ###
    if (!dir.exists(cond_dir)) stop("Conditions folder not found.")
    cond_folders <- list.dirs(cond_dir, recursive = FALSE, full.names = TRUE)
    
    # Extract subset names (first 3 elements of folder names)
    cond_data <- data.table(variable_name = sapply(cond_folders, function(folder) {
      folder_name <- basename(folder)
      folder_parts <- strsplit(folder_name, "_")[[1]]
      if (length(folder_parts) < 3) return(NA)
      return(paste(folder_parts[1:3], collapse = "_"))
    }, USE.NAMES = FALSE))
    
    cond_data <- cond_data[!is.na(variable_name)]  # Remove NAs
    cond_data<-cond_data[!duplicated(variable_name)]
    cond_data[,present:=1]
    
    alg_codesheet_names<-merge.data.table(alg_codesheet_names, cond_data, by="variable_name", all.x = T)
    alg_codesheet_names<-alg_codesheet_names[!duplicated(variable_name)]
    alg_codesheet_names<-alg_codesheet_names[present == 1]
    
    
    comb<-rbind(alg_codesheet_names, cond_data)
    comb<-unique(comb)
    
    fwrite(comb, paste0(projectFolder, "/Errors/copy_folders.csv"))
  })
  
  # Call this function when the "Run Validation" button is clicked
  observeEvent(input$step4, {
    
    results_present<-fread(paste0(projectFolder, "/Errors/copy_folders.csv"))  # Ensure results are available
    
    # Execute the function to save folders in "Available"
    save_message <- save_study_folders(results_present, cond_dir)
    
    # Show confirmation in the console
    print(save_message)
  })
  
  process_status_av <- reactiveVal("Click 'Run Presence and Absence of Variables' to start.")
  process_completed_av <- reactiveVal(FALSE)  # Track if processing is done
  
  
  # Display results in the new tab "Detailed Logs & Reports"
  results_av <- eventReactive(input$step5, {
    Sys.sleep(3) 
    
    process_status_av("Processing started... Please wait.")
    process_completed_av(FALSE)
    available_dir<-file.path(dirname(cond_dir), "Available")
    available_folders<-list.dirs(available_dir, recursive = T)
    
    av_data <- data.table(variable_name = sapply(available_folders, function(folder) {
      folder_name <- basename(folder)
      folder_parts <- strsplit(folder_name, "_")[[1]]
      if (length(folder_parts) < 3) return(NA)
      return(paste(folder_parts[1:3], collapse = "_"))
    }, USE.NAMES = FALSE))
    
    av_data <- av_data[!is.na(variable_name)]  # Remove NAs
    
    fwrite(av_data, paste0(projectFolder, "/Errors/available_folders.csv"))
    
    process_status_av("✅ Processing Complete! Show results.")
    process_completed_av(TRUE)
    
    return(av_data)
    
  })
  
  output$results_av <- renderDT({
    datatable(results_av(), 
              options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })
  
  #### Step 9: Missing data ####
  ### Needed folders (study conditions that are missing) ###
  
  
  # observeEvent(input$step5, {
  #   # Ensure `results_av()` is available before proceeding
  #   available_dir <- file.path(dirname(cond_dir), "Available")
  #   available_folders <- list.dirs(available_dir, recursive = FALSE, full.names = T)
  #   # Extract subset names (first 3 elements of folder names)
  #   cond_data <- data.table(variable_name = sapply(available_folders, function(folder) {
  #     folder_name <- basename(folder)
  #     folder_parts <- strsplit(folder_name, "_")[[1]]
  #     if (length(folder_parts) < 3) return(NA)
  #     return(paste(folder_parts[1:3], collapse = "_"))
  #   }, USE.NAMES = FALSE))
  #   
  #   cond_data <- cond_data[!is.na(variable_name)]  # Remove NAs
  #   
  #   
  #   # Retrieve the available folders from `results_av`
  #   available_folders_f<-as.data.table(cond_data)
  #   
  #   fwrite(available_folders_f, paste0(projectFolder, "/Errors/available_folders.csv"))
  #   
  # })
  
  results_not_present <- eventReactive(input$step7, {
    Sys.sleep(3) 
    study_data<-fread(paste0(projectFolder, "/Errors/study_data.csv"))
    available_folders<-fread(paste0(projectFolder, "/Errors/available_folders.csv"))
    
    #Match against algorithms
    if (!dir.exists(alg_dir)) stop("Algorithms folder not found.")
    alg_folders <- list.dirs(alg_dir, recursive = FALSE, full.names = TRUE)
    alg_data <- data.table(variable_name = sapply(alg_folders, function(folder) {
      folder_name <- basename(folder)
      folder_parts <- strsplit(folder_name, "_")[[1]]
      if (length(folder_parts) < 3) return(NA)
      return(paste(folder_parts[1:3], collapse = "_"))
    }, USE.NAMES = FALSE))
    alg_data[,full_name:=alg_folders]
    
    
    study_data_alg<-merge.data.table(study_data,alg_data, by="variable_name", all.y = T)
    study_data_alg<-study_data_alg[study_status =="yes"]
    study_data_alg<-study_data_alg[!duplicated(variable_name)]
    if(study_data_alg[,.N]>0){
    #Add the extra algorithms found inside
    #Check if algorithms are inside algorithms
    alg_inside_alg<-study_data_alg[,full_name]
    alg_inside_names <- list()
    for (folder in alg_inside_alg) {
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
          alg_inside_names[[folder]] <- unique(sheet_data[, .(variable_name = Codesheet_name)])
        }
      }, error = function(e) { next })
    }
    alg_inside_names<-do.call(rbind,alg_inside_names) #codesheet of algorithms needed for the study
    alg_inside_names <- alg_inside_names[!(variable_name == "" | grepl("^\\s+$", variable_name) | is.na(variable_name))]
    if(alg_inside_names[,.N]>0){
    alg_inside_names<-merge.data.table(alg_inside_names,alg_data, by="variable_name", all.y = T)
    alg_inside_names[,study_status :="yes"]
    #Combine
    study_data_alg<-rbind(study_data_alg,alg_inside_names)
    }
    study_data_alg<-study_data_alg[!duplicated(variable_name)]
    study_data_alg[,remove:=1]
    study_data_alg<-study_data_alg[!is.na(variable_name)]
    study_data_alg[,study_status:=NULL]
    
    
    study_data<-merge.data.table(study_data, study_data_alg, by="variable_name", all.x = T)
    study_data[is.na(remove), remove:=0]
    # Remove rows where the specified column is empty, contains spaces, or is NA
    study_data <- study_data[!(variable_name == "" | grepl("^\\s+$", variable_name) | is.na(variable_name))]
    study_data<-study_data[remove == 0]                     
    
    study_data_alg<-study_data_alg[,full_name]
    alg_codesheet_names <- list()
    
    for (folder in study_data_alg) {
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
    alg_codesheet_names<-do.call(rbind,alg_codesheet_names) #codesheet of algorithms needed for the study
    alg_codesheet_names <- alg_codesheet_names[!(variable_name == "" | grepl("^\\s+$", variable_name) | is.na(variable_name))]
    #sheets inside the algorithms needed for the study
    alg_codesheet_names[,study_status:="yes"]
    alg_data[,alg:=1]
    alg_codesheet_names<-merge.data.table(alg_codesheet_names, alg_data, by="variable_name", all.x = T)
    alg_codesheet_names[is.na(alg), alg:=0]
    alg_codesheet_names<-alg_codesheet_names[alg==0]
    alg_codesheet_names[,full_name:=NULL][,alg:=NULL]
    }else{alg_codesheet_names<-NULL}
    study_data<-study_data[,c("variable_name", "study_status")]
    study_data<-rbind(study_data, alg_codesheet_names)
    study_data<-unique(study_data)
    
    available_folders[,present:=1]
    
    # Identify missing study variables (study_data NOT present in results_av)
    missing_study_vars <-merge(study_data, available_folders, by = "variable_name", all.x = TRUE)
    missing_study_vars[is.na(present), present:=0]
    missing_study_vars<-missing_study_vars[present==0 & study_status == "yes"]
    missing_study_vars[,study_status:=NULL][,present:=NULL]
    # **Remove empty values** (NA or empty strings)
    missing_study_vars <- missing_study_vars[!(variable_name == "" | grepl("^\\s+$", variable_name) | is.na(variable_name))]
    missing_study_vars<-unique(missing_study_vars)
    ### Step 2: Save Missing Study Variables ###
    if(nrow(missing_study_vars)==0){missing_study_vars<-data.table(variable_name="All neccessary conditions are present.")}
    return(missing_study_vars)
    
  })
  
  output$results_not_present <- renderDT({
    datatable(results_not_present(), 
              options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })
  
  #### Process data ####
  
  process_status <- reactiveVal("Click 'Run Processing' to start.")
  clean_status <- reactiveVal("Click 'Run Cleaning' after processing.")
  process_completed <- reactiveVal(FALSE)  # Track if processing is done
  
  # Disable "Run Cleaning" initially
  observe({
    shinyjs::disable("clean_files")
  })
  
  #### Processing Status Output ####
  output$process_status <- renderText({ process_status() })  
  output$process_status_2 <- renderText({ clean_status() })  
  
  #### Step 10: Process codelist ####
  observeEvent(input$process_files, {
    process_status("Processing started... Please wait.")
    process_completed(FALSE)
    
    # Define parameters
    base_dir <- paste0(projectFolder, "/Available")
    selected_columns <- c("Coding system", "Code", "Code name", "Concept", "Concept name", "Tags")
    rename_columns <- c("Coding system" = "coding_system",
                        "Code" = "code",
                        "Code name" = "code_name",
                        "Concept" = "concept",
                        "Concept name" = "concept_name",
                        "Tags" = "tags")
    
    withProgress(message = "Processing Files", value = 0, {
      result <- tryCatch({
        process_excel_files(base_dir, selected_columns, rename_columns)
      }, error = function(e) {
        showNotification(paste("❌ Processing Error:", e$message), type = "error")
        process_status("❌ Processing Failed! Check logs.")
        return(NULL)
      })
      
      req(result)  # Ensure processing was successful
      
      # Save results
      output_dir <- paste0(projectFolder, "/Untouched Codelist")
      if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      
      output_file <- file.path(output_dir, "Processed_Codelist.csv")
      fwrite(result, output_file)
      
      incProgress(1, detail = "Saving Completed")
      process_status("✅ Processing Complete! Results saved in 'Untouched Codelist'.")
      process_completed(TRUE)
      shinyjs::enable("clean_files")
    })
  })
  
  #### Step 11: Clean codelist ####
  observeEvent(input$clean_files, {
    if (!process_completed()) {
      showNotification("❌ You must run 'Processing' first!", type = "error")
      return()
    }
    
    clean_status("Cleaning started... Please wait.")
    
    base_dir <- paste0(projectFolder, "/Untouched Codelist")
    study_name <- input$string_input
    req(study_name)
    
    pedianet_extra_file <- paste0(projectFolder, "/Extra files/Pedianet_exemptions_codes.csv")
    pedianet_extra_file_2 <- paste0(projectFolder, "/Extra files/pedianet_codes_mapped.csv")
    cross_check <- paste0(projectFolder, "/Extra files/Crosscheck")
    
    withProgress(message = "Cleaning Files", value = 0, {
      result <- tryCatch({
        clean_codelist(base_dir, study_name, pedianet_extra_file,pedianet_extra_file_2, cross_check)
      }, error = function(e) {
        showNotification(paste("❌ Cleaning Error:", e$message), type = "error")
        clean_status("❌ Cleaning Failed! Check logs.")
        return(NULL)
      })
      
      req(result)
      incProgress(1, detail = "Saving Completed")
      clean_status("✅ Cleaning Complete! Results saved in 'Cleaned Codelist'.")
    })
  })
  
  #### **Define Reactive Data for Cleaning Reports** ####
  missing_values_data <- eventReactive(input$step6, {
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    missing_values_file <- file.path(cleaned_dir, "missing_values_report.csv")
    if (file.exists(missing_values_file)) fread(missing_values_file)
    else data.table(variable_name="None", result="No missing data in code, coding system, and tags.")
  })
  
  inc_data <- eventReactive(input$step6, {
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    inc_file <- file.path(cleaned_dir, "incorrect_codes.csv")
    if (file.exists(inc_file)) fread(inc_file)
    else data.table(variable_name="None", result="No incorrect codes found.")
  })
  
  range_data <- eventReactive(input$step6, {
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    range_file <- file.path(cleaned_dir, "range_codes.csv")
    if (file.exists(range_file)) fread(range_file)
    else data.table(variable_name="None", result="No ranges found.")
  })
  
  rounding_d <- eventReactive(input$step6, {
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    rounding_fl <- file.path(cleaned_dir, "error_rounding_list.csv")
    if (file.exists(rounding_fl)) fread(rounding_fl)
    else data.table(variable_name="None", result="No rounding issues found.")
  })
  
  rounding_d_p <- eventReactive(input$step6, {
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    rounding_fl_p <- file.path(cleaned_dir, "possible_error_rounding_list.csv")
    if (file.exists(rounding_fl_p)) fread(rounding_fl_p)
    else data.table(variable_name="None", result="No possible rounding issues found.")
  })
  
  #### **Display Cleaning Reports in UI** ####
  output$missing_values_report <- renderDT({ datatable(missing_values_data()) })
  output$incorrect_codes <- renderDT({ datatable(inc_data()) })
  output$range_codes <- renderDT({ datatable(range_data()) })
  output$rounding <- renderDT({ datatable(rounding_d()) })
  output$rounding_p <- renderDT({ datatable(rounding_d_p()) })
  
  
  summary_codelist <- eventReactive(input$step6, {
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    fl<-list.files(cleaned_dir,"summary.csv")
    sum_file <- file.path(cleaned_dir, fl)
    if (file.exists(sum_file)) fread(sum_file)
    else data.table(result="No summary has been produced.")
  })
  
  output$summary <- renderDT({ datatable(summary_codelist()) })
  
  tags_codelist <- eventReactive(input$step6, {
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    fl<-list.files(cleaned_dir,"tags_details.csv")
    tg_file <- file.path(cleaned_dir, fl)
    if (file.exists(tg_file)) fread(tg_file)
    else data.table(result="No tags summary has been produced.")
  })
  
  output$tags <- renderDT({ datatable(tags_codelist()) })
  
  observeEvent(input$capture, {
    shinyscreenshot::screenshot()
  })
  
  observeEvent(input$capture_2, {
    shinyscreenshot::screenshot()
  })
  
  # **Handle UI HTML Download**
  output$download_ui <- downloadHandler(
    filename = function() {
      paste("shiny_ui_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      save_html(ui, file = file)
    }
  ) 
  
  
  
  
  # # Generate and Download Report
  # output$download_report <- downloadHandler(
  #   filename = function() {
  #     paste0("Codelist_Report_", Sys.Date(), ".html")  # Name of the generated PDF
  #   },
  #   content = function(file) {
  #     rmarkdown::render(
  #       input = "report.Rmd",
  #       output_format = "html_document",
  #       output_file = file,
  #       params = list(study_name = input$string_input),
  #       envir = new.env(parent = globalenv())  # Use a fresh environment
  #     )
  #   }
  # )
  
}

shinyApp(ui, server)









