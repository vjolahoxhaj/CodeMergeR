
# Helper function to clear and recreate directories
initialize_directory <- function(dir_path) {
  if (dir.exists(dir_path)) {
    unlink(dir_path, recursive = TRUE)  # Clear directory if it exists
  }
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)  # Create directory
}

# Define paths for directories
temp_dir <- file.path(projectFolder, "temp_unzip")
cond_dir <- file.path(projectFolder, "Conceptsets")
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

source(file.path(projectFolder,"helper_functions", "check_excel_in_folder.R"))
source(file.path(projectFolder,"helper_functions", "check_folder_file_match.R"))
source(file.path(projectFolder,"helper_functions", "check_file_sheet_match.R"))
source(file.path(projectFolder,"helper_functions", "check_excel_mandatory_columns.R"))
source(file.path(projectFolder,"helper_functions", "compare_algorithm_conceptsets_names.R"))
source(file.path(projectFolder,"helper_functions", "process_study_data.R"))
source(file.path(projectFolder,"helper_functions", "not_present_study_data.R"))
source(file.path(projectFolder,"helper_functions", "save_study_folders.R"))
source(file.path(projectFolder,"helper_functions", "process_excel_files.R"))
source(file.path(projectFolder,"helper_functions", "merge_tsv_files.R"))
source(file.path(projectFolder,"helper_functions", "clean_codelist.R"))


###############################################################################
############################### UI DEFINITION #################################
###############################################################################

ui <- navbarPage(
  title = div(style="font-weight:bold; font-size:24px; color:#2c3e50;", "CodeMergeR"),
  id = "tabs",
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
          width: '700px'
        });
      });"
    )),
    tags$style(HTML("
      .tab3-wrap {
        padding-bottom: 72px;
      }
      .tab3-wrap .row {
        margin-bottom: 18px;
      }
      .tab3-wrap h3 {
        background: #dff3e4 !important;
        color: #1f4d2e !important;
        border-left: 8px solid #79bf7b;
        border-radius: 10px;
        padding: 12px 16px;
        margin-top: 14px;
        margin-bottom: 10px;
        font-weight: 700;
      }
      .tab3-wrap h4 {
        color: #1f4d2e !important;
      }
      .tab3-wrap .well {
        border: 1px solid #d7eadc;
        border-radius: 10px;
        background: #f7fcf8;
      }
      .tab3-wrap .btn {
        margin-top: 6px;
        margin-bottom: 6px;
      }
      .tab3-wrap hr {
        margin-top: 18px;
        margin-bottom: 18px;
      }
    "))
  ),
  
  tabPanel("1.0 Conformance and coherence",
           value = "tab1",
           sidebarLayout(
             sidebarPanel(
               textInput("string_input", "Study Name (as in Mother Excel):", placeholder = "Type here..."),
               fileInput("zip_file", "Upload Codelist Folder (.zip):", accept = ".zip"),
               fileInput("xlsx_file1", "Upload Mother Excel File (.xlsx):", accept = c(".xlsx")),
               actionButton("step3", tagList(icon("play-circle"), "Run Validation")),
               width = 3
             ),
             mainPanel(
               fluidPage(
                 actionButton("capture", tagList(icon("camera"), "Capture Screenshot")),
                 h4("Legend", style = "color: blue;"),
                 wellPanel(
                   p(tags$a(href = "#name_check", tagList(icon("check-circle"), "Conceptsets and Algorithms Folder Name Check"))),
                   p(tags$a(href = "#cov_no_codes", tagList(icon("check-circle"), "Conceptsets and Algorithms with No Codes Check"))),
                   p(tags$a(href = "#study_name_check", tagList(icon("check-circle"), "Study Name Presence Check"))),
                   p(tags$a(href = "#file_count_check", tagList(icon("check-circle"), "Excel File Count Check"))),
                   p(tags$a(href = "#folder_file_match", tagList(icon("check-circle"), "Folder & Excel File Name Match Check"))),
                   p(tags$a(href = "#excel_sheet_match", tagList(icon("check-circle"), "Excel Sheet Name Check"))),
                   p(tags$a(href = "#excel_col_match", tagList(icon("check-circle"), "Mandatory Columns Check"))),
                   p(tags$a(href = "#alg_cond_match", tagList(icon("check-circle"), "Conceptsets and Algorithms Match Check")))
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
               p("7. Allowed tags: narrow, possible, exclude, and ignore.")
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
                    h4(id = "name_check", tagList(icon("check-circle"), "Conceptsets Folder Name Check:"), style = "color: green;"),
                    verbatimTextOutput("conceptsetss_name_check")
             ),
             column(6, 
                    h4(tagList(icon("check-circle"), "Algorithms Folder Name Check:"), style = "color: green;"),
                    verbatimTextOutput("algorithms_name_check")
             )
           ),
           tags$hr(),
           fluidRow(
             column(6, 
                    h4(id = "cov_no_codes", tagList(icon("check-circle"), "Covariates with No Codes Check:"), style = "color: green;"),
                    DTOutput("cov_no_codes_cond")
             ),
             column(6, 
                    h4(tagList(icon("check-circle"), "Algorithms with No Codes Check:"), style = "color: green;"),
                    DTOutput("cov_no_codes_alg")
             )
           ),
           tags$hr(),
           fluidRow(
             column(6, 
                    h4(id = "study_name_check", tagList(icon("check-circle"), "Study Name Presence Check:"), style = "color: green;"),
                    verbatimTextOutput("study_name_check")
             )
           ),
           tags$hr(),
           fluidRow(
             column(6, 
                    h4(id = "file_count_check", tagList(icon("check-circle"), "Conceptsets Excel File Check:"), style = "color: green;"),
                    DTOutput("step3_results_cond")
             ),
             column(6, 
                    h4(tagList(icon("check-circle"), "Algorithms Excel File Check:"), style = "color: green;"),
                    DTOutput("step3_results_alg")
             )
           ),
           tags$hr(),
           fluidRow(
             column(6, 
                    h4(id = "folder_file_match", tagList(icon("check-circle"), "Conceptsets Folder & Excel File Name Match Check:"), style = "color: green;"),
                    DTOutput("step3_results_name_cond")
             ),
             column(6, 
                    h4(tagList(icon("check-circle"), "Algorithms Folder & Excel File Name Match Check:"), style = "color: green;"),
                    DTOutput("step3_results_name_alg")
             )
           ),
           tags$hr(),
           fluidRow(
             column(6, 
                    h4(id = "excel_sheet_match", tagList(icon("check-circle"), "Conceptsets Excel Sheet Name Check:"), style = "color: green;"),
                    DTOutput("step3_results_sheet_cond")
             ),
             column(6, 
                    h4(tagList(icon("check-circle"), "Algorithms Excel Sheet Name Check:"), style = "color: green;"),
                    DTOutput("step3_results_sheet_alg")
             )
           ),
           tags$hr(),
           fluidRow(
             column(6, 
                    h4(id = "excel_col_match", tagList(icon("check-circle"), "Conceptsets Mandatory Columns Check:"), style = "color: green;"),
                    DTOutput("results_col_cond")
             ),
             column(6, 
                    h4(tagList(icon("check-circle"), "Algorithms Mandatory Columns Check:"), style = "color: green;"),
                    DTOutput("results_col_alg")
             )
           ),
           tags$hr(),
           fluidRow(
             column(6, 
                    h4(id = "alg_cond_match", tagList(icon("check-circle"), "Conceptsets and Algorithms Match Check:"), style = "color: green;"),
                    DTOutput("results_alg_cond")
             )
           )
  ),
  
  # Second Tab
  tabPanel("1.1 Cleaning and standardization",
           value = "tab2",
           
           fluidPage(
             h4("Legend", style = "color: blue;"),
             wellPanel(
               p(tags$a(href = "#av_data", tagList(icon("check-circle"), "Available Conceptsets"))),
               p(tags$a(href = "#not_av_data", tagList(icon("times-circle"), "Missing necessary conceptsets/algorithms for the study"))),
               p(tags$a(href = "#issues", tagList(icon("times-circle"), "Issues with codes: ranges, scientific notations, rounding"))),
               p(tags$a(href = "#summary", tagList(icon("check-circle"), "Codelist summary"))),
               p(tags$a(href = "#tags", tagList(icon("check-circle"), "Tags summary")))
             ),
             
             tags$hr(),
             
             actionButton("capture_2", tagList(icon("camera"), "Capture Screenshot")),
             
             tags$hr(),
             
             h3("Validation Logs & Reports", style = "color: blue;"),
             actionButton("step4", tagList(icon("play-circle"), "Run Presence and Absence of Variables")),
             actionButton("step5", tagList(icon("play-circle"), "Show Presence of Variables")),
             actionButton("step7", tagList(icon("play-circle"), "Show Absence of Variables")),
             
             tags$hr(),
             
             fluidRow(
               column(6, 
                      h4(id="av_data", tagList(icon("check-circle"), "Study Conceptsets Available:"), style = "color: green;"),
                      DTOutput("results_av")
               ),
               column(6, 
                      h4(id="not_av_data", tagList(icon("times-circle"), "Study Conceptsets Missing:"), style = "color: red;"),
                      DTOutput("results_not_present")
               )
             ),
             
             tags$hr(),
             
             h3("Process Excel Files", style = "color: blue;"),
             actionButton("process_files", tagList(icon("play-circle"), "Run Processing")),
             tags$hr(),
             verbatimTextOutput("process_status"),
             
             tags$hr(),
             
             h3("Final Codelist Cleaning", style = "color: blue;"),
             actionButton("clean_files", tagList(icon("play-circle"), "Run Cleaning")),
             tags$hr(),
             actionButton("step6", tagList(icon("play-circle"), "Show Results")),
             
             tags$hr(),
             
             fluidRow(
               column(6, 
                      h4(id="issues", tagList(icon("times-circle"), "Codes containing scientific notation (E+):"), style = "color: red;"),
                      DTOutput("incorrect_codes")
               ),
               column(6, 
                      h4(tagList(icon("times-circle"), "Codes containing ranges:"), style = "color: red;"),
                      DTOutput("range_codes")
               )
             ),
             
             tags$hr(),
             
             fluidRow(
               column(6, 
                      h4(tagList(icon("times-circle"), "Missing Data:"), style = "color: red;"),
                      DTOutput("missing_values_report")
               )
             ),
             
             tags$hr(),
             
             fluidRow(
               column(6, 
                      h4(tagList(icon("times-circle"), "Rounding Issues:"), style = "color: red;"),
                      DTOutput("rounding")
               ),
               column(6, 
                      h4(tagList(icon("exclamation-triangle"), "Possible Rounding Issues:"), style = "color: orange;"),
                      DTOutput("rounding_p")
               )
             ),
             
             tags$hr(),
             
             verbatimTextOutput("process_status_2"),
             
             tags$hr(),
             
             h3("Codelist Summary", style = "color: blue;"),
             
             fluidRow(
               column(12, 
                      h4(id="summary", tagList(icon("check-circle"), "Codelist Summary:"), style = "color: green;"),
                      DTOutput("summary")
               )
             ),
             
             tags$hr(),
             
             fluidRow(
               column(12, 
                      h4(id="tags", tagList(icon("check-circle"), "Tags Summary:"), style = "color: green;"),
                      DTOutput("tags")
               )
             )
           )
  ),
  
  tabPanel(
    "2.0 Codelist validation",
    value = "tab3",
    div(class = "tab3-wrap",
        sidebarLayout(
          sidebarPanel(
            fileInput("csv_file_tab3", "Upload the codelist of interest", accept = ".csv"),
            h4("Uploaded File Details:", style = "color: blue;"),
            tableOutput("file_details_tab3"),
            h4("Date of Execution:", style = "color: blue;"),
            textOutput("run_date_tab3"),
            h4("Export File Name:", style = "color: blue;"),
            textInput("export_filename_tab3", "Name of export file", value = "verified_codelist"),
            width = 3
          ),
          mainPanel(
            fluidPage(
              h4("Legend", style = "color: blue;"),
              wellPanel(
                p("Upload a CSV file, keep only the columns you need, rename them if required, assign the key columns, and then save the verification file.")
              )
            )
          )
        ),
        tags$hr(),
        fluidRow(
          h3("Column selection", style = "color: blue;"),
          uiOutput("tab3_column_selector")
        ),
        tags$hr(),
        fluidRow(
          h3("Column renaming", style = "color: blue;"),
          uiOutput("tab3_rename_ui")
        ),
        tags$hr(),
        actionButton("save_verified_codelist", tagList(icon("save"), "Save verified codelist")),
        tags$hr(),
        fluidRow(
          h3("Harmonizing vocabularies", style = "color: blue;"),
          uiOutput("harmonize_vocabularies_ui")
        ),
        actionButton("apply_vocab_relabelling", tagList(icon("play-circle"), "Apply relabelling")),
        tags$hr(),
        fluidRow(
          h4("Vocabulary relabelling summary:", style = "color: blue;"),
          DTOutput("vocab_relabelling_summary_table")
        ),
        fluidRow(
          h3("Corrections", style = "color: blue;"),
          fluidRow(
            column(
              8,
              h4("Create ICD9PCS codes?", style = "color: blue;"),
              tags$p(
                tags$em("The coding system ICD9CM/ICD9 is searched for procedure codes. All codes that have 2 digits followed by a dot, or that have a total of 2 characters will be relabelled as ICD9PCS.")
              )
            ),
            column(
              4,
              radioButtons(
                "create_icd9pcs_codes",
                NULL,
                choices = c("Yes", "No"),
                selected = "No",
                inline = TRUE
              )
            )
          )
        ),
        actionButton("apply_icd9pcs_correction", tagList(icon("play-circle"), "Apply")),
        tags$hr(),
        fluidRow(
          h4("ICD9PCS relabelled codes:", style = "color: blue;"),
          DTOutput("icd9pcs_relabelled_table")
        ),
        tags$hr(),
        fluidRow(
          h3("Replicate vocabulary", style = "color: blue;"),
          uiOutput("replicate_vocabulary_ui")
        ),
        actionButton("add_replication_row", tagList(icon("plus"), "Add extra box")),
        actionButton("apply_vocab_replication", tagList(icon("play-circle"), "Apply replication")),
        tags$hr(),
        fluidRow(
          h4("Vocabulary replication summary:", style = "color: blue;"),
          DTOutput("vocab_replication_summary_table")
        ),
        tags$hr(),
        fluidRow(
          h3("Cleaning", style = "color: blue;"),
          tags$p(tags$em('During this check all rows where the code column is empty, "", or equal to "-" will be dropped. Rows where the coding system column, tags column, or variable column are empty will also be dropped.')),
          actionButton("apply_tab3_cleaning", tagList(icon("broom"), "Apply cleaning")),
          DTOutput("tab3_cleaning_summary")
        ),
        tags$hr(),
        fluidRow(
          h3("Rounding issues", style = "color: blue;"),
          tags$p(tags$em("Checks codes ending in 000 for the selected vocabularies only.")),
          uiOutput("rounding_vocab_selector_ui")
        ),
        fluidRow(
          column(12,
                 actionButton("add_rounding_vocab", tagList(icon("plus"), "Add extra box")),
                 actionButton("check_rounding_issues", tagList(icon("search"), "Check for rounding issues"))
          )
        ),
        fluidRow(
          column(12,
                 DTOutput("rounding_issues_tab3"),
                 radioButtons("drop_rounding_issues", "Drop issues", choices = c("No", "Yes"), selected = "No", inline = TRUE),
                 actionButton("apply_drop_rounding_issues", tagList(icon("trash"), "Apply drop")),
                 DTOutput("rounding_drop_summary_tab3")
          )
        ),
        tags$hr(),
        fluidRow(
          h3("Possible rounding issues", style = "color: blue;"),
          tags$p(tags$em("Checks codes ending in 00 but not 000 for the selected vocabularies only.")),
          uiOutput("possible_rounding_vocab_selector_ui")
        ),
        fluidRow(
          column(12,
                 actionButton("add_possible_rounding_vocab", tagList(icon("plus"), "Add extra box")),
                 actionButton("check_possible_rounding_issues", tagList(icon("search"), "Check for possible rounding issues"))
          )
        ),
        fluidRow(
          column(12,
                 DTOutput("possible_rounding_issues_tab3"),
                 radioButtons("drop_possible_rounding_issues", "Drop issues", choices = c("No", "Yes"), selected = "No", inline = TRUE),
                 actionButton("apply_drop_possible_rounding_issues", tagList(icon("trash"), "Apply drop")),
                 DTOutput("possible_rounding_drop_summary_tab3")
          )
        ),
        tags$hr(),
        fluidRow(
          h3("Scientific notation identification", style = "color: blue;"),
          tags$p(tags$em('Checks the code column for values containing the pattern "E+".'))
        ),
        fluidRow(
          column(12,
                 DTOutput("scientific_notation_issues_tab3"),
                 radioButtons("drop_scientific_notation_issues", "Drop issues", choices = c("No", "Yes"), selected = "No", inline = TRUE),
                 actionButton("apply_drop_scientific_notation_issues", tagList(icon("trash"), "Apply drop")),
                 DTOutput("scientific_notation_drop_summary_tab3")
          )
        ),
        tags$hr(),
        fluidRow(
          h3("Tags harmonisation", style = "color: blue;"),
          uiOutput("tags_harmonisation_ui")
        ),
        fluidRow(
          column(
            12,
            actionButton(
              "apply_tags_relabelling",
              tagList(icon("play-circle"), "Apply relabelling")
            )
          )
        ),
        tags$hr(),
        fluidRow(
          column(
            12,
            h4("Tags relabelling summary:", style = "color: blue;"),
            DTOutput("tags_relabelling_summary_table")
          )
        ),
        tags$hr(),
        fluidRow(
          h4("Drop tags:", style = "color: blue;"),
          uiOutput("tags_drop_ui")
        ),
        fluidRow(
          column(
            12,
            actionButton(
              "apply_drop_tags",
              tagList(icon("trash"), "Apply drop")
            )
          )
        ),
        tags$hr(),
        fluidRow(
          column(
            12,
            h4("Dropped tags summary:", style = "color: blue;"),
            DTOutput("tags_drop_summary_table")
          )
        ),
        tags$hr(),
        fluidRow(
          h3("Included tags summary", style = "color: blue;"),
          column(12,
                 h4("Included tags overall summary by coding system and variable:", style = "color: blue;"),
                 uiOutput("included_tags_summary_filters_ui"),
                 actionButton("deduplicate_final_codelist_tab3", tagList(icon("clone"), "Deduplicate")),
                 downloadButton("download_final_codelist_tab3", "Download final codelist (.csv)"),
                 br(), br(),
                 DTOutput("included_tags_summary_table"),
                 br(),
                 DTOutput("deduplicate_summary_table")
          )
        ),
        tags$div(style = "height: 40px;")
    ),
  ),
  
  tabPanel(
    "3.0 Create case definition",
    value = "tab4",
    fluidPage(
      h3("Create case definition", style = "color: blue;"),
      wellPanel(
        p("This tab is reserved for the case definition workflow.")
      )
    )
  )
)  

###############################################################################
############################### SERVER ########################################
###############################################################################

server <- function(input, output, session) {
  
  #### ---------- Pop up window for tab 1.0 ---------- ####
  observeEvent(input$tabs, {
    if (input$tabs == "tab1") {
      session$sendCustomMessage("alertMessage", list(
        title = "Welcome to the Conformance and Coherence Tab!",
        html = "<div style='text-align: left; font-size: 13px;'>
                <p><strong>Instructions:</strong></p>
                <ul>
                  <li><i class='fa fa-folder-open'></i> <b>Upload</b> each file and <b>wait</b> until upload is complete.</li>
                  <li><i class='fa fa-play-circle'></i> <b>Press Run Validation</b> and <b>scroll</b> to the end of the page.</li>
                  <li><i class='fa fa-hourglass-half'></i> <b>Wait</b> until <b>all results</b> are displayed.</li>
                  <li><i class='fa fa-camera'></i> <b>Press Capture Screenshot</b> at the top.</li>
                  <li><i class='fa fa-save'></i> <b>Save the screenshot</b> inside the <b>Cleaned Codelist</b> folder.</li>
                </ul>
              </div>",
        icon = "info"
      ))
      #### ---------- Pop up window for tab 1.1 ---------- ####
    } else if (input$tabs == "tab2") {
      session$sendCustomMessage("alertMessage", list(
        title = "Welcome to the Cleaning and Standardization Tab!",
        html = "<div style='text-align: left; font-size: 13px;'>
                <p><strong>Instructions:</strong></p>
                <ul>
                  <li><i class='fa fa-play-circle'></i> <b>Press Run Presence and Absence of Variables</b>.</li>
                  <li><i class='fa fa-hourglass-half'></i> <b>Wait 10 seconds</b>.</li>
                  <li><i class='fa fa-play-circle'></i> <b>Press Show Presence of Variables</b>.</li>
                  <li><i class='fa fa-play-circle'></i> <b>Press Show Absence of Variables</b>.</li>
                  <li><i class='fa fa-play-circle'></i> <b>Press Run Processing</b>.</li>
                  <li><i class='fa fa-hourglass-half'></i> <b>Wait until Processing Complete appears</b>.</li>
                  <li><i class='fa fa-play-circle'></i> <b>Press Run Cleaning</b>.</li>
                  <li><i class='fa fa-hourglass-half'></i> <b>Wait until Cleaning Complete appears</b>.</li>
                  <li><i class='fa fa-play-circle'></i> <b>Press Show Results</b>.</li>
                  <li><i class='fa fa-camera'></i> <b>Press Capture Screenshot</b> at the top.</li>
                  <li><i class='fa fa-save'></i> <b>Save the screenshot</b> inside the <b>Cleaned Codelist</b> folder.</li>
                </ul>
              </div>",
        icon = "info"
      ))
    }
  })
  
  
  #### ---------- tab 2.0 description ---------- ####

  tab3_run_date <- reactiveVal()
  tab3_uploaded_file_name <- reactiveVal("")
  tab3_data <- reactiveVal(NULL)
  
  observeEvent(input$csv_file_tab3, {
    req(input$csv_file_tab3)
    
    csv_data <- tryCatch({
      as.data.table(fread(input$csv_file_tab3$datapath))
    }, error = function(e) {
      showNotification("Unable to read the uploaded CSV file.", type = "error")
      NULL
    })
    tab3_data(csv_data)
    tab3_run_date(format(Sys.Date(), "%Y-%m-%d"))
    tab3_uploaded_file_name(as.character(input$csv_file_tab3$name))
  })
  
  
  output$run_date_tab3 <- renderText({
    tab3_run_date()
  })
  
  output$file_details_tab3 <- renderTable({
    file_name <- tab3_uploaded_file_name()
    if (is.null(file_name) || !nzchar(file_name)) {
      return(data.frame(File_Type = character(), File_Name = character(), stringsAsFactors = FALSE))
    }
    data.frame(
      File_Type = "CSV File",
      File_Name = file_name,
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)
  

  #### ---------- tab2.0: column availability and selection ---------- ####
  available_columns_tab3 <- reactive({
    dt <- tab3_data()
    if (is.null(dt)) return(character())
    names(dt)
  })
  
  output$tab3_column_selector <- renderUI({
    cols <- available_columns_tab3()
    if (length(cols) == 0) {
      return(wellPanel(p("Upload a CSV file to display available columns.")))
    }
    tagList(
      div(
        style = "border: 1px solid #ddd; border-radius: 6px; padding: 12px;",
        fluidRow(
          column(9, strong("Column name")),
          column(3, strong("Include"))
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 8px;"),
        lapply(seq_along(cols), function(i) {
          fluidRow(
            column(9, div(style = "padding-top: 6px;", cols[i])),
            column(3, checkboxInput(paste0("include_col_tab3_", i), NULL, value = TRUE))
          )
        })
      )
    )
  })
  
  selected_columns_tab3 <- reactive({
    cols <- available_columns_tab3()
    if (length(cols) == 0) return(character())
    keep <- vapply(seq_along(cols), function(i) {
      val <- input[[paste0("include_col_tab3_", i)]]
      if (is.null(val)) TRUE else isTRUE(val)
    }, logical(1))
    cols[keep]
  })
  
  #### ---------- tab 2.0: column relabelling, store also teh name of thre relabelled columns, also select key variables ---------- ####
  output$tab3_rename_ui <- renderUI({
    cols <- selected_columns_tab3()
    if (length(cols) == 0) {
      return(wellPanel(p("Untick fewer columns to enable renaming and role selection.")))
    }
    tagList(
      div(
        style = "border: 1px solid #ddd; border-radius: 6px; padding: 12px; overflow-x: auto;",
        fluidRow(
          column(3, strong("Original column")),
          column(1, strong("Rename")),
          column(3, strong("New column name")),
          column(1, strong("code_column")),
          column(1, strong("coding_system_column")),
          column(1, strong("tags_column")),
          column(1, strong("variable_column"))
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 8px;"),
        lapply(seq_along(cols), function(i) {
          fluidRow(
            column(3, div(style = "padding-top: 6px;", cols[i])),
            column(1, selectInput(paste0("rename_choice_tab3_", i), NULL, choices = c("No", "Yes"), selected = "No", width = "100%")),
            column(3, textInput(paste0("new_col_tab3_", i), NULL, value = cols[i], width = "100%")),
            column(1, checkboxInput(paste0("code_column_tab3_", i), NULL, value = FALSE)),
            column(1, checkboxInput(paste0("coding_system_column_tab3_", i), NULL, value = FALSE)),
            column(1, checkboxInput(paste0("tags_column_tab3_", i), NULL, value = FALSE)),
            column(1, checkboxInput(paste0("variable_column_tab3_", i), NULL, value = FALSE))
          )
        })
      )
    )
  })
  
  observe({
    cols <- selected_columns_tab3()
    if (length(cols) == 0) return()
    for (i in seq_along(cols)) {
      rename_choice <- input[[paste0("rename_choice_tab3_", i)]]
      input_id <- paste0("new_col_tab3_", i)
      if (is.null(rename_choice)) next
      if (identical(rename_choice, "Yes")) {
        shinyjs::enable(input_id)
      } else {
        shinyjs::disable(input_id)
        updateTextInput(session, input_id, value = cols[i])
      }
    }
  })
  
  observe({
    cols <- selected_columns_tab3()
    if (length(cols) == 0) return()
    role_prefixes <- c("code_column_tab3_", "coding_system_column_tab3_", "tags_column_tab3_", "variable_column_tab3_")
    for (prefix in role_prefixes) {
      selected_idx <- which(vapply(seq_along(cols), function(i) {
        isTRUE(input[[paste0(prefix, i)]])
      }, logical(1)))
      if (length(selected_idx) > 1) {
        keep_idx <- selected_idx[length(selected_idx)]
        for (idx in setdiff(selected_idx, keep_idx)) {
          updateCheckboxInput(session, paste0(prefix, idx), value = FALSE)
        }
      }
    }
  })
  
  #### ---------- tab 2.0: working objects used by the checks ---------- ####
  
  icd9pcs_relabelled_data <- reactiveVal(data.table())
  vocab_relabelling_summary_data <- reactiveVal(data.table())
  vocab_replication_summary_data <- reactiveVal(data.table())
  tags_relabelling_summary_data <- reactiveVal(data.table())
  replication_row_ids <- reactiveVal(1L)
  registered_replication_delete_ids <- reactiveVal(integer())
  cleaning_summary_data <- reactiveVal(data.table())
  rounding_vocab_ids <- reactiveVal(1L)
  possible_rounding_vocab_ids <- reactiveVal(1L)
  rounding_issues_data <- reactiveVal(data.table())
  possible_rounding_issues_data <- reactiveVal(data.table())
  scientific_notation_issues_data <- reactiveVal(data.table())
  rounding_drop_summary <- reactiveVal(data.table())
  possible_rounding_drop_summary <- reactiveVal(data.table())
  scientific_notation_drop_summary <- reactiveVal(data.table())
  deduplicate_summary_data <- reactiveVal(data.table())
  tags_drop_summary_data <- reactiveVal(data.table())
  
  ###############################################################################
  ######################## TAB 2.0 - HELPER FUNCTIONS ###########################
  ###############################################################################
  tab3_saved_state <- reactiveVal(0L)
  last_saved_export_file <- reactiveVal(NULL)
  
  get_tab3_saved_info <- reactive({
    tab3_saved_state()
    verification_dir <- file.path(projectFolder, "2_Verification")
    export_name <- trimws(input$export_filename_tab3)
    if (!nzchar(export_name)) export_name <- "verified_codelist"
    requested_codelist_file <- file.path(verification_dir, paste0(export_name, ".csv"))
    fallback_codelist_file <- last_saved_export_file()
    codelist_file <- if (file.exists(requested_codelist_file)) {
      requested_codelist_file
    } else if (!is.null(fallback_codelist_file) && file.exists(fallback_codelist_file)) {
      fallback_codelist_file
    } else {
      requested_codelist_file
    }
    roles_file <- file.path(verification_dir, "column_roles_mapping.csv")
    if (!file.exists(codelist_file) || !file.exists(roles_file)) return(NULL)
    
    role_mapping <- tryCatch({
      fread(roles_file, colClasses = "character")
    }, error = function(e) {
      NULL
    })
    if (is.null(role_mapping) || nrow(role_mapping) == 0) return(NULL)
    
    coding_system_col <- role_mapping[role == "coding_system_column", selected_column]
    if (length(coding_system_col) != 1) return(NULL)
    
    codelist_dt <- tryCatch({
      fread(codelist_file, colClasses = "character")
    }, error = function(e) {
      NULL
    })
    if (is.null(codelist_dt) || !(coding_system_col %in% names(codelist_dt))) return(NULL)
    
    list(
      verification_dir = verification_dir,
      codelist_file = codelist_file,
      codelist_dt = codelist_dt,
      role_mapping = role_mapping,
      coding_system_col = coding_system_col
    )
  })
  
  replication_vocab_choices <- reactive({
    saved_info <- get_tab3_saved_info()
    if (is.null(saved_info)) return(character())
    coding_values <- trimws(as.character(saved_info$codelist_dt[[saved_info$coding_system_col]]))
    coding_values <- coding_values[!is.na(coding_values) & nzchar(coding_values)]
    unique(coding_values)
  })
  
  # Small helper so the later checks all read from the same saved file
  get_tab3_saved_context <- reactive({
    saved_info <- get_tab3_saved_info()
    if (is.null(saved_info)) return(NULL)
    list(
      verification_dir = saved_info$verification_dir,
      export_file = saved_info$codelist_file,
      dt = saved_info$codelist_dt,
      code_col = saved_info$role_mapping[role == "code_column", selected_column][1],
      coding_col = saved_info$role_mapping[role == "coding_system_column", selected_column][1],
      tags_col = saved_info$role_mapping[role == "tags_column", selected_column][1],
      variable_col = saved_info$role_mapping[role == "variable_column", selected_column][1]
    )
  })
  
  get_tab3_vocab_choices <- reactive({
    replication_vocab_choices()
  })
  
  #### ---------- tab 2.0: replicate vocabularies ---------- ####
  output$replicate_vocabulary_ui <- renderUI({
    vocab_choices <- replication_vocab_choices()
    row_ids <- replication_row_ids()
    if (length(vocab_choices) == 0) {
      return(wellPanel(p("Save verified codelist to load the available vocabularies for replication.")))
    }
    tagList(
      div(
        style = "border: 1px solid #ddd; border-radius: 6px; padding: 12px; overflow-x: auto;",
        fluidRow(
          column(4, strong("From")),
          column(5, strong("To")),
          column(3, strong("Delete"))
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 8px;"),
        lapply(seq_along(row_ids), function(pos) {
          row_id <- row_ids[pos]
          fluidRow(
            column(
              4,
              selectizeInput(
                paste0("replicate_from_", row_id),
                NULL,
                choices = vocab_choices,
                selected = if (length(vocab_choices) > 0) vocab_choices[1] else NULL,
                options = list(placeholder = "Select vocabulary"),
                width = "100%"
              )
            ),
            column(5, textInput(paste0("replicate_to_", row_id), NULL, value = "", width = "100%")),
            column(
              3,
              if (pos == 1) {
                div(style = "padding-top: 8px;", "")
              } else {
                actionButton(paste0("delete_replication_row_", row_id), NULL, icon = icon("trash"))
              }
            )
          )
        })
      )
    )
  })
  
  observeEvent(input$add_replication_row, {
    current_ids <- replication_row_ids()
    next_id <- if (length(current_ids) == 0) 1L else max(current_ids) + 1L
    replication_row_ids(c(current_ids, next_id))
  })
  
  observe({
    current_ids <- replication_row_ids()
    registered_ids <- registered_replication_delete_ids()
    ids_to_register <- setdiff(current_ids, c(registered_ids, current_ids[1]))
    if (length(ids_to_register) == 0) return()
    for (row_id in ids_to_register) {
      local({
        local_row_id <- row_id
        observeEvent(input[[paste0("delete_replication_row_", local_row_id)]], {
          replication_row_ids(setdiff(replication_row_ids(), local_row_id))
        }, ignoreInit = TRUE)
      })
    }
    registered_replication_delete_ids(unique(c(registered_ids, ids_to_register)))
  })
  output$harmonize_vocabularies_ui <- renderUI({
    saved_info <- get_tab3_saved_info()
    if (is.null(saved_info)) {
      return(wellPanel(p("Save verified codelist to load the available vocabularies.")))
    }
    
    coding_values <- trimws(as.character(saved_info$codelist_dt[[saved_info$coding_system_col]]))
    coding_values <- coding_values[!is.na(coding_values) & nzchar(coding_values)]
    unique_values <- unique(coding_values)
    
    if (length(unique_values) == 0) {
      return(wellPanel(p("No vocabulary values were found in the selected coding system column.")))
    }
    
    tagList(
      div(
        style = "border: 1px solid #ddd; border-radius: 6px; padding: 12px; overflow-x: auto;",
        fluidRow(
          column(4, strong("Vocabulary")),
          column(3, strong("Relabel?")),
          column(5, strong("Relabel to"))
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 8px;"),
        lapply(seq_along(unique_values), function(i) {
          fluidRow(
            column(4, div(style = "padding-top: 6px;", unique_values[i])),
            column(3, radioButtons(paste0("harmonize_vocab_choice_", i), NULL, choices = c("No", "Yes"), selected = "No", inline = TRUE)),
            column(5, textInput(paste0("harmonize_vocab_value_", i), NULL, value = unique_values[i], width = "100%"))
          )
        })
      )
    )
  })
  
  observe({
    saved_info <- get_tab3_saved_info()
    if (is.null(saved_info)) return()
    
    coding_values <- trimws(as.character(saved_info$codelist_dt[[saved_info$coding_system_col]]))
    coding_values <- coding_values[!is.na(coding_values) & nzchar(coding_values)]
    unique_values <- unique(coding_values)
    if (length(unique_values) == 0) return()
    
    for (i in seq_along(unique_values)) {
      choice_id <- paste0("harmonize_vocab_choice_", i)
      value_id <- paste0("harmonize_vocab_value_", i)
      current_choice <- input[[choice_id]]
      if (is.null(current_choice)) next
      
      if (identical(current_choice, "Yes")) {
        shinyjs::enable(value_id)
      } else {
        shinyjs::disable(value_id)
        updateTextInput(session, value_id, value = unique_values[i])
      }
    }
  })
  
  observeEvent(input$apply_vocab_relabelling, {
    saved_info <- get_tab3_saved_info()
    if (is.null(saved_info)) {
      showNotification("Save verified codelist before applying vocabulary relabelling.", type = "error")
      return()
    }
    
    coding_values <- trimws(as.character(saved_info$codelist_dt[[saved_info$coding_system_col]]))
    coding_values <- coding_values[!is.na(coding_values) & nzchar(coding_values)]
    unique_values <- unique(coding_values)
    
    relabel_list <- lapply(seq_along(unique_values), function(i) {
      choice_value <- input[[paste0("harmonize_vocab_choice_", i)]]
      if (!identical(choice_value, "Yes")) return(NULL)
      new_value <- trimws(as.character(input[[paste0("harmonize_vocab_value_", i)]]))
      if (!nzchar(new_value)) {
        return(data.table(.invalid = TRUE))
      }
      data.table(from_vocabulary = unique_values[i], to_vocabulary = new_value)
    })
    relabel_map <- rbindlist(relabel_list, fill = TRUE)
    
    if (nrow(relabel_map) > 0 && ".invalid" %in% names(relabel_map)) {
      showNotification("Each selected relabelling must have a target vocabulary value.", type = "error")
      return()
    }
    
    if (nrow(relabel_map) == 0) {
      showNotification("No vocabulary relabelling was selected.", type = "message")
      return()
    }
    
    relabel_map <- unique(relabel_map, by = "from_vocabulary")
    
    previous_values <- as.character(saved_info$codelist_dt[[saved_info$coding_system_col]])
    matched_rows <- !is.na(previous_values) & previous_values %in% relabel_map$from_vocabulary
    new_values <- previous_values
    
    for (i in seq_len(nrow(relabel_map))) {
      new_values[previous_values == relabel_map$from_vocabulary[i]] <- relabel_map$to_vocabulary[i]
    }
    
    saved_info$codelist_dt[, (saved_info$coding_system_col) := new_values]
    
    summary_dt <- data.table(
      from_vocabulary = previous_values[matched_rows],
      to_vocabulary = new_values[matched_rows]
    )[, .N, by = .(from_vocabulary, to_vocabulary)]
    setnames(summary_dt, "N", "n_rows_relabelled")
    
    fwrite(saved_info$codelist_dt, saved_info$codelist_file)
    fwrite(relabel_map, file.path(saved_info$verification_dir, "vocabulary_relabelling_mapping.csv"))
    fwrite(summary_dt, file.path(saved_info$verification_dir, "vocabulary_relabelling_summary.csv"))
    
    vocab_relabelling_summary_data(summary_dt)
    tab3_saved_state(tab3_saved_state() + 1L)
    
    showNotification(paste0(sum(summary_dt$n_rows_relabelled), " rows were relabelled across vocabularies."), type = "message")
  })
  
  output$vocab_relabelling_summary_table <- renderDT({
    summary_dt <- vocab_relabelling_summary_data()
    if (is.null(summary_dt) || nrow(summary_dt) == 0) {
      return(datatable(
        data.table(Result = "No vocabulary relabelling has been applied yet."),
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    datatable(summary_dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  #### ---------- tab 2.0: included tags summary ---------- ####
  included_tags_summary_tab3 <- reactive({
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      return(data.table(Result = "Save the verified codelist first."))
    }
    required_cols <- c(ctx$coding_col, ctx$tags_col, ctx$variable_col)
    if (any(is.na(required_cols)) || !all(required_cols %in% names(ctx$dt))) {
      return(data.table(Result = "Mapped columns were not found in the verified codelist."))
    }
    
    dt <- copy(ctx$dt)
    dt[, coding_value := trimws(as.character(get(ctx$coding_col)))]
    dt[, tags_value := tolower(trimws(as.character(get(ctx$tags_col))))]
    dt[, variable_value := trimws(as.character(get(ctx$variable_col)))]
    
    dt <- dt[
      !is.na(coding_value) & coding_value != "" &
        !is.na(tags_value) & tags_value != "" &
        !is.na(variable_value) & variable_value != ""
    ]
    
    dt <- dt[tags_value %in% c("narrow", "possible")]
    
    if (nrow(dt) == 0) {
      return(data.table(Result = "No included tags found."))
    }
    
    summary_dt <- dt[, {
      unique_tags <- unique(tags_value)
      ordered_tags <- c("narrow", "possible")[c("narrow", "possible") %in% unique_tags]
      .(
        tags = paste(ordered_tags, collapse = ", "),
        total_rows = .N
      )
    }, by = .(
      coding_system = coding_value,
      variable = variable_value
    )]
    
    setorder(summary_dt, coding_system, variable)
    summary_dt
  })
  
  output$included_tags_summary_filters_ui <- renderUI({
    dt <- included_tags_summary_tab3()
    if ("Result" %in% names(dt) || nrow(dt) == 0) {
      return(NULL)
    }
    fluidRow(
      column(
        4,
        selectizeInput(
          "included_tags_filter_coding_system",
          "Coding system",
          choices = sort(unique(dt$coding_system)),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "All")
        )
      ),
      column(
        4,
        selectizeInput(
          "included_tags_filter_variable",
          "Variable",
          choices = sort(unique(dt$variable)),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "All")
        )
      ),
      column(
        4,
        selectizeInput(
          "included_tags_filter_tags",
          "Tags",
          choices = sort(unique(dt$tags)),
          selected = NULL,
          multiple = TRUE,
          options = list(placeholder = "All")
        )
      )
    )
  })
  
  included_tags_summary_filtered_tab3 <- reactive({
    dt <- copy(included_tags_summary_tab3())
    if ("Result" %in% names(dt) || nrow(dt) == 0) {
      return(dt)
    }
    
    selected_coding <- input$included_tags_filter_coding_system
    selected_variable <- input$included_tags_filter_variable
    selected_tags <- input$included_tags_filter_tags
    
    if (!is.null(selected_coding) && length(selected_coding) > 0) {
      dt <- dt[coding_system %in% selected_coding]
    }
    if (!is.null(selected_variable) && length(selected_variable) > 0) {
      dt <- dt[variable %in% selected_variable]
    }
    if (!is.null(selected_tags) && length(selected_tags) > 0) {
      dt <- dt[tags %in% selected_tags]
    }
    
    dt
  })
  
  output$included_tags_summary_table <- renderDT({
    dt <- included_tags_summary_filtered_tab3()
    if ("Result" %in% names(dt)) {
      return(datatable(
        dt,
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    
    datatable(
      dt,
      options = list(scrollX = TRUE, pageLength = 10)
    ) %>%
      formatStyle(
        columns = names(dt),
        target = "row",
        valueColumns = "tags",
        backgroundColor = styleEqual("possible", "#f8d7da")
      )
  })
  
  #### ---------- tab 2.0: deduplicate final codelist ---------- ####
  observeEvent(input$deduplicate_final_codelist_tab3, {
    ctx <- get_tab3_saved_context()
    if (is.null(ctx) || !file.exists(ctx$export_file)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    
    dt <- tryCatch({
      fread(ctx$export_file, colClasses = "character")
    }, error = function(e) {
      NULL
    })
    
    if (is.null(dt)) {
      showNotification("Unable to read the saved codelist.", type = "error")
      return()
    }
    
    original_n <- nrow(dt)
    dedup_dt <- unique(dt)
    removed_n <- original_n - nrow(dedup_dt)
    
    fwrite(dedup_dt, ctx$export_file)
    deduplicate_summary_data(data.table(records_removed = removed_n))
    tab3_saved_state(tab3_saved_state() + 1L)
    
    showNotification(paste0(removed_n, " duplicate rows were removed."), type = "message")
  })
  
  output$deduplicate_summary_table <- renderDT({
    dt <- deduplicate_summary_data()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(
        data.table(Result = "No deduplication has been applied yet."),
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    
    datatable(dt, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  
  #### ---------- tab 2.0: download the current final codelist ---------- ####
  output$download_final_codelist_tab3 <- downloadHandler(
    filename = function() {
      export_name <- trimws(input$export_filename_tab3)
      if (is.null(export_name) || !nzchar(export_name)) {
        saved_file <- last_saved_export_file()
        if (!is.null(saved_file) && file.exists(saved_file)) {
          return(basename(saved_file))
        }
        export_name <- "verified_codelist"
      }
      paste0(export_name, ".csv")
    },
    content = function(file) {
      ctx <- get_tab3_saved_context()
      if (!is.null(ctx) && !is.null(ctx$dt) && nrow(ctx$dt) >= 0) {
        fwrite(ctx$dt, file)
        return(invisible(NULL))
      }
      
      saved_file <- last_saved_export_file()
      if (!is.null(saved_file) && file.exists(saved_file)) {
        file.copy(saved_file, file, overwrite = TRUE)
        return(invisible(NULL))
      }
      
      stop("Save the verified codelist first.")
    }
  )
  
  #### ---------- tab 2.0: tags harmonisation ---------- ####
  output$tags_harmonisation_ui <- renderUI({
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      return(wellPanel(p("Save the verified codelist first.")))
    }
    if (is.na(ctx$tags_col) || !(ctx$tags_col %in% names(ctx$dt))) {
      return(wellPanel(p("Mapped tags column was not found in the verified codelist.")))
    }
    
    dt <- copy(ctx$dt)
    dt[, tags_value := trimws(as.character(get(ctx$tags_col)))]
    dt <- dt[!is.na(tags_value) & tags_value != ""]
    original_tags <- sort(unique(dt$tags_value))
    
    if (length(original_tags) == 0) {
      return(wellPanel(p("No tags found.")))
    }
    
    tagList(
      div(
        style = "border: 1px solid #ddd; border-radius: 6px; padding: 12px; overflow-x: auto;",
        fluidRow(
          column(4, strong("Original tag")),
          column(3, strong("Relabel?")),
          column(5, strong("New tag"))
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 8px;"),
        lapply(seq_along(original_tags), function(i) {
          fluidRow(
            column(4, div(style = "padding-top: 6px;", original_tags[i])),
            column(3, radioButtons(
              paste0("tag_relabel_choice_", i),
              NULL,
              choices = c("No", "Yes"),
              selected = "No",
              inline = TRUE
            )),
            column(5, textInput(
              paste0("tag_relabel_value_", i),
              NULL,
              value = "",
              width = "100%"
            ))
          )
        })
      )
    )
  })
  
  observe({
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) return()
    if (is.na(ctx$tags_col) || !(ctx$tags_col %in% names(ctx$dt))) return()
    
    dt <- copy(ctx$dt)
    dt[, tags_value := trimws(as.character(get(ctx$tags_col)))]
    dt <- dt[!is.na(tags_value) & tags_value != ""]
    original_tags <- sort(unique(dt$tags_value))
    
    for (i in seq_along(original_tags)) {
      choice_id <- paste0("tag_relabel_choice_", i)
      value_id <- paste0("tag_relabel_value_", i)
      current_choice <- input[[choice_id]]
      if (is.null(current_choice)) next
      
      if (identical(current_choice, "Yes")) {
        shinyjs::enable(value_id)
      } else {
        shinyjs::disable(value_id)
        updateTextInput(session, value_id, value = "")
      }
    }
  })
  
  observeEvent(input$apply_tags_relabelling, {
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    if (is.na(ctx$tags_col) || !(ctx$tags_col %in% names(ctx$dt))) {
      showNotification("Mapped tags column was not found in the verified codelist.", type = "error")
      return()
    }
    
    dt <- copy(ctx$dt)
    dt[, tags_value := trimws(as.character(get(ctx$tags_col)))]
    dt_non_missing <- dt[!is.na(tags_value) & tags_value != ""]
    original_tags <- sort(unique(dt_non_missing$tags_value))
    
    relabel_list <- lapply(seq_along(original_tags), function(i) {
      choice_value <- input[[paste0("tag_relabel_choice_", i)]]
      if (!identical(choice_value, "Yes")) return(NULL)
      
      new_value <- trimws(as.character(input[[paste0("tag_relabel_value_", i)]]))
      if (!nzchar(new_value)) return(data.table(.invalid = TRUE))
      
      data.table(from_tag = original_tags[i], to_tag = new_value)
    })
    
    relabel_map <- rbindlist(relabel_list, fill = TRUE)
    
    if (nrow(relabel_map) > 0 && ".invalid" %in% names(relabel_map)) {
      showNotification("Each selected tag relabelling must have a new tag value.", type = "error")
      return()
    }
    
    if (nrow(relabel_map) == 0) {
      showNotification("No tag relabelling was selected.", type = "message")
      return()
    }
    
    relabel_map <- unique(relabel_map, by = "from_tag")
    
    previous_values <- as.character(dt[[ctx$tags_col]])
    new_values <- previous_values
    matched_rows <- !is.na(previous_values) & previous_values %in% relabel_map$from_tag
    
    for (i in seq_len(nrow(relabel_map))) {
      new_values[previous_values == relabel_map$from_tag[i]] <- relabel_map$to_tag[i]
    }
    
    dt[, (ctx$tags_col) := new_values]
    dt[, tags_value := NULL]
    
    summary_dt <- data.table(
      from_tag = previous_values[matched_rows],
      to_tag = new_values[matched_rows]
    )[, .N, by = .(from_tag, to_tag)]
    setnames(summary_dt, "N", "n_rows_relabelled")
    
    fwrite(dt, ctx$export_file)
    fwrite(relabel_map, file.path(ctx$verification_dir, "tags_relabelling_mapping.csv"))
    fwrite(summary_dt, file.path(ctx$verification_dir, "tags_relabelling_summary.csv"))
    
    tags_relabelling_summary_data(summary_dt)
    tab3_saved_state(tab3_saved_state() + 1L)
    
    showNotification(
      paste0(sum(summary_dt$n_rows_relabelled), " rows had their tags relabelled."),
      type = "message"
    )
  })
  
  output$tags_relabelling_summary_table <- renderDT({
    dt <- tags_relabelling_summary_data()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(
        data.table(Result = "No tag relabelling has been applied yet."),
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    datatable(dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  #### ---------- tab 2.0: drop selected tags ---------- ####
  output$tags_drop_ui <- renderUI({
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      return(wellPanel(p("Save the verified codelist first.")))
    }
    if (is.na(ctx$tags_col) || !(ctx$tags_col %in% names(ctx$dt))) {
      return(wellPanel(p("Mapped tags column was not found in the verified codelist.")))
    }
    
    dt <- copy(ctx$dt)
    dt[, tags_value := trimws(as.character(get(ctx$tags_col)))]
    dt <- dt[!is.na(tags_value) & tags_value != ""]
    original_tags <- sort(unique(dt$tags_value))
    
    if (length(original_tags) == 0) {
      return(wellPanel(p("No tags found.")))
    }
    
    tagList(
      div(
        style = "border: 1px solid #ddd; border-radius: 6px; padding: 12px; overflow-x: auto;",
        fluidRow(
          column(6, strong("Available tag")),
          column(6, strong("Drop?"))
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 8px;"),
        lapply(seq_along(original_tags), function(i) {
          fluidRow(
            column(6, div(style = "padding-top: 6px;", original_tags[i])),
            column(6, radioButtons(
              paste0("tag_drop_choice_", i),
              NULL,
              choices = c("No", "Yes"),
              selected = "No",
              inline = TRUE
            ))
          )
        })
      )
    )
  })
  
  observeEvent(input$apply_drop_tags, {
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    if (is.na(ctx$tags_col) || !(ctx$tags_col %in% names(ctx$dt))) {
      showNotification("Mapped tags column was not found in the verified codelist.", type = "error")
      return()
    }
    
    dt <- copy(ctx$dt)
    dt[, tags_value := trimws(as.character(get(ctx$tags_col)))]
    dt_non_missing <- dt[!is.na(tags_value) & tags_value != ""]
    original_tags <- sort(unique(dt_non_missing$tags_value))
    
    drop_tags <- original_tags[vapply(seq_along(original_tags), function(i) {
      identical(input[[paste0("tag_drop_choice_", i)]], "Yes")
    }, logical(1))]
    
    if (length(drop_tags) == 0) {
      showNotification("No tags were selected for dropping.", type = "message")
      return()
    }
    
    tag_values <- trimws(as.character(dt[[ctx$tags_col]]))
    drop_rows <- !is.na(tag_values) & tag_values %in% drop_tags
    
    if (!any(drop_rows)) {
      showNotification("No rows matched the selected tags.", type = "message")
      return()
    }
    
    summary_dt <- data.table(dropped_tag = tag_values[drop_rows])[, .N, by = .(dropped_tag)]
    setnames(summary_dt, "N", "n_rows_removed")
    setorder(summary_dt, dropped_tag)
    
    dt <- dt[!drop_rows]
    dt[, tags_value := NULL]
    
    fwrite(dt, ctx$export_file)
    fwrite(data.table(dropped_tag = drop_tags), file.path(ctx$verification_dir, "tags_drop_mapping.csv"))
    fwrite(summary_dt, file.path(ctx$verification_dir, "tags_drop_summary.csv"))
    
    tags_drop_summary_data(summary_dt)
    tab3_saved_state(tab3_saved_state() + 1L)
    
    showNotification(
      paste0(sum(summary_dt$n_rows_removed), " rows were removed based on dropped tags."),
      type = "message"
    )
  })
  
  output$tags_drop_summary_table <- renderDT({
    dt <- tags_drop_summary_data()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(
        data.table(Result = "No tag drops have been applied yet."),
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    datatable(dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observeEvent(input$apply_vocab_replication, {
    saved_info <- get_tab3_saved_info()
    if (is.null(saved_info)) {
      showNotification("Save verified codelist before applying vocabulary replication.", type = "error")
      return()
    }
    
    row_ids <- replication_row_ids()
    if (length(row_ids) == 0) {
      showNotification("No replication rows are available.", type = "error")
      return()
    }
    
    replication_map_list <- lapply(row_ids, function(row_id) {
      from_value <- trimws(as.character(input[[paste0("replicate_from_", row_id)]]))
      to_value <- trimws(as.character(input[[paste0("replicate_to_", row_id)]]))
      if (!nzchar(from_value) || !nzchar(to_value)) return(NULL)
      data.table(from_vocabulary = from_value, to_vocabulary = to_value)
    })
    replication_map <- rbindlist(replication_map_list, fill = TRUE)
    
    if (nrow(replication_map) == 0) {
      showNotification("Add at least one valid From/To replication pair.", type = "error")
      return()
    }
    
    replication_map <- unique(replication_map)
    coding_col <- saved_info$coding_system_col
    current_values <- as.character(saved_info$codelist_dt[[coding_col]])
    replicated_blocks <- vector("list", nrow(replication_map))
    summary_list <- vector("list", nrow(replication_map))
    
    for (i in seq_len(nrow(replication_map))) {
      matched_rows <- !is.na(current_values) & current_values == replication_map$from_vocabulary[i]
      n_rows <- sum(matched_rows)
      summary_list[[i]] <- data.table(
        from_vocabulary = replication_map$from_vocabulary[i],
        to_vocabulary = replication_map$to_vocabulary[i],
        n_rows_replicated = n_rows
      )
      if (n_rows > 0) {
        replicated_dt <- copy(saved_info$codelist_dt[matched_rows])
        replicated_dt[, (coding_col) := replication_map$to_vocabulary[i]]
        replicated_blocks[[i]] <- replicated_dt
      }
    }
    
    summary_dt <- rbindlist(summary_list, fill = TRUE)
    total_replicated <- sum(summary_dt$n_rows_replicated)
    
    if (total_replicated > 0) {
      augmented_dt <- rbindlist(c(list(saved_info$codelist_dt), replicated_blocks), use.names = TRUE, fill = TRUE)
      fwrite(augmented_dt, saved_info$codelist_file)
      fwrite(replication_map, file.path(saved_info$verification_dir, "vocabulary_replication_mapping.csv"))
      fwrite(summary_dt, file.path(saved_info$verification_dir, "vocabulary_replication_summary.csv"))
      tab3_saved_state(tab3_saved_state() + 1L)
    }
    
    vocab_replication_summary_data(summary_dt)
    showNotification(paste0(total_replicated, " rows were replicated across vocabularies."), type = "message")
  })
  
  output$vocab_replication_summary_table <- renderDT({
    summary_dt <- vocab_replication_summary_data()
    if (is.null(summary_dt) || nrow(summary_dt) == 0) {
      return(datatable(
        data.table(Result = "No vocabulary replication has been applied yet."),
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    datatable(summary_dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observeEvent(input$save_verified_codelist, {
    dt <- tab3_data()
    req(dt)
    selected_cols <- selected_columns_tab3()
    if (length(selected_cols) == 0) {
      showNotification("Select at least one column before saving.", type = "error")
      return()
    }
    rename_dt <- data.table(
      original_name = selected_cols,
      rename = vapply(seq_along(selected_cols), function(i) identical(input[[paste0("rename_choice_tab3_", i)]], "Yes"), logical(1)),
      new_name = vapply(seq_along(selected_cols), function(i) {
        value <- input[[paste0("new_col_tab3_", i)]]
        if (is.null(value) || !nzchar(trimws(value))) selected_cols[i] else trimws(value)
      }, character(1)),
      code_column = vapply(seq_along(selected_cols), function(i) isTRUE(input[[paste0("code_column_tab3_", i)]]), logical(1)),
      coding_system_column = vapply(seq_along(selected_cols), function(i) isTRUE(input[[paste0("coding_system_column_tab3_", i)]]), logical(1)),
      tags_column = vapply(seq_along(selected_cols), function(i) isTRUE(input[[paste0("tags_column_tab3_", i)]]), logical(1)),
      variable_column = vapply(seq_along(selected_cols), function(i) isTRUE(input[[paste0("variable_column_tab3_", i)]]), logical(1))
    )
    if (any(grepl("\\s", rename_dt$new_name))) {
      showNotification("New column names cannot contain spaces.", type = "error")
      return()
    }
    if (anyDuplicated(rename_dt$new_name)) {
      showNotification("New column names must be unique.", type = "error")
      return()
    }
    role_cols <- c("code_column", "coding_system_column", "tags_column", "variable_column")
    role_counts <- vapply(role_cols, function(x) sum(rename_dt[[x]]), integer(1))
    if (any(role_counts != 1L)) {
      showNotification("Select exactly one column for code_column, coding_system_column, tags_column, and variable_column.", type = "error")
      return()
    }
    export_name <- trimws(input$export_filename_tab3)
    if (!nzchar(export_name)) export_name <- "verified_codelist"
    if (grepl("\\s", export_name)) {
      showNotification("The export file name cannot contain spaces.", type = "error")
      return()
    }
    verification_dir <- file.path(projectFolder, "2_Verification")
    if (dir.exists(verification_dir)) {
      unlink(list.files(verification_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), recursive = TRUE, force = TRUE)
    } else {
      dir.create(verification_dir, recursive = TRUE, showWarnings = FALSE)
    }
    output_dt <- copy(dt[, ..selected_cols])
    setnames(output_dt, old = rename_dt$original_name, new = rename_dt$new_name)
    saved_export_file <- file.path(verification_dir, paste0(export_name, ".csv"))
    fwrite(output_dt, saved_export_file)
    last_saved_export_file(saved_export_file)
    fwrite(rename_dt[, .(original_name, rename, new_name)], file.path(verification_dir, "column_renaming_mapping.csv"))
    role_mapping <- data.table(
      role = c("code_column", "coding_system_column", "tags_column", "variable_column"),
      selected_column = c(
        rename_dt[code_column == TRUE, new_name],
        rename_dt[coding_system_column == TRUE, new_name],
        rename_dt[tags_column == TRUE, new_name],
        rename_dt[variable_column == TRUE, new_name]
      )
    )
    fwrite(role_mapping, file.path(verification_dir, "column_roles_mapping.csv"))
    icd9pcs_relabelled_data(data.table())
    vocab_relabelling_summary_data(data.table())
    vocab_replication_summary_data(data.table())
    tags_relabelling_summary_data(data.table())
    cleaning_summary_data(data.table())
    rounding_issues_data(data.table())
    possible_rounding_issues_data(data.table())
    scientific_notation_issues_data(data.table())
    rounding_drop_summary(data.table())
    possible_rounding_drop_summary(data.table())
    scientific_notation_drop_summary(data.table())
    tags_drop_summary_data(data.table())
    deduplicate_summary_data(data.table())
    replication_row_ids(1L)
    rounding_vocab_ids(1L)
    possible_rounding_vocab_ids(1L)
    tab3_saved_state(tab3_saved_state() + 1L)
    showNotification("Verified codelist saved in 2_Verification.", type = "message")
  })
  
  observeEvent(input$apply_icd9pcs_correction, {
    if (!identical(input$create_icd9pcs_codes, "Yes")) {
      showNotification("ICD9PCS correction not applied because 'No' is selected.", type = "message")
      return()
    }
    
    verification_dir <- file.path(projectFolder, "2_Verification")
    roles_file <- file.path(verification_dir, "column_roles_mapping.csv")
    
    if (!file.exists(roles_file)) {
      showNotification("column_roles_mapping.csv was not found in 2_Verification.", type = "error")
      return()
    }
    
    role_mapping <- tryCatch({
      fread(roles_file, colClasses = "character")
    }, error = function(e) {
      NULL
    })
    
    if (is.null(role_mapping) || nrow(role_mapping) == 0) {
      showNotification("Unable to read column_roles_mapping.csv.", type = "error")
      return()
    }
    
    coding_system_col <- role_mapping[role == "coding_system_column", selected_column]
    code_col <- role_mapping[role == "code_column", selected_column]
    
    if (length(coding_system_col) != 1 || length(code_col) != 1) {
      showNotification("The coding system column or code column mapping is missing.", type = "error")
      return()
    }
    
    export_name <- trimws(input$export_filename_tab3)
    if (!nzchar(export_name)) export_name <- "verified_codelist"
    codelist_file <- file.path(verification_dir, paste0(export_name, ".csv"))
    
    if (!file.exists(codelist_file)) {
      showNotification("The verified codelist file was not found in 2_Verification.", type = "error")
      return()
    }
    
    codelist_dt <- tryCatch({
      fread(codelist_file, colClasses = "character")
    }, error = function(e) {
      NULL
    })
    
    if (is.null(codelist_dt)) {
      showNotification("Unable to read the verified codelist.", type = "error")
      return()
    }
    
    if (!(coding_system_col %in% names(codelist_dt)) || !(code_col %in% names(codelist_dt))) {
      showNotification("Mapped coding system column or code column is not present in the verified codelist.", type = "error")
      return()
    }
    
    current_coding_system <- as.character(codelist_dt[[coding_system_col]])
    current_code <- as.character(codelist_dt[[code_col]])
    
    first_part <- vapply(
      strsplit(ifelse(is.na(current_code), "", current_code), ".", fixed = TRUE),
      function(x) {
        if (length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
      },
      character(1)
    )
    
    char_count <- nchar(first_part)
    
    relabel_rows <- !is.na(current_coding_system) &
      current_coding_system %in% c("ICD9CM", "ICD9") &
      char_count == 2
    
    previous_coding_system <- current_coding_system
    codelist_dt[relabel_rows, (coding_system_col) := "ICD9PCS"]
    
    relabelled_codes <- codelist_dt[relabel_rows, .(
      code = get(code_col),
      previous_coding_system = previous_coding_system[relabel_rows],
      new_coding_system = get(coding_system_col)
    )]
    
    fwrite(codelist_dt, codelist_file)
    fwrite(relabelled_codes, file.path(verification_dir, "icd9pcs_relabelled_codes.csv"))
    icd9pcs_relabelled_data(relabelled_codes)
    tab3_saved_state(tab3_saved_state() + 1L)
    
    showNotification(
      paste0(nrow(relabelled_codes), " codes were relabelled to ICD9PCS."),
      type = "message"
    )
  })
  
  output$icd9pcs_relabelled_table <- renderDT({
    relabelled_dt <- icd9pcs_relabelled_data()
    if (is.null(relabelled_dt) || nrow(relabelled_dt) == 0) {
      return(datatable(
        data.table(Result = "No codes have been relabelled yet."),
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    datatable(
      relabelled_dt,
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  ###############################################################################
  ######################## TAB 2.0 - CLEANING ##################################
  ###############################################################################
  
  observeEvent(input$apply_tab3_cleaning, {
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    required_cols <- c(ctx$code_col, ctx$coding_col, ctx$tags_col, ctx$variable_col)
    if (any(is.na(required_cols)) || !all(required_cols %in% names(ctx$dt))) {
      showNotification("The mapped role columns could not be found in the verified codelist.", type = "error")
      return()
    }
    
    dt <- copy(ctx$dt)
    dt[, .row_id := .I]
    code_vals <- trimws(as.character(dt[[ctx$code_col]]))
    coding_vals <- trimws(as.character(dt[[ctx$coding_col]]))
    tags_vals <- trimws(as.character(dt[[ctx$tags_col]]))
    variable_vals <- trimws(as.character(dt[[ctx$variable_col]]))
    
    code_empty_rows <- is.na(code_vals) | code_vals == ""
    code_dash_rows <- !is.na(code_vals) & code_vals == "-"
    coding_empty_rows <- is.na(coding_vals) | coding_vals == ""
    tags_empty_rows <- is.na(tags_vals) | tags_vals == ""
    variable_empty_rows <- is.na(variable_vals) | variable_vals == ""
    
    summary_dt <- data.table(
      column_checked = c(ctx$code_col, ctx$code_col, ctx$coding_col, ctx$tags_col, ctx$variable_col),
      reason = c("empty_or_blank", "dash", "empty_or_blank", "empty_or_blank", "empty_or_blank"),
      n_rows_dropped = c(sum(code_empty_rows), sum(code_dash_rows), sum(coding_empty_rows), sum(tags_empty_rows), sum(variable_empty_rows))
    )
    summary_dt <- summary_dt[n_rows_dropped > 0]
    
    drop_rows <- code_empty_rows | code_dash_rows | coding_empty_rows | tags_empty_rows | variable_empty_rows
    if (!any(drop_rows)) {
      cleaning_summary_data(data.table(Result = "No rows were dropped during cleaning."))
      showNotification("No rows met the cleaning drop criteria.", type = "message")
      return()
    }
    
    cleaned_dt <- dt[!drop_rows]
    cleaned_dt[, .row_id := NULL]
    fwrite(cleaned_dt, ctx$export_file)
    fwrite(summary_dt, file.path(ctx$verification_dir, "tab3_cleaning_summary.csv"))
    cleaning_summary_data(summary_dt)
    tab3_saved_state(tab3_saved_state() + 1L)
    showNotification(paste0(sum(summary_dt$n_rows_dropped), " rows were dropped during cleaning."), type = "message")
  })
  
  output$tab3_cleaning_summary <- renderDT({
    dt <- cleaning_summary_data()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(data.table(Result = "No cleaning has been applied yet."), options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    datatable(dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  ###############################################################################
  ######################## TAB 2.0 - ROUNDING ISSUES ###########################
  ###############################################################################
  
  output$rounding_vocab_selector_ui <- renderUI({
    ids <- rounding_vocab_ids()
    choices <- get_tab3_vocab_choices()
    tagList(lapply(ids, function(id) {
      fluidRow(
        column(10, selectizeInput(paste0("rounding_vocab_", id), "Coding system", choices = choices, selected = NULL, multiple = FALSE)),
        column(2, br(), actionButton(paste0("delete_rounding_vocab_", id), NULL, icon = icon("trash"), class = "btn-danger btn-sm"))
      )
    }))
  })
  
  output$possible_rounding_vocab_selector_ui <- renderUI({
    ids <- possible_rounding_vocab_ids()
    choices <- get_tab3_vocab_choices()
    tagList(lapply(ids, function(id) {
      fluidRow(
        column(10, selectizeInput(paste0("possible_rounding_vocab_", id), "Coding system", choices = choices, selected = NULL, multiple = FALSE)),
        column(2, br(), actionButton(paste0("delete_possible_rounding_vocab_", id), NULL, icon = icon("trash"), class = "btn-danger btn-sm"))
      )
    }))
  })
  
  observeEvent(input$add_rounding_vocab, {
    ids <- rounding_vocab_ids()
    rounding_vocab_ids(c(ids, max(ids) + 1L))
  })
  
  observeEvent(input$add_possible_rounding_vocab, {
    ids <- possible_rounding_vocab_ids()
    possible_rounding_vocab_ids(c(ids, max(ids) + 1L))
  })
  
  observe({
    ids <- rounding_vocab_ids()
    lapply(ids, function(id) {
      observeEvent(input[[paste0("delete_rounding_vocab_", id)]], {
        current_ids <- rounding_vocab_ids()
        if (length(current_ids) > 1) {
          rounding_vocab_ids(setdiff(current_ids, id))
        }
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    ids <- possible_rounding_vocab_ids()
    lapply(ids, function(id) {
      observeEvent(input[[paste0("delete_possible_rounding_vocab_", id)]], {
        current_ids <- possible_rounding_vocab_ids()
        if (length(current_ids) > 1) {
          possible_rounding_vocab_ids(setdiff(current_ids, id))
        }
      }, ignoreInit = TRUE)
    })
  })
  
  selected_rounding_vocabs <- reactive({
    ids <- rounding_vocab_ids()
    unique(Filter(function(x) !is.null(x) && nzchar(x), lapply(ids, function(id) input[[paste0("rounding_vocab_", id)]])))
  })
  
  selected_possible_rounding_vocabs <- reactive({
    ids <- possible_rounding_vocab_ids()
    unique(Filter(function(x) !is.null(x) && nzchar(x), lapply(ids, function(id) input[[paste0("possible_rounding_vocab_", id)]])))
  })
  
  observeEvent(input$check_rounding_issues, {
    ctx <- get_tab3_saved_context()
    if (is.null(ctx) || is.na(ctx$code_col) || is.na(ctx$coding_col)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    vocabs <- selected_rounding_vocabs()
    if (length(vocabs) == 0) {
      showNotification("Select at least one coding system.", type = "error")
      return()
    }
    dt <- copy(ctx$dt)
    dt[, .row_id := .I]
    dt[, code_value := as.character(get(ctx$code_col))]
    dt[, coding_value := as.character(get(ctx$coding_col))]
    issues <- dt[coding_value %in% vocabs & !is.na(code_value) & grepl("000$", trimws(code_value)), .(.row_id, code = code_value, coding_system = coding_value)]
    rounding_issues_data(issues)
    if (nrow(issues) == 0) showNotification("No rounding issues found for the selected coding systems.", type = "message")
  })
  
  observeEvent(input$check_possible_rounding_issues, {
    ctx <- get_tab3_saved_context()
    if (is.null(ctx) || is.na(ctx$code_col) || is.na(ctx$coding_col)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    vocabs <- selected_possible_rounding_vocabs()
    if (length(vocabs) == 0) {
      showNotification("Select at least one coding system.", type = "error")
      return()
    }
    dt <- copy(ctx$dt)
    dt[, .row_id := .I]
    dt[, code_value := as.character(get(ctx$code_col))]
    dt[, coding_value := as.character(get(ctx$coding_col))]
    issues <- dt[coding_value %in% vocabs & !is.na(code_value) & grepl("00$", trimws(code_value)) & !grepl("000$", trimws(code_value)), .(.row_id, code = code_value, coding_system = coding_value)]
    possible_rounding_issues_data(issues)
    if (nrow(issues) == 0) showNotification("No possible rounding issues found for the selected coding systems.", type = "message")
  })
  
  output$rounding_issues_tab3 <- renderDT({
    dt <- rounding_issues_data()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(data.table(Result = "No rounding issues identified yet."), options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    datatable(dt[, !".row_id"], options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$possible_rounding_issues_tab3 <- renderDT({
    dt <- possible_rounding_issues_data()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(data.table(Result = "No possible rounding issues identified yet."), options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    datatable(dt[, !".row_id"], options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observeEvent(input$apply_drop_rounding_issues, {
    if (!identical(input$drop_rounding_issues, "Yes")) {
      showNotification("Drop issues is set to No.", type = "message")
      return()
    }
    issues <- rounding_issues_data()
    if (is.null(issues) || nrow(issues) == 0) {
      showNotification("No rounding issues to drop.", type = "message")
      return()
    }
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    dt <- copy(ctx$dt)
    dt[, .row_id := .I]
    dt <- dt[!.row_id %in% issues$.row_id]
    dt[, .row_id := NULL]
    fwrite(dt, ctx$export_file)
    summary_dt <- issues[, .(n_rows_dropped = .N), by = .(coding_system)]
    rounding_drop_summary(summary_dt)
    fwrite(summary_dt, file.path(ctx$verification_dir, "rounding_drop_summary.csv"))
    rounding_issues_data(data.table())
    tab3_saved_state(tab3_saved_state() + 1L)
    showNotification(paste(sum(summary_dt$n_rows_dropped), "rounding issue rows dropped."), type = "message")
  })
  
  observeEvent(input$apply_drop_possible_rounding_issues, {
    if (!identical(input$drop_possible_rounding_issues, "Yes")) {
      showNotification("Drop issues is set to No.", type = "message")
      return()
    }
    issues <- possible_rounding_issues_data()
    if (is.null(issues) || nrow(issues) == 0) {
      showNotification("No possible rounding issues to drop.", type = "message")
      return()
    }
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    dt <- copy(ctx$dt)
    dt[, .row_id := .I]
    dt <- dt[!.row_id %in% issues$.row_id]
    dt[, .row_id := NULL]
    fwrite(dt, ctx$export_file)
    summary_dt <- issues[, .(n_rows_dropped = .N), by = .(coding_system)]
    possible_rounding_drop_summary(summary_dt)
    fwrite(summary_dt, file.path(ctx$verification_dir, "possible_rounding_drop_summary.csv"))
    possible_rounding_issues_data(data.table())
    tab3_saved_state(tab3_saved_state() + 1L)
    showNotification(paste(sum(summary_dt$n_rows_dropped), "possible rounding issue rows dropped."), type = "message")
  })
  
  output$rounding_drop_summary_tab3 <- renderDT({
    dt <- rounding_drop_summary()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(data.table(Result = "No rounding drops applied yet."), options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    datatable(dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$possible_rounding_drop_summary_tab3 <- renderDT({
    dt <- possible_rounding_drop_summary()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(data.table(Result = "No possible rounding drops applied yet."), options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    datatable(dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Quick check for scientific notation in the code column
  observe({
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) return()
    issues <- data.table()
    if (!is.na(ctx$code_col) && ctx$code_col %in% names(ctx$dt)) {
      dt <- copy(ctx$dt)
      dt[, .row_id := .I]
      dt[, code_value := trimws(as.character(get(ctx$code_col)))]
      if (!is.na(ctx$coding_col) && ctx$coding_col %in% names(dt)) {
        dt[, coding_value := as.character(get(ctx$coding_col))]
      } else {
        dt[, coding_value := NA_character_]
      }
      issues <- dt[!is.na(code_value) & grepl("E+", code_value, fixed = TRUE), .(.row_id, code = code_value, coding_system = coding_value)]
    }
    scientific_notation_issues_data(issues)
  })
  
  output$scientific_notation_issues_tab3 <- renderDT({
    dt <- scientific_notation_issues_data()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(data.table(Result = 'No rows with "E+" were identified.'), options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    datatable(dt[, !".row_id"], options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observeEvent(input$apply_drop_scientific_notation_issues, {
    if (!identical(input$drop_scientific_notation_issues, "Yes")) {
      showNotification("Drop issues is set to No.", type = "message")
      return()
    }
    issues <- scientific_notation_issues_data()
    if (is.null(issues) || nrow(issues) == 0) {
      showNotification('No rows with "E+" to drop.', type = "message")
      return()
    }
    ctx <- get_tab3_saved_context()
    if (is.null(ctx)) {
      showNotification("Save the verified codelist first.", type = "error")
      return()
    }
    dt <- copy(ctx$dt)
    dt[, .row_id := .I]
    dt <- dt[!.row_id %in% issues$.row_id]
    dt[, .row_id := NULL]
    fwrite(dt, ctx$export_file)
    summary_dt <- issues[, .(n_rows_dropped = .N), by = .(coding_system)]
    scientific_notation_drop_summary(summary_dt)
    fwrite(summary_dt, file.path(ctx$verification_dir, "scientific_notation_drop_summary.csv"))
    scientific_notation_issues_data(data.table())
    tab3_saved_state(tab3_saved_state() + 1L)
    showNotification(paste(sum(summary_dt$n_rows_dropped), 'rows with "E+" were dropped.'), type = "message")
  })
  
  output$scientific_notation_drop_summary_tab3 <- renderDT({
    dt <- scientific_notation_drop_summary()
    if (is.null(dt) || nrow(dt) == 0) {
      return(datatable(data.table(Result = "No scientific notation drops applied yet."), options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    datatable(dt, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Keep the run date around for the report boxes
  run_date <- reactiveVal(format(Sys.Date(), "%Y-%m-%d"))
  folder_names <- reactiveVal(NULL)  # Reactive value to store folder names
  algorithms_folders <- reactiveVal(NULL)  # Reactive value to store folders inside "Algorithms"
  algorithms_exists <- reactiveVal(FALSE)  # Reactive value to store if Algorithms exist
  other_folders_exist <- reactiveVal(FALSE)  # Reactive value to store if other folders exist
  study_name_exists <- reactiveVal(FALSE)
  mother_excel_data <- reactiveVal(NULL)
  
  get_mother_excel_data <- function() {
    mother_excel_data()
  }
  
  resolve_zip_root <- function(base_dir) {
    clean_dirs <- function(paths) {
      paths <- paths[dir.exists(paths)]
      paths <- paths[!basename(paths) %in% "__MACOSX"]
      paths <- paths[!startsWith(basename(paths), ".")]
      paths
    }
    
    top_dirs <- clean_dirs(list.files(base_dir, full.names = TRUE, all.files = FALSE))
    
    if (length(top_dirs) == 1) {
      inner_dirs <- clean_dirs(list.files(top_dirs[1], full.names = TRUE, all.files = FALSE))
      if (length(inner_dirs) > 0 || file.exists(file.path(top_dirs[1], "Algorithms"))) {
        return(top_dirs[1])
      }
    }
    
    base_dir
  }
  
  # **Update date and cache Mother Excel data when files are uploaded**
  observeEvent(input$xlsx_file1, {
    req(input$xlsx_file1)
    run_date(format(Sys.Date(), "%Y-%m-%d"))
    
    excel_data <- tryCatch({
      as.data.table(read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS"))
    }, error = function(e) {
      NULL
    })
    
    mother_excel_data(excel_data)
  })
  
  observeEvent(list(input$xlsx_file1, input$string_input), {
    excel_data <- get_mother_excel_data()
    study_name <- input$string_input
    
    if (is.null(excel_data) || is.null(study_name) || !nzchar(study_name)) {
      study_name_exists(FALSE)
    } else {
      study_name_exists(study_name %in% colnames(excel_data))
    }
  }, ignoreInit = FALSE)
  
  # **Display the date in UI**
  output$run_date <- renderText({
    run_date()
  })
  
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
        Conceptsets = NA
      ))
    }
    data.frame(
      Algorithms = ifelse(algorithms_exists(), "Yes", "No"),
      Conceptsets = ifelse(other_folders_exist(), "Yes", "No"),
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
  #Check for invalid names in the conceptsets and algorithm folders
  observeEvent(input$zip_file, {
    req(input$zip_file)
    
    withProgress(message = "Preparing uploaded ZIP file", value = 0, {
      incProgress(0.1, detail = "Resetting folders...")
      initialize_directory(temp_dir)
      initialize_directory(cond_dir)
      initialize_directory(alg_dir)
      folder_names(NULL)
      algorithms_folders(NULL)
      algorithms_exists(FALSE)
      other_folders_exist(FALSE)
      
      incProgress(0.3, detail = "Extracting ZIP...")
      unzip(input$zip_file$datapath, exdir = temp_dir)
      
      incProgress(0.5, detail = "Scanning folders...")
      root_path <- resolve_zip_root(temp_dir)
      root_dirs <- list.dirs(root_path, recursive = FALSE, full.names = TRUE)
      dir_names <- basename(root_dirs)
      
      files_all <- dir_names
      print(files_all)
      
      # Check if the Algorithm folder exists
      if ("Algorithms" %in% files_all) {
        algorithms_exists(TRUE)
        alg_root <- file.path(root_path, "Algorithms")
        alg_subdirs <- list.dirs(alg_root, recursive = FALSE, full.names = TRUE)
        algorithms_folders(basename(alg_subdirs))
        
        valid_algorithms <- alg_subdirs[sapply(basename(alg_subdirs), function(name) {
          str_count(name, "_") == 3
        })]
        
        if (length(valid_algorithms) > 0) {
          incProgress(0.7, detail = "Copying Algorithms folders...")
          file.copy(valid_algorithms, alg_dir, recursive = TRUE)
        }
      } else {
        algorithms_folders(NULL)
        algorithms_exists(FALSE)
      }
      
      conceptset_dirs <- root_dirs[basename(root_dirs) != "Algorithms"]
      folder_names(basename(conceptset_dirs))
      
      if (length(conceptset_dirs) > 0) {
        other_folders_exist(TRUE)
        
        valid_cond <- conceptset_dirs[sapply(basename(conceptset_dirs), function(name) {
          str_count(name, "_") == 3
        })]
        
        if (length(valid_cond) > 0) {
          incProgress(0.9, detail = "Copying Conceptsets folders...")
          file.copy(valid_cond, cond_dir, recursive = TRUE)
        }
      } else {
        folder_names(NULL)
        other_folders_exist(FALSE)
      }
      
      incProgress(1, detail = "Done")
      unlink(temp_dir, recursive = TRUE, force = TRUE)
    })
  })
  
  
  #Output all files that do not comply with 3 underscores rule
  output$conceptsetss_name_check <- renderPrint({
    folders <- folder_names()
    if (is.null(folders)) {
      return("No folders in Conceptsets to check.")
    }
    
    invalid_folders <- folders[sapply(folders, function(name) {
      str_count(name, "_") != 3
    })]
    
    if (length(invalid_folders) > 0) {
      print("Conceptsets folders with invalid names:")
      invalid_folder_rep<-data.table(wrong_folder_names=invalid_folders)
      fwrite(invalid_folder_rep, paste0(report_dir, "/conceptsets_name_check.csv"), row.names = F)
      
      return(invalid_folders)
      
    } else {
      return("All Conceptsets folders are named correctly.")
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
    
<<<<<<< HEAD
    excel_data <- get_mother_excel_data()
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading the codelist library metadata file (MotherExcel). Please check the file format or permissions."))
=======
    # Read the Excel file
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
>>>>>>> parent of 6a73946 (Comments update)
    
    excel_data <- copy(excel_data)
    if (!all(c("Event_abbreviation / Variable Name", "Covariates with no codes") %in% colnames(excel_data))) {
      return(data.table(Folder = "None", Result = "Invalid", Comment = "Required columns not found in the Mother Excel file."))
    }
    
    # Rename and filter
    setnames(excel_data, old = c("Event_abbreviation / Variable Name", "Covariates with no codes"),
             new = c("variable_name", "covariates_no_code"))
    excel_data[, covariates_no_code := tolower(covariates_no_code)]
    excel_data <- excel_data[covariates_no_code == "yes"]
    
    if (nrow(excel_data) > 0) excel_data[, covariates_no_code := 1]
    
    # Check Conceptsets Folder
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conceptsets folder found."))
    conceptsets_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(conceptsets_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conceptsets directory."))
    
    # Extract relevant folder details
    folder_details <- lapply(conceptsets_folders, function(folder) {
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
      fwrite(result, paste0(report_dir, "/no_code_covariates_conceptsetss.csv"), row.names = F)
      
      return(result)
    } else {
      result<-data.table(Folder = "None", Result = "Valid",
                         Comment = "'Covariates with no codes' folders are correctly specified.")
      fwrite(result, paste0(report_dir, "/no_code_covariates_conceptsetss.csv"), row.names = F)
      
      return(result)
    }
    
  })
  
  # Output Table
  output$cov_no_codes_cond <- renderDT({
    datatable(cov_no_codes_cond(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })  
  
  cov_no_codes_alg <- eventReactive(input$xlsx_file1,{
    req(input$xlsx_file1)
    
<<<<<<< HEAD
    excel_data <- get_mother_excel_data()
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading the codelist library metadata file (MotherExcel)."))
=======
    # Read the Excel file
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
>>>>>>> parent of 6a73946 (Comments update)
    
    excel_data <- copy(excel_data)
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
      fwrite(result, paste0(report_dir, "/no_code_covariates_conceptsetss.csv"), row.names = F)
      
      return(result)
    } else {
      result<-data.table(Folder = "None", Result = "Valid",
                         Comment = "'Covariates with no codes' folders are correctly specified.")
      fwrite(result, paste0(report_dir, "/no_code_covariates_conceptsetss.csv"), row.names = F)
      
      return(result)
    }
  })
  
  # Output Table
  output$cov_no_codes_alg <- renderDT({
    datatable(cov_no_codes_alg(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })  
  
  
  #### delete covariates with no codes####
  # Delete Covariates with No Codes - Conceptsets
  cov_no_codes_delete_cond <- eventReactive(input$step3, {
    req(input$xlsx_file1)
    
<<<<<<< HEAD
    excel_data <- get_mother_excel_data()
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading the codelist library metadata file (MotherExcel)."))
=======
    # Read the Excel file safely
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
>>>>>>> parent of 6a73946 (Comments update)
    
    excel_data <- copy(excel_data)
    
    # Ensure required columns exist
    if (!all(c("Event_abbreviation / Variable Name", "Covariates with no codes") %in% colnames(excel_data))) {
      return(data.table(Folder = "None", Result = "Invalid", Comment = "Required columns not found in the Mother Excel file."))
    }
    
    # Rename and filter
    setnames(excel_data, old = c("Event_abbreviation / Variable Name", "Covariates with no codes"),
             new = c("variable_name", "covariates_no_code"))
    excel_data[, covariates_no_code := tolower(covariates_no_code)]
    excel_data <- excel_data[covariates_no_code == "yes"]
    
    # Check Conceptsets Folder
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conceptsets folder found."))
    conceptsets_folders <- list.dirs(cond_dir, recursive = FALSE, full.names = TRUE)
    if (length(conceptsets_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conceptsets directory."))
    
    # Extract relevant folder details
    folder_details <- lapply(conceptsets_folders, function(folder) {
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
    
<<<<<<< HEAD
    excel_data <- get_mother_excel_data()
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading the codelist library metadata file (MotherExcel)."))
=======
    # Read the Excel file safely
    excel_data <- tryCatch({
      read_excel(input$xlsx_file1$datapath, col_types = "text", sheet = "CDM_EVENTS")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(excel_data)) return(data.table(Folder = "None", Result = "Invalid", Comment = "Error reading Excel file."))
>>>>>>> parent of 6a73946 (Comments update)
    
    excel_data <- copy(excel_data)
    
    # Ensure required columns exist
    if (!all(c("Event_abbreviation / Variable Name", "Covariates with no codes") %in% colnames(excel_data))) {
      return(data.table(Folder = "None", Result = "Invalid", Comment = "Required columns not found in the Mother Excel file."))
    }
    
    # Rename and filter
    setnames(excel_data, old = c("Event_abbreviation / Variable Name", "Covariates with no codes"),
             new = c("variable_name", "covariates_no_code"))
    excel_data[, covariates_no_code := tolower(covariates_no_code)]
    excel_data <- excel_data[covariates_no_code == "yes"]
    
    # Check Conceptsets Folder
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
  
  output$study_name_check <- renderText({
    if (study_name_exists()) {
      return("The study name is present as a column in the Mother Excel file.")
    } else {
      return("The study name is NOT present as a column in the Mother Excel file.")
    }
  })
  
  #### Step 3: ####
  #Grab folder names inside the Conceptsets directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conceptsets folder found."))
    
    # Get all folder names in the Conceptsets directory
    conceptsets_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(conceptsets_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conceptsets directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(conceptsets_folders, check_excel_in_folder)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) > 0) {
      fwrite(results, paste0(report_dir, "/no_files_excel_conceptsetss.csv"), row.names = F)
      
      return(results)
    } else {
      return(data.table(Folder = "All", Result = "Valid", Comment = "All folders contain only one Excel file."))
    }  })
  
  alg_excel_results <- eventReactive(input$step3, {
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    
    # Get all folder names in the Conceptsets directory
    alg_folders <- list.dirs(alg_dir, recursive = FALSE)
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
    # Delete invalid Conceptsets folders
    invalid_cond_folders <- con_excel_results()[Result == "Invalid", Folder]
    if (length(invalid_cond_folders) > 0 && all(invalid_cond_folders != "None")) {
      sapply(invalid_cond_folders, function(folder) {
        unlink(file.path(cond_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
    
    # Delete invalid Algorithms folders
    invalid_alg_folders <- alg_excel_results()[Result == "Invalid", Folder]
    if (length(invalid_alg_folders) > 0 && all(invalid_alg_folders != "None")) {
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
  #Grab folder names inside the Conceptsets directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_name_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conceptsets folder found."))
    
    # Get all folder names in the Conceptsets directory
    conceptsets_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(conceptsets_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conceptsets directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(conceptsets_folders, check_folder_file_match)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) > 0) {
      return(results)
    } else {
      return(data.table(Folder = "All", Result = "Valid", Comment = "All folders and Excel Files names match."))
    }  })
  
  alg_excel_name_results <- eventReactive(input$step3, {
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    
    # Get all folder names in the Conceptsets directory
    alg_folders <- list.dirs(alg_dir, recursive = FALSE)
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
    # Delete invalid Conceptsets folders
    invalid_cond_name_folders <- con_excel_name_results()[Result == "Invalid", Folder]
    if (length(invalid_cond_name_folders) > 0 && all(invalid_cond_name_folders != "None")) {
      sapply(invalid_cond_name_folders, function(folder) {
        unlink(file.path(cond_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
    
    # Delete invalid Algorithms folders
    invalid_alg_name_folders <- alg_excel_name_results()[Result == "Invalid", Folder]
    if (length(invalid_alg_name_folders) > 0 && all(invalid_alg_name_folders != "None")) {
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
  #Grab folder names inside the Conceptsets directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_sheet_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conceptsets folder found."))
    
    # Get all folder names in the Conceptsets directory
    conceptsets_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(conceptsets_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conceptsets directory."))
    
    # Apply check_excel_in_folder to each folder
    results <- lapply(conceptsets_folders, check_file_sheet_match)
    results<-rbindlist(results)
    results<-results[Result == "Invalid"]
    if (nrow(results) > 0) {
      return(results)
    } else {
      return(data.table(Folder = "All", Result = "Valid", Comment = "There is at least one sheet with the correct name."))
    }  })
  
  alg_excel_sheet_results <- eventReactive(input$step3, {
    if (!dir.exists(alg_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Algorithms folder found."))
    
    # Get all folder names in the Conceptsets directory
    alg_folders <- list.dirs(alg_dir, recursive = FALSE)
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
    # Delete invalid Conceptsets folders
    invalid_cond_sheet_folders <- con_excel_sheet_results()[Result == "Invalid", Folder]
    if (length(invalid_cond_sheet_folders) > 0 && all(invalid_cond_sheet_folders != "None")) {
      sapply(invalid_cond_sheet_folders, function(folder) {
        unlink(file.path(cond_dir, folder), recursive = TRUE, force = TRUE)
      })
    }
    
    # Delete invalid Algorithms folders
    invalid_alg_sheet_folders <- alg_excel_sheet_results()[Result == "Invalid", Folder]
    if (length(invalid_alg_sheet_folders) > 0 && all(invalid_alg_sheet_folders != "None")) {
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
  #Grab folder names inside the Conceptsets directory as a reactive value
  # Reactive to retrieve and validate folders
  con_excel_col_results <- eventReactive(input$step3, {
    if (!dir.exists(cond_dir)) return(data.table(Folder = "None", Result = "Invalid", Comment = "No Conceptsets folder found."))
    
    # Get all folder names in the Conceptsets directory
    conceptsets_folders <- list.dirs(cond_dir, recursive = FALSE)
    if (length(conceptsets_folders) == 0) return(data.table(Folder = "None", Result = "Invalid", Comment = "No folders in Conceptsets directory."))
    
    # Define mandatory columns
    cols <- c("Coding system", "Code", "Code name", "Concept", "Concept name", "Tags")
    
    # Apply the function to each folder
    results <- lapply(conceptsets_folders, function(folder) check_excel_mandatory_columns(folder, cols))
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
    # Delete invalid Conceptsets folders
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
  #### Step 7: Algorithms vs Conceptsets ####
  #Grab folder names inside the Conceptsets directory as a reactive value
  # Reactive to retrieve and validate folders
  con_alg_results <- eventReactive(input$step3, {
    
    # Apply the function to each folder
    results <- compare_algorithm_conceptsets_names(alg_dir, cond_dir)
    return(results)
  })
  
  
  #### Step 8: Present data ####
  # Output Step 3 Results as scrollable tables
  output$results_alg_cond <- renderDT({
    datatable(con_alg_results(), options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })
  
  ### Needed folders (study conceptsets that are present) ####
  #wite the stdudy data inside Errors
  observeEvent(input$step4, {
    req(input$xlsx_file1)
    study_name <- input$string_input
    req(study_name)  # Ensure study name is entered
    
    study_data <- tryCatch({
      excel_data <- get_mother_excel_data()
      if (is.null(excel_data)) {
        stop("Error reading the Mother Excel file.")
      }
      excel_data <- copy(excel_data)
      
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
    cond_data<-cond_data[!duplicated(variable_name)]
    cond_data[,present:=1]
    
    alg_codesheet_names<-merge.data.table(alg_codesheet_names, cond_data, by="variable_name", all.x = T)
    alg_codesheet_names<-alg_codesheet_names[!duplicated(variable_name)]
    alg_codesheet_names<-alg_codesheet_names[present == 1]
    
    comb<-rbind(alg_codesheet_names, cond_data)
    comb<-unique(comb)
    
    fwrite(comb, paste0(projectFolder, "/Errors/copy_folders.csv"))
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
    
    alg_data_study<-alg_data[variable_name %in% study_data[,variable_name]]
    setDT(alg_data_study)
    if(nrow(alg_data_study)>0){
      fwrite(alg_data_study[,"variable_name"], paste0(projectFolder,"/Errors/study_data_algorithms.csv"))
      
      #sheetnames for the study only
      alg_folders_study<-alg_data_study[,full_name]
      alg_codesheet_names_study <- list()
      for (folder in alg_folders_study) {
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
            alg_codesheet_names_study[[folder]] <- unique(sheet_data[, .(variable_name = Codesheet_name)])
          }
        }, error = function(e) { next })
      }
      alg_codesheet_names_study<-do.call(rbind,alg_codesheet_names_study) #codesheet of algorithms needed for the study
      alg_codesheet_names_study<-alg_codesheet_names_study[!is.na(variable_name)]
      alg_codesheet_names_study<-unique(alg_codesheet_names_study)
    }else{alg_codesheet_names_study<-NULL}
    
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
    cond_data<-cond_data[!duplicated(variable_name)]
    cond_data[,present:=1]
    
    if(!is.null(alg_codesheet_names_study)){
      alg_codesheet_names_study<-merge.data.table(alg_codesheet_names_study, cond_data, by="variable_name", all.x = T)
      alg_codesheet_names_study<-alg_codesheet_names_study[!duplicated(variable_name)]
      alg_codesheet_names_study<-alg_codesheet_names_study[present == 1]
      alg_codesheet_names_study[,present:=NULL]
    }
    cond_data_study<-cond_data[variable_name %in% study_data[,variable_name]]
    cond_data_study[,present:=NULL]
    fwrite(cond_data_study[,"variable_name"], paste0(projectFolder,"/Errors/study_data_conceptsetss.csv"))
    comb_study<-rbind(alg_codesheet_names_study, cond_data_study)
    comb_study<-unique(comb_study)
    fwrite(comb_study[,"variable_name"], paste0(projectFolder,"/Errors/study_data_only_cond.csv"))
    
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
    
    process_status_av("Processing Complete! Show results.")
    process_completed_av(TRUE)
    
    return(av_data)
    
  })
  
  output$results_av <- renderDT({
    datatable(results_av(), 
              options = list(scrollX = TRUE, scrollY = "300px", paging = TRUE, pageLength = Inf))
  })
  
  #### Step 9: Missing data ####
  ### Needed folders (study conceptsets that are missing) ###
  
  
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
    if(nrow(missing_study_vars)==0){missing_study_vars<-data.table(variable_name="All neccessary conceptsets are present.")}
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
        showNotification(paste("Processing Error:", e$message), type = "error")
        process_status("Processing Failed! Check logs.")
        return(NULL)
      })
      
      req(result)  # Ensure processing was successful
      
      # Save results
      output_dir <- paste0(projectFolder, "/Untouched Codelist")
      if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      
      #study_data<-fread( paste0(projectFolder, "/Errors/study_data.csv"))
      #result<-merge(result, study_data[,"variable_name"], by="variable_name")
      output_file <- file.path(output_dir, "Processed_Codelist.csv")
      fwrite(result, output_file)
      
      incProgress(1, detail = "Saving Completed")
      process_status("Processing Complete! Results saved in 'Untouched Codelist'.")
      process_completed(TRUE)
      shinyjs::enable("clean_files")
    })
  })
  
  #### Step 11: Clean codelist ####
  observeEvent(input$clean_files, {
    if (!process_completed()) {
      showNotification("You must run 'Processing' first!", type = "error")
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
        showNotification(paste("Cleaning Error:", e$message), type = "error")
        clean_status("Cleaning Failed! Check logs.")
        return(NULL)
      })
      
      req(result)
      incProgress(1, detail = "Saving Completed")
      clean_status("Cleaning Complete! Results saved in 'Cleaned Codelist'.")
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









