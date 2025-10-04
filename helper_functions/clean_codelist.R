
clean_codelist <- function(base_dir, study_name, pedianet_extra_file, pedianet_extra_file_2, cross_check) {
  
  # Define the directory containing codelist files
  file <- list.files(base_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(file) == 0) {
    stop("No CSV files found in the 'Untouched Codelist' directory.")
  }
  
  # Initialize result tables


    # Read the CSV file
    data <- fread(file, na.strings = c("NA"), strip.white = T)  # Treat empty and NA values consistently
    
    # Ensure necessary columns exist
    required_cols <- c("variable_name", "coding_system", "code", "tags")
    if (!all(required_cols %in% names(data))) {
      warning(paste("Skipping", basename(file), "due to missing required columns."))
      next
    }
    
    # **Remove hyphens ("-") from the `code` column**
    data<-data[code != "-"]
    data[, c("system", "event_abbreviation", "type") := tstrsplit(variable_name, "_", fixed = TRUE)]
    
    data_preg<-data[type == "PrA"]
    data<-data[type != "PrA"]
    #Update study data to contain only conceptsets names
    study_data<-fread(paste0(projectFolder, "/Errors/study_data_only_cond.csv"))
    data<-data[variable_name %in% study_data[,variable_name]]
    data<-rbind(data, data_preg)
    rm(data_preg)
    data[,system:=NULL][,type:=NULL][,event_abbreviation:=NULL]
    
    # Add pedianet data
    pedianet<-fread(pedianet_extra_file, colClasses = "character")
    pedianet[,variable_name:=paste(system, event_abbreviation, type, sep="_")]
    cols<-names(data)
    pedianet<-pedianet[,cols,with=F]
    pedianet<-pedianet[variable_name %in% study_data[,variable_name]]

    # Create vocabulary ICD9CMP for Pedianet
    icd_9<-data[coding_system == "ICD9CM"]
    icd_9[,coding_system:="ICD9CMP"]
    
    data<-rbind(data, icd_9, pedianet)
    rm(icd_9, pedianet)
    data <-unique(data)
    
    pedianet_2<-fread(pedianet_extra_file_2, colClasses = "character")
    if(!("code_name" %in% names(pedianet_2))){pedianet_2[,code_name:=NA]}
    pedianet_2<-pedianet_2[,cols,with=F]
    pedianet_2<-pedianet_2[variable_name %in% study_data[,variable_name]]
    data<-rbind(data, pedianet_2)
    data <-unique(data)
    rm(pedianet_2)
    
    
    #Chcek if empty rows exist
    data[is.na(code) | code == "", code_remove:=1]
    data[is.na(coding_system) | coding_system == "", coding_system_remove:=1]
    data[is.na(tags) | tags == "", tags_remove:=1]
    data[code_remove == 1 & coding_system_remove==1 & tags_remove==1, remove:=1]
    
    data[is.na(remove), remove:=0]
    data<-data[remove == 0]
    data[,remove:=NULL][,code_remove:=NULL][,coding_system_remove:=NULL][,tags_remove:=NULL]
    
    # Report missing values by variable_name
    missing_counts <- data[, .(
      missing_coding_system = sum(is.na(coding_system) | coding_system == ""),
      missing_code = sum(is.na(code) | code == ""),
      missing_tags = sum(is.na(tags) | tags == "")
    ), by = variable_name]
    
    # **Keep only records where at least one column has missing values**
    missing_counts <- missing_counts[
      missing_coding_system > 0 | missing_code > 0 | missing_tags > 0
    ]
    # Store missing values report
    final_missing_report <- missing_counts
    
    # Remove rows where Code or Coding System is empty
    cleaned_data <- data[!(is.na(code) | code == "" | is.na(coding_system) | coding_system == "")]
    
    # Trim spaces from Code, Coding System, and Tags
    cleaned_data[, `:=`(
      code = trimws(code),
      coding_system = trimws(coding_system),
      tags = trimws(tags)
    )]
    
    # **Remove all special characters & spaces from coding_system** (Keep only letters & numbers)
    cleaned_data[!coding_system %in% c("SNOMEDCT_US", "Free_text", "SNOMED_US", "ITA_procedures_coding_system"), coding_system := gsub("[^A-Za-z0-9]", "", coding_system)]
    
    # **Replace commas (`,`) with dots (`.`) in the `code` column**
    cleaned_data[, code := gsub(",", ".", code)]
    
    # Identify codes containing scientific notation (e.g., "1.23e+10", "5.67e-5")
    incorrect_codes <- cleaned_data[grepl("e\\+", code, ignore.case = TRUE)]
    incorrect_codes<-incorrect_codes[,c("code", "variable_name")]
    
    # **Remove duplicate rows**
    cleaned_data <- unique(cleaned_data)
    
    # **Fix tags**
    cleaned_data[, tags := tolower(tags)]  # Convert to lowercase
    cleaned_data[tags == "broad", tags := "possible"]
    
    # **Approximate string matching to fix misspellings in tags**
    narrow_matches <- cleaned_data[!duplicated(tags), tags][agrep("narrow", cleaned_data[!duplicated(tags), tags])]
    possible_matches <- cleaned_data[!duplicated(tags), tags][agrep("possible", cleaned_data[!duplicated(tags), tags])]
    broad_matches <- cleaned_data[!duplicated(tags), tags][agrep("broad", cleaned_data[!duplicated(tags), tags])]
    exclude_matches <- cleaned_data[!duplicated(tags), tags][agrep("excl", cleaned_data[!duplicated(tags), tags])]
    delete_matches <- cleaned_data[!duplicated(tags), tags][agrep("delete", cleaned_data[!duplicated(tags), tags])]
    ignore_matches <- cleaned_data[!duplicated(tags), tags][agrep("ignore", cleaned_data[!duplicated(tags), tags])]
    
    # Remove correct values to avoid overwriting them
    narrow_matches <- narrow_matches[!narrow_matches %in% "narrow"]
    possible_matches <- possible_matches[!possible_matches %in% "possible"]
    ignore_matches <- ignore_matches[!ignore_matches %in% "ignore"]
    
    # Apply corrections
    if (length(narrow_matches) > 0) {
      cleaned_data[tags %in% narrow_matches, tags := "narrow"]
    }
    if (length(possible_matches) > 0) {
      cleaned_data[tags %in% possible_matches, tags := "possible"]
    }
    if (length(broad_matches) > 0) {
      cleaned_data[tags %in% broad_matches, tags := "possible"]
    }
    if (length(exclude_matches) > 0) {
      cleaned_data[tags %in% exclude_matches, tags := "possible"]
    }
    if (length(delete_matches) > 0) {
      cleaned_data[tags %in% delete_matches, tags := "possible"]
    }
    if (length(ignore_matches) > 0) {
      cleaned_data[tags %in% ignore_matches, tags := "possible"]
    }
    
    
    cleaned_data<-cleaned_data[!(is.na(tags) | tags == "")]
    
    cleaned_data<-cleaned_data[tags %in% c("narrow", "possible")]
    
    # **Check for range codes (excluding "SNM")**
    range_codes <- cleaned_data[!coding_system %in% c("SNM", "SNMI") & grepl("-", code)]
    range_codes<-range_codes[,c("coding_system", "code", "variable_name")]
    
    cleaned_data <- cleaned_data[!(grepl("-", code) & !coding_system %in% c("SNM", "SNMI"))]
    cols<-c("coding_system", "code", "code_name", "concept", "concept_name", "tags", "variable_name", "event_definition")
    cleaned_data<-cleaned_data[,cols,with=F]
    
    
    cross_check_tsv<-merge_tsv_files(folder_path = cross_check)
    cross_check_tsv<-cross_check_tsv[,c("Coding system", "Code", "Code name"), with=F]
    names(cross_check_tsv)<-c("coding_system", "code", "code_name")
    
    xls_files <- list.files(cross_check, pattern = "\\.xlsx$", full.names = TRUE)
    xls_fl<-read_excel(xls_files, col_types = "text")
    xls_fl<-xls_fl[,c("Coding system", "Code", "Code name"), with=F]
    names(xls_fl)<-c("coding_system", "code", "code_name")
    
    cross_check_tsv<-rbind(cross_check_tsv, xls_fl)
    
    cross_check_tsv<-cross_check_tsv[coding_system %in% c("SCTSPA", "SNOMEDCT_US", "SNOMED", "MEDCODEID", "SCTSPA_SNS", "SPA_EXT")]
    cross_check_tsv<-cross_check_tsv[code != "-"]
    cross_check_tsv[, nchar:=nchar(code)]
    cross_check_tsv<-cross_check_tsv[nchar>15]
    setnames(cross_check_tsv, "code", "codemapper_code")
    
    cleaned_data<-merge.data.table(cleaned_data, cross_check_tsv, by=c("coding_system", "code_name"), all.x=T)
    cleaned_data[!is.na(codemapper_code), code:=codemapper_code]
    cleaned_data[,codemapper_code:=NULL][,nchar:=NULL]
    cleaned_data <-unique(cleaned_data)
    
    
    #### Fix coding system #####
    cleaned_data[coding_system == "SNOMED", coding_system:="SNOMEDCT_US"]
    cleaned_data[coding_system == "SNOMED_US", coding_system:="SNOMEDCT_US"]
    cleaned_data[coding_system == "Free_text", coding_system:="free_text"]
    cleaned_data[coding_system == "ICD10PC", coding_system:="ICD10PCS"]
    cleaned_data[coding_system == "SCTSPASNS", coding_system:="SCTSPA_SNS"]
    cleaned_data <-unique(cleaned_data)
    
    #Copy all ICD9PCS to ICD9PROC and vice versa and then remove duplicates
    icd9pcs<-cleaned_data[coding_system == "ICD9PCS"]
    if(nrow(icd9pcs)>0){
    icd9pcs[,coding_system:="ICD9PROC"]
    }else{icd9pcs<-NULL}
    icd9proc<-cleaned_data[coding_system == "ICD9PROC"]
    if(nrow(icd9proc)>0){
    icd9proc[,coding_system:="ICD9PCS"]
    }else{icd9proc<-NULL}
    icd9extra<-rbind(icd9pcs, icd9proc)
    if(!is.null(icd9extra)){
      cleaned_data<-rbind(cleaned_data, icd9extra)
      cleaned_data <- cleaned_data[!duplicated(cleaned_data[, .(code, coding_system, tags, variable_name)])]
    }
    
    
    # **Check for rounding errors (codes with 15+ characters ending in "000" or "00")**
    cleaned_data[, char := nchar(code)]
    
    cleaned_data[coding_system %in% c("SCTSPA", "SNOMEDCT_US", "SNOMED", "MEDCODEID", "SCTSPA_SNS", "SPA_EXT"),
                 last_characters := as.character(str_sub(code, -2))]
    
    cleaned_data[coding_system %in% c("SCTSPA", "SNOMEDCT_US", "SNOMED", "MEDCODEID", "SCTSPA_SNS", "SPA_EXT"),
                 last_characters_3 := as.character(str_sub(code, -3))]
    
    # **First check: Codes with 15+ characters ending in "000"**
    cleaned_data[last_characters_3 == "000" & char > 15, error_rounding := TRUE]
    
    rounding_error <- cleaned_data[error_rounding == TRUE]
    rounding_error<-rounding_error[,c("variable_name", "code", "coding_system"), with=F]
    
    cleaned_data[last_characters_3 == "000" & char > 15, present_in_both := TRUE]
    cleaned_data[, `:=`(error_rounding = NULL, last_characters_3 = NULL)]
    
    # **Second check: Codes with 15+ characters ending in "00"**
    cleaned_data[last_characters == "00" & char > 15, error_rounding := TRUE]
    cleaned_data[present_in_both == TRUE, error_rounding := NA]
    cleaned_data[, present_in_both := NULL]
    
    rounding_error_p <- cleaned_data[error_rounding == TRUE]
    rounding_error_p<-rounding_error_p[,c("variable_name", "code", "coding_system"), with=F]
    
    setcolorder(cleaned_data, c("variable_name","event_definition", "code", "coding_system","code_name", "concept", "concept_name", "tags"))
    cleaned_data<-cleaned_data[,c("variable_name","event_definition", "code", "coding_system","code_name", "concept", "concept_name", "tags"), with=F]
    # Splitting into three columns
    cleaned_data[, c("system", "event_abbreviation", "type") := tstrsplit(variable_name, "_", fixed = TRUE)]
    
    # Save cleaned data
    cleaned_dir <- file.path(projectFolder, "Cleaned_Codelist")
    if (dir.exists(cleaned_dir)) unlink(cleaned_dir, recursive = TRUE)  # Clear if exists
    dir.create(cleaned_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Save missing values report
    missing_report_path <- file.path(cleaned_dir, "missing_values_report.csv")
    fwrite(final_missing_report, missing_report_path)
    
    # Save incorrect values report
    incorrect_path <- file.path(cleaned_dir, "incorrect_codes.csv")
    fwrite(incorrect_codes, incorrect_path)
    
    if (range_codes[,.N] > 0) {
      fwrite(range_codes, file.path(cleaned_dir, "range_codes.csv"))
    }
    
    if (rounding_error[,.N] > 0) {
      fwrite(rounding_error, file.path(cleaned_dir, "error_rounding_list.csv"))
    }
    
    if (rounding_error_p[,.N] > 0) {
      fwrite(rounding_error_p, file.path(cleaned_dir, "possible_error_rounding_list.csv"))
    }
    
  
      ####Create the pregnancy codelist for the Pregnancy algorithm####
      preg_algorithm<-cleaned_data[type=="PrA"]
      preg_path <- file.path(cleaned_dir, paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"), "_",study_name, "_pregnancy_algorithm_codelist.csv"))
      if("unique_tags" %in% names(preg_algorithm)){preg_algorithm[,unique_tags:=NULL]}
      fwrite(preg_algorithm, preg_path)

      
      cleaned_data<-cleaned_data[variable_name %in% study_data[,variable_name]]
      # Create summary table
      # Identify all vocabularies present in the dataset
      all_vocabularies <- unique(cleaned_data$coding_system)
      
      # Create summary table with missing vocabularies
      summary_dt <- cleaned_data[, .(
        unique_coding_systems = paste(unique(coding_system), collapse = ", "),
        unique_tags = paste(unique(tags), collapse = ", "),
        missing_vocabulary = paste(setdiff(all_vocabularies, unique(coding_system)), collapse = ", ")
      ), by = variable_name]
     sum_path <- file.path(cleaned_dir, paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"), "_",study_name, "_summary.csv"))
      fwrite(summary_dt, sum_path)
      
      # Create summary table with missing vocabularies
      summary_dt_tags <- cleaned_data[, unique_tags := toString(unique(na.omit(tags))), by = c("variable_name", "coding_system")]
      summary_dt_tags<-summary_dt_tags[,c("variable_name", "coding_system", "unique_tags"), with=F]
      summary_dt_tags<-unique(summary_dt_tags)
      sum_tags_path <- file.path(cleaned_dir, paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"), "_",study_name, "_tags_details.csv"))
      fwrite(summary_dt_tags, sum_tags_path)
      
      #remove Pra from final codelist
      cleaned_data<-cleaned_data[type != "PrA"]
  
      cleaned_path <- file.path(cleaned_dir, paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"), "_", study_name, "_full_codelist.csv"))
      if("unique_tags" %in% names(cleaned_data)){cleaned_data[,unique_tags:=NULL]}
      fwrite(cleaned_data, cleaned_path)
      
    return(message = "Processing completed. Cleaned data and missing values report saved.")

  }
  
   

