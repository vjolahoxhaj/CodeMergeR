library(readxl)
library(dplyr)
library(writexl)


folder_path<-"/Users/vhoxhaj/Desktop/Codelist/Drugs & Drug Proxies definitions forms & Algorithms/"
all_entries <- list.files(folder_path, full.names = TRUE)
folder_all <- all_entries[dir.exists(all_entries) & grepl("^DP_", basename(all_entries))]

df_list <- list()

for (folder in folder_all) {
  folder_name <- basename(folder)
  
  # Split folder name to extract metadata
  parts <- strsplit(folder_name, "_")[[1]]
  if (length(parts) >= 3) {
    system <- parts[1]
    event_abbreviation <- parts[2]
    type <- paste(parts[3:length(parts)], collapse = " ")
  } else {
    warning("Unexpected folder format: ", folder_name)
    next
  }
  
  # Find Excel files
  excel_files <- list.files(folder, pattern = "\\.xlsx?$", full.names = TRUE)
  
  if (length(excel_files) > 0) {
    file <- excel_files[1]
    message("Reading: ", file)
    
    df <- read_excel(file) %>%
      select(any_of(c("product_identifier", "code", "product_name", "tags"))) %>%
      dplyr::mutate(system = system,
             event_abbreviation = event_abbreviation,
             type = type)
    
    df_list[[length(df_list) + 1]] <- df
  }
}

# Combine and export
combined_df <- bind_rows(df_list)
combined_df<-as.data.table(combined_df)
combined_df[,event_definition:=paste(system, event_abbreviation, type, sep="_")]
combined_df[,drug_abbreviation:=paste(system, event_abbreviation, sep="_")]


#study specific
study_name<-"ADEPT"
if(!is.null(study_name)){
  excel<-read_excel("/Users/vhoxhaj/Desktop/Codelist/VAC4EU PASS CovidVaccineMonitoringVariables_2025_w16.xlsx", sheet = "CDM_Medicines", col_types = "text")
  setDT(excel)
  excel_data<-excel[,.(Drug_abbreviation,
                       study_name = get(study_name))]
  excel_data<- excel_data[tolower(study_name) == "yes"]
  
  combined_df<-combined_df[drug_abbreviation %in% excel_data[,Drug_abbreviation]]
}
folder_out<-"/Users/vhoxhaj/Desktop/Codelist/"
fwrite(combined_df, file.path(folder_out, "ADEPT_drug_proxies_full_codelist.csv"))

