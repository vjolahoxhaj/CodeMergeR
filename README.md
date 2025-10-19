
# 📌 CodeMergeR App

## 📝 Overview

This **Shiny application** facilitates the **validation, processing, and
cleaning** of codelists for medical studies. It automates checks on
**folder structures, file names, Excel sheet formats, and mandatory
column requirements**, ensuring **data integrity** before generating
clean, validated codelists.

------------------------------------------------------------------------

## ✨ Features

-   ✅ **Validation**: Ensures proper **naming conventions** and
    required **files**.
-   🔍 **Processing**: Extracts and merges **codelists** from structured
    folders.
-   🧹 **Cleaning**: Identifies and removes errors such as **scientific
    notation (E+), incorrect ranges, and rounding issues**.
-   📊 **Interactive Reports**: Displays **missing and available study
    conditions, validation results, and detected issues**.
-   📥 **Downloadable Output**: Saves **processed and cleaned
    codelists** in structured directories.

------------------------------------------------------------------------

## 🚀 How to Use

1️⃣ **Upload** the **Concepts Folder (.zip)** and **Mother Excel File
(.xlsx)**.\
2️⃣ Click **Run Validation** ✅ to check **file structure and data
integrity**.\
3️⃣ Click **Run Presence and Absence of Variables** 🔍 to confirm
**required study conditions**.\
4️⃣ Click **Run Processing** 📊 to **extract and merge codelists**.\
5️⃣ Click **Run Cleaning** 🧹 to **remove errors and finalize the
codelist**.\
6️⃣ **Download** 📥 the **final codelist** from the **Cleaned Codelist**
directory.

------------------------------------------------------------------------

## 📦 Dependencies

Packages used in the app:

``` r
install.packages(c("stringr", "data.table", "knitr", "DT", "shiny", "shinyjs", "rstudioapi", "readxl", "htmltools", "utils", "tinytex", "shinyscreenshot"))
```
------------------------------------------------------------------------

## ▶️ Run the app

Run the application using the following command:

- Open the `torun.R` script.     
- Select all script.     
- Press Run.      
- The shiny webpage will open.     
- Follow the instructions in the app.      
- Make sure to wait a few second in between the actions, as it takes a while to process the data.      
- A web screenshot capture button is provided for each tab. Finish running the script (i.e., for each section there are results present) then capture the screenshots.      
- Save the screenshots in the `Cleaned codelist` folder.    

## 📂 Directory structure

📁 Concepts/ → Contains study clinical concepts files.     
📁 Algorithms/ → Contains algorithm files.     
📁 Errors/ → Stores logs and missing files
report.      
📁 Available/ → Holds validated folders after processing.      
📁Cleaned Codelist/ → Final cleaned and processed codelists.     
📁 Extra files/ → Codelist for Pedianet exemption codes and correct rounding
codes.     
📁 helper_functions/ → Stores all neccessary functions to run the
app.     
    
💡 For more details, check the documentation or the helper functions
inside the 📂 helper_functions/ directory.


## Expected error files

This are files that will be produced by the app if errors are identified:
- conceptsets_name_check: the name of the concepts folder doesn't contain 3 underscores      
- algorithms_name_check: the name of the algorithms folder doesn't contain 3 underscores     
- no_code_covariates_conceptsets: the folders has been tagged in the 'Covariates with no codes' column but has been already mapped      
- no_files_excel_conceptsets: there are zero or multiple excel files in the concept folder      
- no_files_excel_algorithms: there are zero or multiple excel files in the algorithms folder     
- incorrect_codes: codes containing scientific notations (E+)      
- range_codes: codes containing dashes (-)      
- missing_values_report: a report of rows with missing codes, coding_system or tags 
- error_rounding_list: a code with a length of 15 or more character ending in '000'      
- possible_error_rounding_list: a code with a length of 15 or more character ending in '00'      



