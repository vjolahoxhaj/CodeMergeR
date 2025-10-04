
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

1️⃣ **Upload** the **Codelist Folder (.zip)** and **Mother Excel File
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

📁 Conditions/ → Contains study condition files.     
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
