# Google Trends Clean Data
# Master Script

## Filepath to code
CODE_PATH <- file.path(github_file_path, "DataWork", "google_trends", "01_scrape_clean_data", 
                       "scrape_with_reference_state_global")

## Run scripts
source(file.path(CODE_PATH, "02_append_clean.R"))
source(file.path(CODE_PATH, "03_merge_other_data.R"))
source(file.path(CODE_PATH, "04_variable_construction.R"))
source(file.path(CODE_PATH, "05_correlations.R"))





