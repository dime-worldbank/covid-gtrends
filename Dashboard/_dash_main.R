# Google Trends Clean Data
# Master Script

# INSTRUCTIONS: 
# Run covid-gtrends/_main.R before running this script.

# Filepath to code -------------------------------------------------------------
CODE_PATH <- file.path(github_file_path, "Dashboard")

# Run scripts ------------------------------------------------------------------
source(file.path(CODE_PATH, "01_prepare_data_for_dashboard.R"))
source(file.path(CODE_PATH, "02_stylized_example_gif.R"))
#source(file.path(CODE_PATH, "03_data_to_github.R"))

