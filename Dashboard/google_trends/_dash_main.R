# Google Trends Clean Data
# Master Script

## Filepath to code
CODE_PATH <- file.path(github_file_path, "Dashboard", "google_trends")

## Run scripts
source(file.path(CODE_PATH, "01_prepare_data_for_dashboard.R"))
source(file.path(CODE_PATH, "02_stylized_example_gif.R"))
source(file.path(CODE_PATH, "03_data_to_github.R"))



