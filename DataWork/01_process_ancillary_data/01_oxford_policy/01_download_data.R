# Download Oxford COVID-19 Policy Tracker Dataset

OUT_PATH <- file.path(oxpol_dir, "FinalData", "OxCGRT_compact_national_v1.Rds")

if(!file.exists(OUT_PATH)){
  ox_df <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv")
  
  saveRDS(ox_df, OUT_PATH)
}