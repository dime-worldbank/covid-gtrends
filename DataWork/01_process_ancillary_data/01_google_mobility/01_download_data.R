# Download Google Mobility Data

OUT_PATH <- file.path(gmobility_dir, "FinalData", "Global_Mobility_Report.Rds")

if(!file.exists(OUT_PATH)){
  
  gm_df <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
  saveRDS(gm_df, OUT_PATH)
  
}
