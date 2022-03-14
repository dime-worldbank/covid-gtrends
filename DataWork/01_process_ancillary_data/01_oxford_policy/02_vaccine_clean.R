# First date for vaccine things

# Export data ------------------------------------------------------------------
vac_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_vaccine_latest.Rds"))

names(vac_df) <- names(vac_df) %>%
  tolower() %>%
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "") %>%
  str_replace_all(" ", "_")

vac_df <- vac_df %>%
  dplyr::select(geo, date, 
                v1_vaccine_prioritisation_summary,
                v2_vaccine_availability_summary,
                v3_vaccine_financial_support_summary) %>%
  dplyr::rename(v1_vaccine = v1_vaccine_prioritisation_summary,
                v2_vaccine = v2_vaccine_availability_summary,
                v3_vaccine = v3_vaccine_financial_support_summary)

# Export data ------------------------------------------------------------------
saveRDS(vac_df, 
        file.path(oxpol_dir, "FinalData", "OxCGRT_vaccine_national_timeseries.Rds"))


