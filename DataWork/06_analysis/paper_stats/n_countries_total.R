# N Countries in Sample

cor_df <- bind_rows(
  readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2021-12-31_symptoms.Rds")), 
  
  readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2021-12-31_contain.Rds")),
  
  readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2021-12-31_vaccine.Rds")) 
)

cor_df <- cor_df %>%
  dplyr::filter(type %in% "Cases")

# N Countries
cor_df %>%
  pull(geo) %>%
  unique() %>%
  length()

# N Keywords
cor_df %>%
  pull(keyword_en) %>%
  unique() %>%
  length()


