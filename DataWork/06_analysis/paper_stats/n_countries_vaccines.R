# N Countries with Vaccine Data

gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "gtrends_since2020-01-01_until2021-12-31_vaccine.Rds"))

gtrends_df <- gtrends_df %>%
  dplyr::filter(!is.na(total_vaccinations_per_hundred),
                !is.na(hits))

gtrends_df %>%
  pull(geo) %>%
  unique() %>%
  length()


