
scraped_df <- file.path(gtrends_dir, "RawData", "timeseries") %>%
  list.files(pattern = "*.Rds") %>%
  as.data.frame() %>%
  dplyr::rename(file_name = ".")

saveRDS(scraped_df, "~/Desktop/TO_UPLOAD/scraped_files.Rds")

nrow(scraped_df)
