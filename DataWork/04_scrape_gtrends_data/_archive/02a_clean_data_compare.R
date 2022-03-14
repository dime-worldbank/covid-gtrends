# Clean Google Trends Data

library(dplyr) # had other package conflict so run here
# Load Data --------------------------------------------------------------------

#comparison_iso <- "BR-SP"
comparison_iso <- "BR-RJ"

trends_df <- readRDS(file.path(dropbox_file_path, paste0("Data/google_trends/RawData/brazil_extract_extra_words_compare",comparison_iso,".Rds")))


# Clean Variables --------------------------------------------------------------

#### Hits variable
# If variable is "<1", just consider as 0.5 and convert into numeric
trends_df$hits[trends_df$hits %in% "<1"] <- "0.5"
trends_df$hits <- trends_df$hits %>% as.numeric()

#### Date
# Convert into date format
trends_df$date <- trends_df$date %>% as.Date()

# Standardize Everything to BR-SP ----------------------------------------------
trends_df <- trends_df %>%
  dplyr::group_by(iso_search_group, keyword) %>%
  dplyr::mutate(scale_var = max(hits) / max(hits[geo == comparison_iso])) %>%
  
  dplyr::ungroup() %>%
  dplyr::mutate(hits_normalized = hits * scale_var)

## Check standardization
trends_df %>%
  
  # For BR-SP, check standard deviation within the same date and keyword
  dplyr::filter(geo == comparison_iso) %>%
  dplyr::group_by(date, keyword) %>%
  dplyr::summarise(hits_normalized_sd = sd(hits_normalized)) %>%
  
  # Check how well certain variables do
  dplyr::group_by(keyword) %>%
  dplyr::summarise(hits_normalized_sd_min = min(hits_normalized_sd),
            hits_normalized_sd_mean = mean(hits_normalized_sd),
            hits_normalized_sd_max = max(hits_normalized_sd))

# a <- trends_df[trends_df$geo %in% comparison_iso & trends_df$keyword == "volta brasil",]

## Keep BR-SP only in search group 1
trends_df <- trends_df %>%
  filter(!(geo == comparison_iso & iso_search_group != 1))


# Merge with Shapefile ---------------------------------------------------------


# Merge the data above with Shapefile ---------------------------------------------------------

sf_geo_data <- st_as_sf(geo_data)

sf_geo_data <- 
  sf_geo_data %>% 
  mutate(HASC_1 = str_replace(HASC_1, pattern = "[.]", replacement = "-"))

trends_df_geo <- 
  trends_admin_df %>% 
  left_join(sf_geo_data, by = c("geo" = "HASC_1"))

# Export -----------------------------------------------------------------------
saveRDS(trends_df, file.path(dropbox_file_path, paste0("Data/google_trends/FinalData/brazil_extract_clean_compare",comparison_iso,".Rds")))
write.csv(trends_df, file.path(dropbox_file_path, paste0("Data/google_trends/FinalData/brazil_extract_clean_compare",comparison_iso,".csv")), row.names = F)

