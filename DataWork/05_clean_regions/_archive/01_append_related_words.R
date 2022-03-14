# Append and Clean Google Trends Data

# Load Data --------------------------------------------------------------------
region_folders <- file.path(dropbox_file_path, "Data", "google_trends", "RawData") %>%
  list.files("timeseries_regions") %>%
  str_subset("2021-03-01_2021-05-31|2020-12-01_2021-05-31")

region_df <- map_df(region_folders, function(region_folder_i){
  
  df_date_i <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                         region_folder_i) %>%
    list.files(pattern = "*.Rds", full.names = T) %>%
    map_df(function(path){
      df <- readRDS(path)
      df <- df$interest_by_region
      df$time_span <- region_folder_i %>% 
        str_replace_all("timeseries_regions_", "")
      
      return(df)
    }) 
  
  return(df_date_i)
})

# Merge in English Version of Keyword ------------------------------------------
# keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
#                               "keywords", "FinalData","covid_keywords_alllanguages_clean.Rds"))
# 
# keywords$keyword_en <- keywords$keyword_en %>% str_replace_all("'", "") %>% tolower()
# region_df$keyword   <- region_df$keyword %>% str_replace_all("'", "") %>% tolower()
# 
# #### Dataset with english and translated word
# keywords <- keywords %>%
#   dplyr::select(names(keywords) %>% str_subset("keyword")) %>%
#   dplyr::mutate(keyword = keyword_en) %>%
#   pivot_longer(cols = -c(keyword)) %>%
#   dplyr::rename(keyword_en = keyword) %>%
#   dplyr::rename(keyword = value) %>%
#   dplyr::select(keyword_en, keyword) %>%
#   
#   mutate(keyword = keyword %>% tolower(),
#          keyword_en = keyword_en %>% tolower())
# 
# region_df <- merge(region_df, keywords, by = "keyword", all.x=T, all.y=F)

region_df <- region_df %>%
  mutate(keyword_en = keyword)

# Export -----------------------------------------------------------------------
saveRDS(region_df,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                  "gtrends_regional",
                  "gtrends_regional.Rds"))

