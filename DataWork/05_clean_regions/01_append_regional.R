# Append and Clean Google Trends Data

# Load Data --------------------------------------------------------------------
region_files <- file.path(gtrends_dir, "RawData", "timeseries_regions") %>%
  list.files(pattern = "*.Rds",
             full.names = T) #%>%
  #str_subset("2021-03-01 2021-05-31|2020-12-01 2021-05-31")

region_df <- map_df(region_files, function(region_file_i){
  
  region_obj_i <- readRDS(region_file_i)
  
  df_i <- region_obj_i$interest_by_region
  
  df_i$time_span <- region_file_i %>%
    str_replace_all(".*/", "") %>%
    str_replace_all("gtrends_date", "") %>%
    str_replace_all("_isoUS.*", "")
  
  return(df_i)
})

region_df <- region_df %>%
  mutate(keyword_en = keyword)

# Export -----------------------------------------------------------------------
saveRDS(region_df,
        file.path(gtrends_dir, "FinalData",
                  "gtrends_regional",
                  "gtrends_regional.Rds"))

