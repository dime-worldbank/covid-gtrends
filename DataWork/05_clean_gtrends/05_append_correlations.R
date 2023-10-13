begin_day <- c("2020-01-01",
               "2020-07-01",
               "2021-01-01",
               "2021-07-01",
               "2022-01-01",
               "2022-07-01")

end_day <- c("2020-06-30",
             "2020-12-31",
             "2021-06-30",
             "2021-12-31",
             "2022-06-30",
             "2022-12-31")

begin_day_i <- begin_day[1]
end_day_i   <- end_day[1]
keyword_type_i <- "contain"

dates_df_1 <- data.frame(
  begin_day = c("2020-01-01","2020-07-01", "2021-01-01","2021-07-01", "2022-01-01","2022-07-01"),
  end_day   = c("2020-06-30","2020-12-31", "2021-06-30","2021-12-31", "2022-06-30","2022-12-31")
)

dates_df_2 <- data.frame(
  begin_day = c("2020-01-01","2021-01-01","2022-01-01",  "2020-01-01","2020-01-01",  "2021-01-01"),
  end_day   = c("2020-12-31","2021-12-31","2022-12-31",  "2021-12-31","2022-12-31",  "2022-12-31")
)

dates_df <- bind_rows(
  dates_df_1,
  dates_df_2
)

#for(begin_day_i in begin_day){
#  for(end_day_i in end_day){
for(i in 1:nrow(dates_df)){
    for(keyword_type_i in c("contain", "symptoms")){ 
      for(dataset_type in c("cor", "panel")){
        begin_day_i = dates_df$begin_day[i]
        end_day_i = dates_df$end_day[i]
        
        print(paste(begin_day_i, end_day_i, keyword_type_i, dataset_type))
        
        if(end_day_i < begin_day_i) next
        
        #### Panel
        if(dataset_type == "panel"){
          df <- file.path(data_dir, "google_trends", "FinalData",
                          "gtrends_full_timeseries",
                          "correlation_datasets",
                          "panel_with_correlation_individual_keywords") %>%
            list.files(pattern = "*.Rds",
                       full.names = T) %>%
            str_subset(paste0("since", begin_day_i, "_until", end_day_i, "_", keyword_type_i)) %>%
            map_df(readRDS)
          
          saveRDS(df,
                  file.path(data_dir, "google_trends", "FinalData",
                            "gtrends_full_timeseries",
                            "correlation_datasets",
                            paste0("gtrends_since",begin_day_i,"_until",end_day_i,"_",keyword_type_i,".Rds")))

        }
        
        #### Correlations
        if(dataset_type == "cor"){
          df <- file.path(data_dir, "google_trends", "FinalData",
                          "gtrends_full_timeseries",
                          "correlation_datasets",
                          "correlation_individual_keywords") %>%
            list.files(pattern = "*.Rds",
                       full.names = T) %>%
            str_subset(paste0("since", begin_day_i, "_until", end_day_i, "_", keyword_type_i)) %>%
            map_df(readRDS)
          
          saveRDS(df,
                  file.path(data_dir, "google_trends", "FinalData",
                            "gtrends_full_timeseries",
                            "correlation_datasets",
                            paste0("correlations_gtrends_since",begin_day_i,"_until",end_day_i,"_",keyword_type_i,".Rds")))
        }
        
        rm(df)
        gc()
        
        
      }
    }
  }
#}



