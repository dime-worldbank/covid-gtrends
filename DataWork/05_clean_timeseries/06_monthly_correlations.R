# Correlation Dataset: Monthly

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

cor_na_rm <- function(x, y){
  df <- data.frame(x = x,
                   y = y) %>%
    dplyr::filter(!is.na(x),
                  !is.na(y))
  
  cor(df$x, df$y)
}
cor_na_rm <- Vectorize(cor_na_rm)

df_append <- data.frame(NULL)

begin_day_i = begin_day[1]
end_day_i = end_day[5]
for(begin_day_i in begin_day){
  for(end_day_i in end_day){
    
    print(begin_day_i)
    
    # Load data ----------------------------------------------------------------
    df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                            "gtrends_full_timeseries", 
                            "gtrends_otherdata_complete_monthly.Rds"))
    
    # Prep data ----------------------------------------------------------------
    df <- df %>%
      dplyr::filter(date >= as.Date(begin_day_i),
                    date <= as.Date(end_day_i)) 
    
    df_clean <- df %>%
      ungroup() %>%
      dplyr::group_by(geo, keyword_en,
                      gdp_pc, per_pop_using_internet, mobile_cell_sub_per100, income, wb_region) %>%
      dplyr::summarise(cor_excess = cor(hits, excess_mean),
                       cor_cases  = cor(hits, cases_new),
                       cases_total = max(cases_total, na.rm = T)) %>%
      ungroup() %>%
      dplyr::filter(!is.na(cor_excess),
                    !is.na(cor_cases)) 
    
    df_clean$cases_total[df_clean$cases_total %in% -Inf] <- NA
    
    df_clean$begin_date <- begin_day_i
    df_clean$end_date   <- end_day_i
    
    df_append <- bind_rows(df_append, df_clean)
  }
}

saveRDS(df_append,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                  "gtrends_full_timeseries", "correlation_datasets",
                  "gtrends_monthly_correlations.Rds"))







