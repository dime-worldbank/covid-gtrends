# Make Monthly Dataset with Excess Mortality

# Load/clean excess mortality data ---------------------------------------------
ex_mort_df <- read_excel(file.path(ex_mort_dir, "RawData", "WHO_COVID_Excess_Deaths_EstimatesByCountry.xlsx"), 2, skip = 12)

ex_mort_clean_df <- ex_mort_df %>%
  dplyr::filter(!is.na(iso3)) %>%
  mutate(geo = iso3 %>% countrycode(origin = 'iso3c', destination = 'iso2c'),
         date = paste(year, "-", month, "-01") %>% ymd()) %>%
  clean_names() %>%
  dplyr::select(geo, date, expected_mean, acm_mean, excess_mean) %>%
  dplyr::rename(excess_expected_mean = expected_mean)

# Make monthly data ------------------------------------------------------------
df_append <- map_df(c("symptoms", "contain"), function(keyword_type){
  print(paste(keyword_type, "================================================"))
  
  #### Load data
  df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                          "gtrends_full_timeseries", 
                          paste0("gtrends_otherdata_complete_",keyword_type,".Rds")))
  
  #### Aggregate to monthly
  df_monthly <- df %>%
    dplyr::filter(date >= ymd("2020-01-01"),
                  date <= ymd("2022-12-01")) %>%
    
    group_by(geo) %>%
    mutate(cases_total = max(cases, na.rm = T)) %>%
    ungroup() %>%
    
    dplyr::mutate(date = date %>% floor_date(unit = "month")) %>%
    group_by(date, geo, keyword_en,
             wb_region, continent, income) %>%
    dplyr::summarise(cases_new = sum(cases_new, na.rm = T),
                     death_new = sum(death_new, na.rm = T),
                     hits = mean(hits, na.rm = T),
                     gdp_pc = mean(gdp_pc, na.rm = T),
                     cases_total = max(cases_total, na.rm = T),
                     per_pop_using_internet = mean(per_pop_using_internet, na.rm = T),
                     mobile_cell_sub_per100 = mean(mobile_cell_sub_per100, na.rm = T)) %>%
    ungroup() 
  
  df_monthly$cases_total[df_monthly$cases_total %in% -Inf] <- NA
  
  #### Merge with mortality
  df_monthly <- df_monthly %>%
    inner_join(ex_mort_clean_df, by = c("geo", "date"))
  
  return(df_monthly)
})


#### Export
saveRDS(df_append,
        file.path(data_dir, "google_trends", "FinalData",
                  "gtrends_full_timeseries", 
                  "gtrends_otherdata_complete_monthly.Rds"))

