# COVID Case Data

# Load Data --------------------------------------------------------------------
cases_df <- read.csv(file.path(who_covid_dir, 
                               "RawData", "WHO-COVID-19-global-data.csv"),
                     stringsAsFactors = F)

# Reading CSV reads string of "NA" as NA
cases_df$Country_code[cases_df$Country %in% "Namibia"] <- "NA"

# Cleanup ----------------------------------------------------------------------
cases_df <- cases_df %>%
  dplyr::select(Date_reported, Country_code, New_cases, Cumulative_cases, 
                New_deaths, Cumulative_deaths, Country) %>%
  dplyr::rename(cases = Cumulative_cases,
                cases_new = New_cases,
                death = Cumulative_deaths,
                death_new = New_deaths,
                geo = Country_code,
                date = Date_reported,
                country = Country) %>%
  dplyr::mutate(date = date %>% as.Date()) %>%
  
  # Sometimes cases_new is negative; make 0
  dplyr::mutate(cases_new = case_when(
    cases_new < 0 ~ 0L, #0L: integer
    TRUE ~ cases_new
  )) %>%
  dplyr::mutate(death_new = case_when(
    death_new < 0 ~ 0L,
    TRUE ~ death_new
  ))

# Add moving average -----------------------------------------------------------
cases_df <- cases_df %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(geo) %>%
  dplyr::mutate(cases_new_ma7 = runMean(cases_new, n = 7),
                death_new_ma7 = runMean(death_new, n = 7)) %>%
  ungroup()

# Export -----------------------------------------------------------------------
saveRDS(cases_df, file.path(who_covid_dir, "FinalData", "covid.Rds"))







