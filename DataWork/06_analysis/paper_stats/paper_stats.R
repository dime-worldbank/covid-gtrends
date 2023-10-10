# Stats for Paper

# N Countries and Keywords -----------------------------------------------------
cor_df <- bind_rows(
  readRDS(file.path(data_dir, "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2022-12-31_symptoms.Rds")), 
  
  readRDS(file.path(data_dir, "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2022-12-31_contain.Rds"))
)

cor_df <- cor_df %>%
  dplyr::filter(type %in% "Cases",
                !is.na(cor))

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

# Primary Language Dominance ---------------------------------------------------
languages <- readRDS(file.path(prim_lang_dir, 
                               "FinalData",
                               "country_language.Rds"))

## Proportion of countries with one language
languages_1 <- languages %>%
  dplyr::filter(is.na(language_id_2))

languages_2more <- languages %>%
  dplyr::filter(!is.na(language_id_2))

nrow(languages_1) / nrow(languages)

## Dominance
scnd_highest <- function(x){
  rev(sort(x))[2]
}

languages_2more_prop_greater <- languages_2more %>%
  select(geo, contains("hits_mean_id_")) %>%
  pivot_longer(-geo) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(geo) %>%
  dplyr::summarise(hits_max = max(value),
                   hits_2nd = scnd_highest(value)) %>%
  ungroup() %>%
  mutate(perc_greater = (hits_max - hits_2nd) / hits_2nd*100) %>%
  dplyr::filter(perc_greater != Inf)

languages_2more_prop_greater$perc_greater %>% summary()

# Number of days can scrape in Google Trends for a given query -----------------
# Shows that a date range 270 days can be used to query daily data. 

#### Test 1
## Too long of time range for daily
seq.Date(from = ymd("2020-01-01"), to = ymd("2020-09-27"), by = 1) %>% length()
out1 <- gtrends(keyword = "fever", 
                geo = "US",
                time = "2020-01-01 2020-09-27",
                onlyInterest = T,
                low_search_volume=T)
out1$interest_over_time %>% head()

## Daily data
seq.Date(from = ymd("2020-01-01"), to = ymd("2020-09-26"), by = 1) %>% length()
out2 <- gtrends(keyword = "fever", 
                geo = "US",
                time = "2020-01-01 2020-09-26",
                onlyInterest = T,
                low_search_volume=T)
out2$interest_over_time %>% head()

#### Test 2
## Too long of time range for daily
seq.Date(from = ymd("2021-01-01"), to = ymd("2021-09-28"), by = 1) %>% length()
out3 <- gtrends(keyword = "fever", 
                geo = "US",
                time = "2021-01-01 2021-09-28",
                onlyInterest = T,
                low_search_volume=T)
out3$interest_over_time %>% head()

## Daily data
seq.Date(from = ymd("2021-01-01"), to = ymd("2021-09-27"), by = 1) %>% length()
out4 <- gtrends(keyword = "fever", 
                geo = "US",
                time = "2021-01-01 2021-09-27",
                onlyInterest = T,
                low_search_volume=T)
out4$interest_over_time %>% head()
