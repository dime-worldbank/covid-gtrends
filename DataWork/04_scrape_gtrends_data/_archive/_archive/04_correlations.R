# Predict Future Cases

comparison_iso <- "US"

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref",comparison_iso,"_adj_cases.Rds")))

gtrends_df <- gtrends_df[gtrends_df$date >= as.Date("2020-02-01"),]
gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% c("i can't smell", 
                                                      "loss of smell", 
                                                      "fever", 
                                                      "cough",
                                                      "coronavirus",
                                                      "corona symptoms",
                                                      "coronavirus symptoms",
                                                      "covid symptoms",
                                                      "loss of taste",
                                                      "tired"),]

gtrends_df$keyword_en = gtrends_df$keyword_en %>% tools::toTitleCase()
gtrends_df$keyword_en[gtrends_df$keyword_en %in% "i Can't Smell"] <- "I Can't Smell"

# TODO: Why need this?
gtrends_df <- gtrends_df %>%
  distinct(geo, date, keyword_en, .keep_all = T)

# Moving Averages and Changes --------------------------------------------------
gtrends_df <- gtrends_df %>%
  arrange(date) %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = runMean(hits, n = 7)) %>%
  mutate(hits_ma7_lag7 = lag(hits_ma7, 7)) %>%
  mutate(cases_lag7 = lag(cases, 7),
         death_lag7 = lag(death, 7)) %>%
  mutate(cases_total = max(cases, na.rm=T),
         death_total = max(death, na.rm=T)) %>%
  ungroup() %>%
  mutate(hits_ma7_7dayinc = hits_ma7 - hits_ma7_lag7,
         cases_7dayinc = cases - cases_lag7,
         death_7dayinc = death - death_lag7) %>%
  dplyr::select(-hits_ma7_lag7) %>%
  
  dplyr::select(geo, date, Country, keyword_en, 
                hits, hits_ma7, hits_ma7_7dayinc,
                cases, death,
                cases_new, death_new,
                cases_total, death_total,
                cases_7dayinc, death_7dayinc)

# Lags -------------------------------------------------------------------------
vars_to_lag <- c(#"hits_ma7_7dayinc",
                 #"hits_ma7",
                 "hits")

gtrends_df <- lapply(1:25, function(lag_i){
  print(lag_i)
  
  lag_df <- gtrends_df %>%
    dplyr::select(c("date", "geo", "keyword_en", vars_to_lag)) %>%
    arrange(date) %>%
    group_by(geo, keyword_en) %>%
    mutate_at(vars_to_lag, function(x) lag(x, lag_i)) %>%
    rename_if(is.numeric, ~paste0( . , "_lag", lag_i ))
  
  return(lag_df)
}) %>%
  reduce(full_join, by = c("date", "geo", "keyword_en")) %>%
  right_join(gtrends_df, by = c("date", "geo", "keyword_en"))

gtrends_df <- lapply(1:25, function(lag_i){
  print(lag_i)
  
  lag_df <- gtrends_df %>%
    dplyr::select(c("date", "geo", "keyword_en", vars_to_lag)) %>%
    arrange(date) %>%
    group_by(geo, keyword_en) %>%
    mutate_at(vars_to_lag, function(x) lead(x, lag_i)) %>%
    rename_if(is.numeric, ~paste0( . , "_lead", lag_i ))
  
  return(lag_df)
}) %>%
  reduce(full_join, by = c("date", "geo", "keyword_en")) %>%
  right_join(gtrends_df, by = c("date", "geo", "keyword_en"))

gtrends_df <- gtrends_df %>%
  dplyr::rename(#hits_ma7_7dayinc_lead0 = hits_ma7_7dayinc,
                #hits_ma7_lead0 = hits_ma7,
                hits_lead0 = hits)

# Data to Long -----------------------------------------------------------------
hits_vars <- names(gtrends_df) %>% str_subset("hits_") %>%
  str_subset("lead|lag")

gtrends_long_df <- gtrends_df %>%
  dplyr::select(c("Country", "geo", "date", "keyword_en", 
                  "cases_total", "death_total", 
                  "cases_new", "death_new", 
                  "cases_7dayinc", "death_7dayinc", hits_vars)) %>%
  pivot_longer(cols = -c(Country, geo, date, keyword_en, 
                         cases_total, death_total,
                         cases_7dayinc, cases_new,
                         death_7dayinc, death_new)) %>%
  mutate(time_lag = name %>%
           str_replace_all("hits_ma7_7dayinc_", "") %>%
           str_replace_all("hits_ma7_", "") %>%
           str_replace_all("hits_", "") %>%
           str_replace_all("lag", "-") %>%
           str_replace_all("lead", "") %>%
           as.numeric(),
         hits_type = name %>%
           str_replace_all("_lead.*", "") %>%
           str_replace_all("_lag.*", "")) %>%
  dplyr::rename(hits_value = value)

cor_df <- gtrends_long_df %>%
  filter(!is.na(cases_7dayinc),
         !is.na(hits_value)) %>%
  group_by(keyword_en, time_lag, geo, hits_type) %>%
  mutate(cor_cases_new = cor(cases_new, hits_value),
         cor_cases_7dayinc = cor(cases_7dayinc, hits_value),
         cor_death_new = cor(death_new, hits_value),
         cor_death_7dayinc = cor(death_7dayinc, hits_value)) %>%
  ungroup()

# Export -----------------------------------------------------------------------
saveRDS(cor_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                          "global_with_refstate",
                          paste0("gl_gtrends_ref","US","_adj_cases_correlations.Rds")))




if(F){
  
  p <- cor_df %>%
    filter(hits_type %in% "hits") %>%
    ggplot() +
    geom_col(aes(x = time_lag, y = cor_level, fill = cor_level)) + 
    geom_vline(xintercept = 0,
               color = "black") +
    scale_fill_gradient2(low =  "#1A9850",
                         mid = "#FFFFBF",
                         high = "#D73027",
                         midpoint = 0) +
    labs(x = "Time Lag (Days)",
         y = "Correlation") +
    theme_ipsum() +
    facet_wrap(~Country,
               ncol = 2)
  ggsave(p, filename = file.path("~/Desktop/hits.png"),
         height = 30, width=10)
  
  p <- cor_df %>%
    filter(hits_type %in% "hits_ma7") %>%
    ggplot() +
    geom_col(aes(x = time_lag, y = cor_level, fill = cor_level)) + 
    geom_vline(xintercept = 0,
               color = "black") +
    scale_fill_gradient2(low =  "#1A9850",
                         mid = "#FFFFBF",
                         high = "#D73027",
                         midpoint = 0) +
    labs(x = "Time Lag (Days)",
         y = "Correlation") +
    theme_ipsum() +
    facet_wrap(~Country,
               ncol = 2)
  ggsave(p, filename = file.path("~/Desktop/hits_ma7.png"),
         height = 30, width=10)
  
  p <- cor_df %>%
    filter(hits_type %in% "hits_ma7_7dayinc") %>%
    ggplot() +
    geom_col(aes(x = time_lag, y = cor_7dayinc, fill = cor_7dayinc)) + 
    geom_vline(xintercept = 0,
               color = "black") +
    scale_fill_gradient2(low =  "#1A9850",
                         mid = "#FFFFBF",
                         high = "#D73027",
                         midpoint = 0) +
    labs(x = "Time Lag (Days)",
         y = "Correlation") +
    theme_ipsum() +
    facet_wrap(~Country,
               ncol = 2)
  ggsave(p, filename = file.path("~/Desktop/hits_ma7_7dayinc.png"),
         height = 30, width=10)
  
  
  
  
  cor_max_df <- cor_df %>%
    filter(!is.na(cor_level)) %>%
    filter(hits_type == "hits") %>%
    group_by(Country) %>%
    summarise(time_lag_max_cor = time_lag[which.max(cor_level)],
              max_cor = cor_level[which.max(cor_level)])
  
  cor_max_df %>%
    filter(max_cor > 0.2) %>%
    ggplot() +
    geom_histogram(aes(x = time_lag_max_cor),
                   binwidth=4) +
    geom_vline(xintercept = 0)
  
  
  cor_max_df$time_lag_max_cor %>% hist()
  
}