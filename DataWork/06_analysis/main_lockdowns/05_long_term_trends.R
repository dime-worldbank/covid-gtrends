# Long Term Trends in Indicators

# Functions --------------------------------------------------------------------
prep_keywords <- function(df){
  
  df <- df %>%
    dplyr::mutate(keyword_type = case_when(
      keyword_en %in% c("anxiety",
                        "anxiety attack",
                        "anxiety symptoms",
                        "boredom",
                        "hysteria",
                        "insomnia",
                        "loneliness",
                        "lonely",
                        "panic",
                        "social isolation",
                        "overwhelmed",
                        "suicide") ~ "Mental Health",
      
      keyword_en %in% c("debt",
                        "file for unemployment",
                        "unemployment",
                        "unemployment benefits",
                        "unemployment insurance",
                        "unemployment office") ~ "Economic",
      
      keyword_en %in% c("divorce",
                        "emergency pill",
                        "pregnancy test",
                        "wedding") ~ "Relationships\n&Family Planning",
      
      keyword_en %in% c("social distance",
                        "stay at home") ~ "Social Distancing"
    )) %>%
    dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase()) %>%
    dplyr::mutate(keyword_en_newline = case_when(
      keyword_en == "File for Unemployment" ~ "File for\nUnemployment",
      keyword_en == "Unemployment Benefits" ~ "Unemployment\nBenefits",
      keyword_en == "Unemployment Insurance" ~ "Unemployment\nInsurance",
      keyword_en == "Unemployment Office" ~ "Unemployment\nOffice",
      #keyword_en == "" ~ "",
      TRUE ~ keyword_en
    ))
  
  keyword_factor_order <- df %>%
    distinct(keyword_en, keyword_type) %>%
    arrange(keyword_type, keyword_en) %>%
    pull(keyword_en)
  
  keyword_newline_factor_order <- df %>%
    distinct(keyword_en_newline, keyword_type) %>%
    arrange(keyword_type, keyword_en_newline) %>%
    pull(keyword_en_newline)
  
  df <- df %>%
    dplyr::mutate(keyword_en = factor(keyword_en, levels = keyword_factor_order),
                  keyword_en_newline = factor(keyword_en_newline, levels = keyword_newline_factor_order))
  
  return(df)
}

# Load data --------------------------------------------------------------------
# Timeseries data
gtrends_df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete_contain.Rds"))

# So use same sample as is in did sample
did_df <- readRDS(file.path(data_dir, "google_trends", "FinalData", "results", 
                                  "did_pooled_data.Rds"))

# Restrict sample --------------------------------------------------------------
did_df <- did_df %>%
  mutate(keygeo = paste(keyword_en, geo))

gtrends_df <- gtrends_df %>%
  mutate(keygeo = paste(keyword_en, geo))

gtrends_df <- gtrends_df[gtrends_df$keygeo %in% unique(did_df$keygeo),]

# N Countries data -------------------------------------------------------------
n_country_df <- gtrends_df %>%
  distinct(keyword_en, geo) %>%
  group_by(keyword_en) %>%
  summarise(n_country = n()) %>%
  ungroup() %>%
  prep_keywords() %>%
  select(keyword_en, n_country)

gtrends_df <- gtrends_df %>%
  
  # Standardized hits value
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::group_by(keyword_en, date) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  ungroup() %>%
  
  # Cleanup variables
  prep_keywords() %>%
  
  left_join(n_country_df, by = "keyword_en") %>%
  mutate(keyword_en_newline = paste0(keyword_en_newline, "\n[N = ", n_country, "]"))

p <- gtrends_df %>%
  dplyr::filter(date >= ymd("2019-01-01"),
                date <= ymd("2022-12-31")) %>%
  ggplot(aes(x = date, y = hits_ma7_std, color = keyword_type)) +
  geom_line() +
  geom_vline(xintercept = ymd("2020-01-01"), color = "black") +
  geom_text(aes(x = ymd("2020-08-01"),
                y = 42,
                label = "2020"),
            color = "black") +
  facet_wrap(~keyword_en_newline) +
  labs(x = NULL,
       y = NULL,
       color = NULL) +
  theme_classic2() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 8),
        legend.position = "bottom",
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))

ggsave(p, 
       filename = file.path(paper_figures, "contain_long_trends.png"),
       height = 7.5, width = 10)

