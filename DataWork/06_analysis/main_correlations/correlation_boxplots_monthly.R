# Correlation Boxplots: Monthly

# Load/prep data ---------------------------------------------------------------
gtrends_monthly_df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                                        "gtrends_full_timeseries", "correlation_datasets",
                                        "gtrends_monthly_correlations.Rds"))

gtrends_monthly_df <- gtrends_monthly_df %>%
  dplyr::filter((begin_date == ymd("2020-01-01") & end_date == ymd("2020-12-31")) |
                  (begin_date == ymd("2021-01-01") & end_date == ymd("2021-12-31")) | 
                  (begin_date == ymd("2022-01-01") & end_date == ymd("2022-12-31"))) %>%
  dplyr::mutate(year = begin_date %>% year())

gtrends_monthly_df <- gtrends_monthly_df %>%
  dplyr::select(geo, keyword_en, year, cor_excess, cor_cases) %>%
  pivot_longer(cols = c(cor_excess, cor_cases)) %>%
  dplyr::mutate(name = case_when(
    name == "cor_excess" ~ "Excess Mortality",
    name == "cor_cases" ~ "COVID Cases"
  )) %>%
  dplyr::filter(keyword_en %in% KEYWORDS_SYMTPOMS) %>%
  mutate(keyword_en = keyword_en %>% 
           tools::toTitleCase() %>% 
           str_replace_all("\\bi\\b", "I") %>%
           fct_rev(),
         year = year %>% factor(levels = c("2022", "2021", "2020")) %>%
           fct_rev()) 

p <- gtrends_monthly_df %>%
  ggplot(aes(x = value, y = keyword_en, fill = name)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_boxplot() +
  facet_wrap(~year) +
  labs(x = "Correlation",
       y = NULL,
       fill = "Variable Correlating\nwith Google Search\nInterest") +
  scale_fill_manual(values = c("darkorange", "dodgerblue2")) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"))

ggsave(p, filename = file.path(paper_figures, "cases_excess_boxplot.png"),
       height = 6, width = 7)



