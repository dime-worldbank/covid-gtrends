# Excess Deaths vs Reported Cases

# Load data --------------------------------------------------------------------
df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                        "gtrends_full_timeseries", 
                        "gtrends_otherdata_complete_monthly.Rds"))

cor_df <- df %>%
  distinct(date, geo, .keep_all = T) %>%
  group_by(geo) %>%
  summarise(cor = cor(excess_mean, cases_new),
            cases_total = max(cases_total),
            income = income[1]) %>%
  ungroup()

# Histogram --------------------------------------------------------------------
cor_df %>%
  ggplot() +
  geom_histogram(aes(x = cor),
                 fill = "dodgerblue",
                 color = "black") +
  labs(x = "Correlation Between Reported\nCOVID-19 Cases and Excess Mortality",
       y = "N Countries") +
  theme_classic2()

ggsave(filename = file.path(paper_figures, "cor_cases_excess.png"),
       height = 3, width = 5)





