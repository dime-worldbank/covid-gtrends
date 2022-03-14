# Lockdown Difference-in-Difference Analysis

n_countries_df <- gtrends_df %>%
  dplyr::filter(!is.na(hits_ma7),
                !is.na(days_since_c_policy_yearcurrent)) %>%
  distinct(keyword_en, geo) %>%
  group_by(keyword_en) %>%
  dplyr::summarise(N_countries_keyword = n())

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_c_policy_yearcurrent))

gtrends_sum_df <- gtrends_sum_df %>%
  dplyr::filter(pandemic_time %in% c(0, 1),
                keyword_en %in% KEYWORDS_CONTAIN_USE,
                days_since_c_policy_yearcurrent >= -60,
                days_since_c_policy_yearcurrent <= 60) %>%
  dplyr::select(geo, keyword_en, pandemic_time, days_since_c_policy_yearcurrent, hits_ma7) %>%
  dplyr::mutate(days_since_c_policy_yearcurrent = days_since_c_policy_yearcurrent %>% as.numeric) %>%
  tidyr::complete(geo = unique(gtrends_sum_df$geo), 
                  keyword_en = KEYWORDS_CONTAIN_USE, 
                  days_since_c_policy_yearcurrent = -60:60, 
                  pandemic_time = c(0, 1),
                  fill = list(hits_ma7 = 0)) 

## Delete if no hits or policy data
gtrends_sum_df <- gtrends_sum_df %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(hits_ma7_geoSUM = sum(hits_ma7)) %>%
  ungroup() %>%
  dplyr::filter(hits_ma7_geoSUM > 0)

gtrends_sum_df <- gtrends_sum_df %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::group_by(keyword_en, pandemic_time, days_since_c_policy_yearcurrent) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  #dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase() %>% paste0("\nN Countries = ", N_countries_keyword))
  dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase())

p <- gtrends_sum_df %>% 
  dplyr::mutate(pandemic_time_str = ifelse(pandemic_time == 1,
                                           "Pandemic",
                                           "Pre-Pandemic")) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_line(aes(x = days_since_c_policy_yearcurrent,
                y = hits_ma7_std,
                color = factor(pandemic_time))) +
  labs(color = "Period",
       x = "Days Since Lockdown",
       y = "Average\nSearch\nInterest") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  scale_color_manual(values = c("gray40", "darkorange")) +
  facet_wrap(~keyword_en,
             ncol = 4) 

ggsave(p, filename = file.path(paper_figures, "global_lockdown_trends.png"),
       height = 9, width = 9)
# height = 5, width = 9
