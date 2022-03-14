# Merge Correlation Data with Other Data

#### Parameters
begin_day_i <- "2020-02-01"

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor_since_",begin_day_i,".Rds")))


gtrends_sum_df <- gtrends_df %>%
  mutate(month = date %>% month()) %>%
  group_by(month, geo, keyword_en) %>%
  summarise(hits_adj = mean(hits_adj),
            hits = mean(hits),
            cases = mean(cases))


gtrends_sum_df %>%
  filter(keyword_en %in% "loss of taste") %>%
  filter(month %in% 8) %>%
  ggplot() +
  geom_point(aes(x = cases,
                 y = hits_adj))


head(gtrends_sum_df)


