# Merge Correlation Data with Other Data

# Load Data --------------------------------------------------------------------
cor_02_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "global_with_refstate",
                               paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",
                                      "2020-02-01",
                                      ".Rds")))

cor_02_df <- cor_02_df %>%
  filter(keyword_en %in% "loss of smell")

cor_02_df$cor[cor_02_df$type %in% "Cases"] %>% mean()
cor_02_df$cor[cor_02_df$type %in% "Deaths"] %>% mean()

cor_02_df$lag[cor_02_df$type %in% "Cases"] %>% mean()
cor_02_df$lag[cor_02_df$type %in% "Deaths"] %>% mean()



cor_06_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "global_with_refstate",
                               paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",
                                      "2020-06-01",
                                      ".Rds")))

cor_06_df <- cor_06_df %>%
  filter(keyword_en %in% "loss of smell")

cor_06_df$cor[cor_06_df$type %in% "Cases"] %>% mean()
cor_06_df$cor[cor_06_df$type %in% "Deaths"] %>% mean()
