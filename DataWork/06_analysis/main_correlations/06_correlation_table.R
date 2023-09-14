# What variables explain correlation?

keywords_en_use <- KEYWORDS_SYMTPOMS

# Load Data --------------------------------------------------------------------
cor_1_df <- bind_rows(readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2020-01-01_until2020-12-31_symptoms.Rds")),
                      readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2020-01-01_until2020-12-31_contain.Rds")),
                      readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2020-01-01_until2020-12-31_vaccine.Rds"))
) %>%
  dplyr::mutate(date_since = "2020")

cor_2_df <- bind_rows(readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2021-01-01_until2021-12-31_symptoms.Rds")),
                      readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2021-01-01_until2021-12-31_contain.Rds")),
                      readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2021-01-01_until2021-12-31_vaccine.Rds"))
) %>% 
  dplyr::mutate(date_since = "2021")

cor_3_df <- bind_rows(readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2022-01-01_until2022-12-31_symptoms.Rds")),
                      readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2022-01-01_until2022-12-31_contain.Rds")),
                      readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries",
                                        "correlation_datasets",
                                        "correlations_gtrends_since2022-01-01_until2022-12-31_vaccine.Rds"))
) %>% 
  dplyr::mutate(date_since = "2022")

# cor_3_df <- bind_rows(readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
#                                         "gtrends_full_timeseries",
#                                         "correlation_datasets",
#                                         "correlations_gtrends_since2020-01-01_until2021-12-31_symptoms.Rds")),
#                       readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
#                                         "gtrends_full_timeseries",
#                                         "correlation_datasets",
#                                         "correlations_gtrends_since2020-01-01_until2021-12-31_contain.Rds")),
#                       readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
#                                         "gtrends_full_timeseries",
#                                         "correlation_datasets",
#                                         "correlations_gtrends_since2020-01-01_until2021-12-31_vaccine.Rds"))
# ) %>% 
#   dplyr::mutate(date_since = "2020_2021")

# Prep Data --------------------------------------------------------------------
cor_df <- bind_rows(cor_1_df,
                    cor_2_df,
                    cor_3_df) %>%
  dplyr::filter(type %in% "Cases") %>%
  dplyr::filter(keyword_en %in% keywords_en_use) %>%
  
  # For ranking correlations show terms with highest median correlation
  group_by(keyword_en) %>%
  dplyr::mutate(cor_sort_all = quantile(cor_nolag, 0.95, na.rm=T)) %>%
  ungroup() %>%
  
  # Cleanup variables
  mutate(keyword_en = keyword_en %>% 
           tools::toTitleCase() %>% 
           str_replace_all("\\bi\\b", "I"))

cor_df$keyword_en <- factor(cor_df$keyword_en, 
                            levels=unique(cor_df$keyword_en[order(cor_df$cor_sort_all)]), 
                            ordered=TRUE)

#cor_df$date_since <- cor_df$date_since %>% as.factor() %>% fct_rev

# Table ========================================================================

# Prep Data --------------------------------------------------------------------
cor_sum_df <- bind_rows(
  cor_df %>%
    group_by(date_since, keyword_en) %>%
    dplyr::summarise(min = min(cor, na.rm=T),
                     cor_p0_05 = quantile(cor, 0.05, na.rm=T),
                     cor_p0_25 = quantile(cor, 0.25, na.rm=T),
                     cor_p0_50 = quantile(cor, 0.50, na.rm=T),
                     cor_p0_75 = quantile(cor, 0.75, na.rm=T),
                     cor_p0_95 = quantile(cor, 0.95, na.rm=T),
                     max = max(cor, na.rm=T),
                     N = n()) %>%
    dplyr::mutate(type = "cor"),
  
  cor_df %>%
    group_by(date_since, keyword_en) %>%
    dplyr::summarise(min = min(cor_nolag, na.rm=T),
                     cor_p0_05 = quantile(cor_nolag, 0.05, na.rm=T),
                     cor_p0_25 = quantile(cor_nolag, 0.25, na.rm=T),
                     cor_p0_50 = quantile(cor_nolag, 0.50, na.rm=T),
                     cor_p0_75 = quantile(cor_nolag, 0.75, na.rm=T),
                     cor_p0_95 = quantile(cor_nolag, 0.95, na.rm=T),
                     max = max(cor_nolag, na.rm=T),
                     N = n()) %>%
    dplyr::mutate(type = "cor_nolag"),
  
  cor_df %>%
    #dplyr::filter(zscore >= 3) %>%
    group_by(date_since, keyword_en) %>%
    dplyr::summarise(min = min(lag, na.rm=T),
                     cor_p0_05 = quantile(lag, 0.05, na.rm=T),
                     cor_p0_25 = quantile(lag, 0.25, na.rm=T),
                     cor_p0_50 = quantile(lag, 0.50, na.rm=T),
                     cor_p0_75 = quantile(lag, 0.75, na.rm=T),
                     cor_p0_95 = quantile(lag, 0.95, na.rm=T),
                     max = max(lag, na.rm=T),
                     N = n()) %>%
    dplyr::mutate(type = "lag") 
)

cor_sum_wide_df <- cor_sum_df %>%
  pivot_wider(values_from = c(min,
                              cor_p0_05,
                              cor_p0_25,
                              cor_p0_50,
                              cor_p0_75,
                              cor_p0_95,
                              max,
                              N),
              names_from = date_since) %>%
  arrange(desc(keyword_en)) %>%
  mutate_if(is.numeric, round, 2)

cor_sum_wide_df <- cor_sum_wide_df %>%
  
  dplyr::mutate(tex_2020 = paste(keyword_en,
                                 min_2020,
                                 cor_p0_05_2020,
                                 cor_p0_25_2020,
                                 cor_p0_50_2020,
                                 cor_p0_75_2020,
                                 cor_p0_95_2020,
                                 max_2020,
                                 N_2020,
                                 
                                 sep = " & ")) %>%
  mutate(tex_2020 = paste(tex_2020, " \\\\ \n")) %>%
  
  dplyr::mutate(tex_2021 = paste(keyword_en,
                            min_2021,
                            cor_p0_05_2021,
                            cor_p0_25_2021,
                            cor_p0_50_2021,
                            cor_p0_75_2021,
                            cor_p0_95_2021,
                            max_2021,
                            N_2021,
                            sep = " & ")) %>%
  mutate(tex_2021 = paste(tex_2021, " \\\\ \n")) %>%
  
  dplyr::mutate(tex_2022 = paste(keyword_en,
                                 min_2022,
                                 cor_p0_05_2022,
                                 cor_p0_25_2022,
                                 cor_p0_50_2022,
                                 cor_p0_75_2022,
                                 cor_p0_95_2022,
                                 max_2022,
                                 N_2022,
                                 sep = " & ")) %>%
  mutate(tex_2022 = paste(tex_2022, " \\\\ \n"))

# Table: 2020 ------------------------------------------------------------------
cor_sum_wide_df <- cor_sum_wide_df %>%
  arrange(keyword_en)

sink(file.path(paper_tables, "cor_lag_table_2020.tex"))

cat("\\begin{tabular}{l llllllll} \n")
cat("\\hline \n")
cat("Term & & \\multicolumn{5}{c}{Percentile} &  &  \\\\ \n")
cat(" & Min & 5th & 25th & 50th & 75th & 95th & Max & N \\\\ \n")
cat("\\hline \n")

cat("\\multicolumn{9}{l}{{\\bf Correlation}} \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor_nolag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2020[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l}{{\\bf Correlation using best lag}} \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2020[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l}{{\\bf Lag with best correlation}} \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "lag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2020[i])

cat("\\hline \n")
cat("\\end{tabular} ")

sink()

# Table: 2021 ------------------------------------------------------------------
cor_sum_wide_df <- cor_sum_wide_df %>%
  arrange(keyword_en)

sink(file.path(paper_tables, "cor_lag_table_2021.tex"))

cat("\\begin{tabular}{l llllllll} \n")
cat("\\hline \n")
cat("Term & & \\multicolumn{5}{c}{Percentile} &  &  \\\\ \n")
cat(" & Min & 5th & 25th & 50th & 75th & 95th & Max & N \\\\ \n")
cat("\\hline \n")

cat("\\multicolumn{9}{l}{{\\bf Correlation}}  \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor_nolag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2021[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l}{{\\bf Correlation using best lag}}  \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2021[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l}{{\\bf Lag with best correlation}}  \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "lag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2021[i])

cat("\\hline \n")
cat("\\end{tabular} ")

sink()

# Table: 2022 ------------------------------------------------------------------
cor_sum_wide_df <- cor_sum_wide_df %>%
  arrange(keyword_en)

sink(file.path(paper_tables, "cor_lag_table_2022.tex"))

cat("\\begin{tabular}{l llllllll} \n")
cat("\\hline \n")
cat("Term & & \\multicolumn{5}{c}{Percentile} &  &  \\\\ \n")
cat(" & Min & 5th & 25th & 50th & 75th & 95th & Max & N \\\\ \n")
cat("\\hline \n")

cat("\\multicolumn{9}{l}{{\\bf Correlation}} \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor_nolag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2022[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l}{{\\bf Correlation using best lag}}  \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2022[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l}{{\\bf Lag with best correlation}} \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "lag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex_2022[i])

cat("\\hline \n")
cat("\\end{tabular} ")

sink()

















# Table ------------------------------------------------------------------------
cor_sum_wide_df <- cor_sum_wide_df %>%
  arrange(keyword_en)

sink(file.path(paper_tables, "cor_lag_table.tex"))

cat("\\begin{tabular}{l llllllll | llllllll} \n")
cat("\\hline \n")
cat(" & \\multicolumn{8}{c|}{{\\bf Using data in 2020}} & \\multicolumn{8}{c}{{\\bf Using data in 2021}} \\\\ \n")
cat("Term & & \\multicolumn{5}{c}{Percentile} &  &  &  & \\multicolumn{5}{c}{Percentile} & Max & \\\\ \n")
cat(" & Min & 5th & 25th & 50th & 75th & 95th & Max & N & Min & 5th & 25th & 50th & 75th & 95th & Max & N \\\\ \n")
cat("\\hline \n")

cat("\\multicolumn{9}{l|}{{\\bf Correlation}} & & & & & & & & \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor_nolag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l|}{{\\bf Correlation using best lag}} & & & & & & & & \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "cor",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex[i])

cat("\\hline \n")
cat("\\multicolumn{9}{l|}{{\\bf Lag with best correlation}} & & & & & & & & \\\\ \n")
cor_sum_wide_df_tmp <- cor_sum_wide_df[cor_sum_wide_df$type %in% "lag",]
for(i in 1:nrow(cor_sum_wide_df_tmp)) cat(cor_sum_wide_df_tmp$tex[i])

cat("\\hline \n")
cat("\\end{tabular} ")

sink()

