
gtrends_df <- bind_rows(
  readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2021-12-31_symptoms.Rds")) %>%
    dplyr::mutate(category = "symptoms"),
  readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2021-12-31_contain.Rds")) %>%
    dplyr::mutate(category = "contain"),
  readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries",
                    "correlation_datasets",
                    "correlations_gtrends_since2020-01-01_until2021-12-31_vaccine.Rds")) %>%
    dplyr::mutate(category = "vaccines")
)

gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(type %in% "Cases") %>%
  group_by(keyword_en, category) %>%
  dplyr::summarise(N = n()) %>%
  dplyr::mutate(keyword_en = keyword_en %>%
                  tools::toTitleCase() %>%
                  str_replace_all("\\bi\\b", "I") %>%
                  str_replace_all("^can ", "Can ") %>%
                  str_replace_all("Dna", "DNA") %>%
                  str_replace_all("^where ", "Where ") %>%
                  str_replace_all("Covid", "COVID")) %>%
  arrange(category, keyword_en) %>%
  dplyr::mutate(tex = paste(keyword_en, " & ", N, " \\\\ \n"))

for(cat_i in unique(gtrends_sum_df$category)){
  
  gtrends_sum_df_i <- gtrends_sum_df[gtrends_sum_df$category %in% cat_i,]
  
  sink(file.path(paper_tables, paste0("terms_queried_n_",cat_i,".tex")))
  cat("\\begin{tabular}{ll} ")
  cat("\\hline ")
  cat("Search Term & N Countries \\\\ \n")
  cat("\\hline ")
  
  for(i in 1:nrow(gtrends_sum_df_i)) cat(gtrends_sum_df_i$tex[i])
  
  cat("\\hline ")
  cat("\\end{tabular} ")
  sink()
  
}





