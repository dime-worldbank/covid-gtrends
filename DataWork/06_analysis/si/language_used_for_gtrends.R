# Figure illustrating creating a consistent time series
# NOTE: Run with R version 3.6; issues in text encoding with R version 4.

# Load data --------------------------------------------------------------------
df_best <- readRDS(file.path(data_dir, 
                             "country_primary_language", 
                             "FinalData",
                             "country_language.Rds"))

gcodes_df <- read_csv(file.path(data_dir, 
                                "country_primary_language", "RawData",
                                "google_language_code.csv"))

# Prep Data --------------------------------------------------------------------
gcodes_df <- gcodes_df %>%
  dplyr::select(language,
                language_code)

gcodes_df$language <- gcodes_df$language %>% str_replace_all("\\(.*", "") %>% str_squish()

df_best <- df_best %>%
  left_join(gcodes_df %>%
              dplyr::rename(language_best = language_code,
                            language_best_name = language),
            by = "language_best") %>%
  left_join(gcodes_df %>%
              dplyr::rename(language_id_1 = language_code,
                            language_name_1 = language),
            by = "language_id_1") %>%
  left_join(gcodes_df %>%
              dplyr::rename(language_id_2 = language_code,
                            language_name_2 = language),
            by = "language_id_2") %>%
  left_join(gcodes_df %>%
              dplyr::rename(language_id_3 = language_code,
                            language_name_3 = language),
            by = "language_id_3") %>%
  left_join(gcodes_df %>%
              dplyr::rename(language_id_4 = language_code,
                            language_name_4 = language),
            by = "language_id_4") %>%
  left_join(gcodes_df %>%
              dplyr::rename(language_id_5 = language_code,
                            language_name_5 = language),
            by = "language_id_5")

df_best$country <- countrycode(df_best$geo, origin = "iso2c", destination = "country.name")

df_best$hits_mean_id_1[is.na(df_best$hits_mean_id_1)] <- 100

df_best$country[nchar(df_best$country) >= 25]



df_best$country[df_best$country %in% "Micronesia (Federated States of)"] <- "Micronesia"
df_best$country[df_best$country %in% "South Georgia & South Sandwich Islands"] <- "S. Georgia & S. Sand. Isl"
df_best$country[df_best$country %in% "British Indian Ocean Territory"] <- "Br. Indian Ocean Terr."
df_best$country[df_best$country %in% "Saint Martin (French part)"] <- "St. Martin (French)"
df_best$country[df_best$country %in% "French Southern Territories"] <- "French S. Terr."
df_best$country[df_best$country %in% "United States Minor Outlying Islands (the)"] <- "US Minor Outlying Isl"

df_best$country <- df_best$country %>% str_replace_all("\\&", "\\\\&")

df_best$country[df_best$country %in% "Heard \\& McDonald Islands"] <- "Heard \\& McDonald Islands"
#df_best$country[df_best$country %in% ""] <- ""
#df_best$country[df_best$country %in% ""] <- ""

# Make Table -------------------------------------------------------------------
df_best <- df_best %>%
  arrange(geo) %>%
  mutate_at(vars(contains("hits_mean")), ~ round(., digits = 1) %>% as.character() %>% replace_na("")) %>%
  mutate_at(vars(contains("language_name")), ~ as.character(.) %>% replace_na("")) %>%
  dplyr::mutate(tex = paste0(country, " & ", 
                             language_best_name, " & ",
                             language_name_1, " (", hits_mean_id_1, ") & ",
                             language_name_2, " (", hits_mean_id_2, ") & ",
                             language_name_3, " (", hits_mean_id_3, ") & ",
                             language_name_4, " (", hits_mean_id_4, ") & ",
                             language_name_5, " (", hits_mean_id_5, ") \\\\ \n ") %>%
                  str_replace_all("\\(\\)", ""))

sink(file.path(paper_tables, "language_most_sa.tex"))
cat("\\begin{longtable}{lc | c | c | c | c | c} \n ")
cat("\\caption{Google search activity across countries and languages} \n")
cat("\\label{tab:sa_language_table} \\\\ \n")
cat("\\hline \n ")
cat(" Country & Language with     & \\multicolumn{5}{c}{Language and Search Activity (S.A.)} \\\\ \n ")
cat(" \\cline{3-7} \n ")
cat("     & Highest S.A. & Name (S.A.) & Name (S.A.) & Name (S.A.) & Name (S.A.) & Name (S.A.) \\\\ \n ")
cat("\\hline \n ")

for(i in 1:nrow(df_best)) cat(df_best$tex[i])

cat(" \\hline \n ")
cat("\\end{longtable} ")
sink()

