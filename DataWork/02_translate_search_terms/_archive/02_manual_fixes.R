# Translate Search Terms

# Load Data --------------------------------------------------------------------
keywords_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                                 "keywords", "FinalData", "covid_keywords_alllanguages.Rds"))

# Manual Fixes -----------------------------------------------------------------
keywords_df$keyword_pt[keywords_df$keyword_en %in% "I can't smell"] <- "perdi o olfato"

saveRDS(keywords_df, 
        file.path(dropbox_file_path, "Data", "google_trends", 
                  "keywords", "FinalData", "covid_keywords_alllanguages_clean.Rds"))

