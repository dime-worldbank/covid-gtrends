# Append and Clean Google Trends Data

comparison_iso <- "US"

# Load Data --------------------------------------------------------------------
gtrends_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                        "global_with_ref_state_by_keyword") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  unique()

# Hits to Numeric --------------------------------------------------------------
gtrends_df$hits[gtrends_df$hits %in% "<1"] <- "1"
gtrends_df$hits <- gtrends_df$hits %>% as.numeric()

#gtrends_df$hits_with_compstate[gtrends_df$hits_with_compstate %in% "<1"] <- "1"
#gtrends_df$hits_with_compstate <- gtrends_df$hits_with_compstate %>% as.numeric()

#gtrends_df$hits_compstate[gtrends_df$hits_compstate %in% "<1"] <- "1"
#gtrends_df$hits_compstate <- gtrends_df$hits_compstate %>% as.numeric()

# Add Hits Adjusted ------------------------------------------------------------
# Applies equation to make hits comparable across time/states using a comparison
# state

# Using formulat on page 32 
# http://documents1.worldbank.org/curated/en/821821591104924698/pdf/Winners-and-Losers-from-COVID-19-Global-Evidence-from-Google-Search.pdf

# gtrends_df <- gtrends_df %>%
#   
#   mutate(hits = hits %>% as.numeric(),
#          hits_with_compstate = hits_with_compstate %>% as.numeric(),
#          hits_compstate = hits_compstate %>% as.numeric(),
#          date = date %>% as.Date()) %>%
#   
#   group_by(geo, keyword) %>%
#   mutate(hits_adj = hits_with_compstate / max(hits_compstate))

gtrends_df <- gtrends_df %>%
  mutate(hits = hits %>% as.numeric(),
         date = date %>% as.Date()) 

# Choose Based on Language -----------------------------------------------------
#### Merge in Language
languages <- read.csv(file.path(dropbox_file_path, 
                                "Data", "country_primary_language", "countries_lang.csv"),
                      stringsAsFactors = F) 

languages <- languages %>%
  dplyr::select(Code, Language_code_main) %>%
  dplyr::rename(geo = Code,
                state_language = Language_code_main) %>%
  filter(!(state_language == ""))

gtrends_df <- merge(gtrends_df, languages, by = "geo", all.x = T, all.y = F)

#### Only keep if google hits language matches state language
gtrends_df <- gtrends_df %>%
  filter(!is.na(state_language)) %>%
  filter(language == state_language)

# Merge in English Version of Keyword ------------------------------------------
keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                               "keywords", "FinalData","covid_keywords_alllanguages_clean.Rds"))

#### Dataset with english and translated word
keywords <- keywords %>%
  dplyr::select(names(keywords) %>% str_subset("keyword")) %>%
  dplyr::mutate(keyword = keyword_en) %>%
  pivot_longer(cols = -c(keyword)) %>%
  dplyr::rename(keyword_en = keyword) %>%
  dplyr::rename(keyword = value) %>%
  dplyr::select(keyword_en, keyword) %>%
  
  mutate(keyword = keyword %>% tolower(),
         keyword_en = keyword_en %>% tolower())

#### Merge
gtrends_df <- merge(gtrends_df, keywords, by = "keyword", all.x=T, all.y=F)

# Save Data --------------------------------------------------------------------
saveRDS(gtrends_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "global_with_refstate",
                              paste0("gl_gtrends_ref",comparison_iso,"_adj.Rds")))

