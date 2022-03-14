# Check Language Dataset

# Check to ensure country codes match between language dataset and google trends
# dataset

# Load Data --------------------------------------------------------------------
#### Country Languages
languages_df <- read_csv(file.path(dropbox_file_path, "Data", 
                                   "country_primary_language", "RawData",
                                   "countries.csv"))

languages_df <- languages_df[!is.na(languages_df$Language_code),]

#### Language Codes
gcodes_df <- read_csv(file.path(dropbox_file_path, "Data", 
                                "country_primary_language", "RawData",
                                "google_language_code.csv"))

#### Country codes that gTrends uses
data(countries)
countries <- countries %>%
  mutate_all(as.character)
countries <- countries[countries$sub_code == "",]

# Examine Differences ----------------------------------------------------------
#### Country codes in language data not in google data
languages_df$Code[!(languages_df$Code %in% countries$country_code)]

languages_df[languages_df$Code %in% "XK",]

tolower(countries$name) %>% str_subset("kos")

#### Check which countries don't have language from google
lapply(languages_df$Language_code, function(i){
  i %>% str_split(",") %>% unlist %>% length()
}) %>% 
  unlist() %>%
  max()

languages_df <- languages_df %>%
  separate(col = Language_code,
           sep = ",",
           into = paste0("lang_", 1:10))

for(i in 1:10){
  languages_df[[paste0("lang_in_gcode_", i)]] <-
    grepl(gcodes_rx, languages_df[[paste0("lang_", i)]])
}

languages_df$n_lang_google <- languages_df %>%
  dplyr::select(contains("lang_in_gcode")) %>%
  apply(1, sum)

df_0 <- languages_df[languages_df$n_lang_google %in% 0,]

