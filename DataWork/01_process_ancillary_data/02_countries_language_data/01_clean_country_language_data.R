# Clean language data

# Restrict countries:
# 1. Remove if country not in Google Translate
# 2. Only keep countries if have EITHER (1) COVID case data or (2) lockdown data

# Load Language Data -----------------------------------------------------------
languages_df <- read.csv(file.path(prim_lang_dir, "RawData",
                                   "countries_modified.csv"),
                         stringsAsFactors = F)

## Remove XK (not available via Google Trends)
languages_df <- languages_df %>%
  dplyr::filter(!is.na(Language_code_5)) %>%
  dplyr::rename(geo = Code)

# R reads "NA" as NA; make string
languages_df$geo[languages_df$Name %in% "Namibia"] <- "NA"

# Load Other Data --------------------------------------------------------------
### Countries with gTrends Data
library(gtrendsR)
data(countries)
gtrends_countries <- countries %>%
  mutate_all(as.character)
gtrends_countries <- gtrends_countries[is.na(countries$sub_code),]

### COVID Policy Data
ox_clean_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_measure.Rds"))

### COVID Case Data
cases_df <- readRDS(file.path(who_covid_dir, "FinalData", "covid.Rds"))
cases_df <- cases_df %>%
  distinct(geo, .keep_all = T)

# [Subset]: Countries with Google Trends Data ----------------------------------
languages_df$geo[!(languages_df$geo %in% gtrends_countries$country_code)]

languages_df <- languages_df[(languages_df$geo %in% gtrends_countries$country_code),]

# [Subset]: Either Case or Policy Data -----------------------------------------
# In language data, but not in case or oxford policy data
geo_case_policy <- c(cases_df$geo, ox_clean_df$geo) %>% unique()

# REMOVING THESE
languages_df$Name[!(languages_df$geo %in% geo_case_policy)]

languages_df <- languages_df[(languages_df$geo %in% geo_case_policy),]

# In google data, but not in languages data
# These tend to be former countries
gtrends_countries$name[!(gtrends_countries$country_code %in% languages_df$geo)] 
gtrends_countries$country_code[!(gtrends_countries$country_code %in% languages_df$geo)] 

# Export -----------------------------------------------------------------------
saveRDS(languages_df, file.path(prim_lang_dir, "FinalData",
                                "countries_modified_clean.Rds"))


