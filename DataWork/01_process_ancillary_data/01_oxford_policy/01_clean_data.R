# Download latest data from Oxford Policy Response Tracker

# MAIN DATA ====================================================================

# Read data from OxCGRT Githib -------------------------------------------------
#ox_df <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
ox_df <- read_csv(file.path(oxpol_dir, "RawData", "OxCGRT_nat_latest.csv"))

# Cleanup ----------------------------------------------------------------------
ox_clean_df <- ox_df %>%
  dplyr::mutate(Date = Date %>% as.character() %>% ymd(),
                geo = countrycode(CountryCode, origin = "iso3c", destination = "iso2c")) %>%
  dplyr::mutate(geo = case_when(
    CountryCode == "RKS" ~ "XK",
    TRUE ~ geo
  )) %>%
  dplyr::rename(date = Date,
                country = CountryName) %>%
  dplyr::select(-CountryCode)

# Export data ------------------------------------------------------------------
saveRDS(ox_clean_df, file.path(oxpol_dir, "FinalData", "OxCGRT_latest.Rds"))

# VACCINE DATA =================================================================
# vac_df <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_vaccines_full.csv")
#   
# # Cleanup ----------------------------------------------------------------------
# vac_clean_df <- vac_df %>%
#   dplyr::mutate(Date = Date %>% as.character() %>% ymd(),
#                 geo = countrycode(CountryCode, origin = "iso3c", destination = "iso2c")) %>%
#   dplyr::mutate(geo = case_when(
#     CountryCode == "RKS" ~ "XK",
#     TRUE ~ geo
#   )) %>%
#   dplyr::rename(date = Date,
#                 country = CountryName) %>%
#   dplyr::select(-CountryCode)
# 
# # Export data ------------------------------------------------------------------
# saveRDS(vac_clean_df, file.path(oxpol_dir, "FinalData", "OxCGRT_vaccine_latest.Rds"))
# 
# 
# 



# df1 <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_latest.Rds"))
# df2 <- readRDS(file.path("~/Dropbox/World Bank/Replication Packages/COVID Google Trends copy/Data/oxford_covid_policy_tracker",
#                                       "FinalData", "OxCGRT_latest.Rds"))
# 
# df1 <- df1[df1$country == "United States",]
# df2 <- df2[df2$country == "United States",]
# 
# df1$StringencyIndex_Average
# df1$StringencyIndex_Average[1:100]
# df2$StringencyIndex[1:100]
# 
# 
# df2$date %>% min()
# 
# # 
# # df1$`C1M_School closing`[1:1000] == df2$`C1_School closing`[1:1000]
# # table(df1$`C1M_School closing`[1:1000])
# 
# # names(df1)[14]
# # names(df2)[14]
# # names(df1)[14] == names(df2)
# 
