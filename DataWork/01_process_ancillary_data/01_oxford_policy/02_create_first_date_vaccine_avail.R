# First date for vaccine things

# Export data ------------------------------------------------------------------
vac_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_vaccine_latest.Rds"))

names(vac_df) <- names(vac_df) %>%
  tolower() %>%
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "") %>%
  str_replace_all(" ", "_")

vac_df <- vac_df %>%
  dplyr::select(geo, date, 
                v1_vaccine_prioritisation_summary,
                v2_vaccine_availability_summary) %>%
  dplyr::rename(v1_vaccine = v1_vaccine_prioritisation_summary,
                v2_vaccine = v2_vaccine_availability_summary)

vac_df$v1_vaccine_1 <- vac_df$v1_vaccine >= 1
vac_df$v1_vaccine_2 <- vac_df$v1_vaccine >= 2

vac_df$v2_vaccine_1 <- vac_df$v2_vaccine >= 1
vac_df$v2_vaccine_2 <- vac_df$v2_vaccine >= 2
vac_df$v2_vaccine_3 <- vac_df$v2_vaccine >= 3

# CODE FROM OTHER FILE !!!!!!!!

# Helper Functions -------------------------------------------------------------
is_greater_zero <- function(x) as.numeric(x > 0)

min_ignore_0_and_na <- function(x){
  x_clean <- x[!is.na(x)]
  out <- min(x_clean[x_clean > 0])
  return(out)
}

# Clean data -------------------------------------------------------------------
vac_clean_df <- vac_df %>%
  dplyr::select(-c(v1_vaccine,
                   v2_vaccine)) %>%
  dplyr::mutate(date = date %>% 
                  as.character() %>% 
                  str_replace_all("-", "") %>%
                  as.numeric()) %>%
  dplyr::mutate_at(vars(c(v1_vaccine_1,
                          v1_vaccine_2,
                          v2_vaccine_1,
                          v2_vaccine_2,
                          v2_vaccine_3)), 
                   ~is_greater_zero(.) * date) %>%
  dplyr::select(-date) %>%
  dplyr::group_by(geo) %>%
  dplyr::summarise_all(min_ignore_0_and_na) %>%
  dplyr::mutate_if(is.numeric, . %>% as.character() %>% ymd()) 

vac_clean_df <- vac_clean_df %>% 
  rename_at(vars(-geo), ~ paste0(., '_first_date'))

# Export data ------------------------------------------------------------------
saveRDS(vac_clean_df, 
        file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_vaccine_dates.Rds"))


