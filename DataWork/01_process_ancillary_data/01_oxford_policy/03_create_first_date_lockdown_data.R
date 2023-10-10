# Clean Oxford Policy Response Data

# Create a dataset that indicates the first date of lockdown/closures for each
# country

# Load data --------------------------------------------------------------------
ox_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_latest.Rds"))

# Helper Functions -------------------------------------------------------------
is_greater_zero <- function(x) as.numeric(x > 0)

min_ignore_0_and_na <- function(x){
  x_clean <- x[!is.na(x)]
  out <- min(x_clean[x_clean > 0])
  return(out)
}

# Clean data -------------------------------------------------------------------
ox_clean_df <- ox_df %>%
  dplyr::select(geo, date, 
                "C1M_School closing",
                "C2M_Workplace closing",
                "C3M_Cancel public events",
                "C4M_Restrictions on gatherings",
                "C5M_Close public transport",
                "C6M_Stay at home requirements",
                "C7M_Restrictions on internal movement",
                "C8EV_International travel controls") %>%
  dplyr::mutate(C_policy = 
                  `C1M_School closing` +
                  `C2M_Workplace closing` +
                  `C3M_Cancel public events` +
                  `C4M_Restrictions on gatherings` +
                  `C5M_Close public transport` +
                  `C6M_Stay at home requirements` +
                  `C7M_Restrictions on internal movement` +
                  `C8EV_International travel controls`) %>%
  dplyr::mutate(date = date %>% 
                  as.character() %>% 
                  str_replace_all("-", "") %>%
                  as.numeric()) %>%
  dplyr::mutate_at(vars(c("C1M_School closing",
                          "C2M_Workplace closing",
                          "C3M_Cancel public events",
                          "C4M_Restrictions on gatherings",
                          "C5M_Close public transport",
                          "C6M_Stay at home requirements",
                          "C7M_Restrictions on internal movement",
                          "C8EV_International travel controls",
                          "C_policy")), 
                   ~is_greater_zero(.) * date) %>%
  dplyr::select(-date) %>%
  dplyr::group_by(geo) %>%
  dplyr::summarise_all(min_ignore_0_and_na) %>%
  dplyr::mutate_if(is.numeric, . %>% as.character() %>% ymd()) 

names(ox_clean_df) <- names(ox_clean_df) %>%
  tolower() %>%
  str_replace_all(" ", "_")

ox_clean_df <- ox_clean_df %>% 
  rename_at(vars(-geo), ~ paste0(., '_first_date'))

names(ox_clean_df) <- names(ox_clean_df) %>%
  str_replace_all("c1m", "c1") %>%
  str_replace_all("c2m", "c2") %>%
  str_replace_all("c3m", "c3") %>%
  str_replace_all("c4m", "c4") %>%
  str_replace_all("c5m", "c5") %>%
  str_replace_all("c6m", "c6") %>%
  str_replace_all("c7m", "c7") %>%
  str_replace_all("c8ev", "c8")

# Export data ------------------------------------------------------------------
saveRDS(ox_clean_df, 
        file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_measure.Rds"))




