# Clean Oxford Policy Response Data

# Create a dataset that indicates the first date of lockdown/closures for each
# country

# Load data --------------------------------------------------------------------
ox_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_latest.Rds"))

# Clean data -------------------------------------------------------------------
ox_clean_df <- ox_df %>%
  dplyr::filter(Jurisdiction %in% "NAT_TOTAL") %>%
  dplyr::select(date, geo, 
                #"E1_Income support",
                #"E2_Debt/contract relief",
                #"E3_Fiscal measures",
                #"E4_International support",
                #"C1_School closing",
                #"C2_Workplace closing",
                #"C3_Cancel public events",
                #"C4_Restrictions on gatherings",
                #"C5_Close public transport",
                #"C6_Stay at home requirements",
                #"C7_Restrictions on internal movement",
                #"C8_International travel controls",
                # ContainmentHealthIndex
                `H2_Testing policy`,
                StringencyIndex, 
                GovernmentResponseIndex, 
                EconomicSupportIndex) %>%
  dplyr::rename(h2_testing_policy = `H2_Testing policy`)

# Export data ------------------------------------------------------------------
saveRDS(ox_clean_df, 
        file.path(oxpol_dir, "FinalData", "OxCGRT_national_timeseries.Rds"))







