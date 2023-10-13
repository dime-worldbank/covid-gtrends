# Merge Google Trends Data with Other Data Sources

for(keyword_type in c("symptoms", "contain")){
  print(paste(keyword_type, "================================================"))
  
  # Load Data --------------------------------------------------------------------
  gtrends_df <- readRDS(file.path(gtrends_dir, "FinalData",
                                  "gtrends_full_timeseries", 
                                  paste0("gtrends_complete_",keyword_type,".Rds")))
  
  ## WHO Cases
  cases_df <- readRDS(file.path(who_covid_dir, 
                                "FinalData", "covid.Rds"))
  
  ## Oxford Policy
  ox_earliest_measure_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_measure.Rds"))
  
  ox_nat_timeseries_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_national_timeseries.Rds"))
  
  ## WDI
  wdi_df <- readRDS(file.path(wdi_dir, "FinalData", "wdi_data.Rds"))
  
  ## Google Mobility
  gmobility_df <- readRDS(file.path(data_dir, "google_mobility", "FinalData",
                                    "Global_Mobility_Report.Rds"))
  
  # Merge Cases ------------------------------------------------------------------
  cases_df <- cases_df %>%
    dplyr::select(-c(country))
  
  gtrends_df <- gtrends_df %>%
    left_join(cases_df, by = c("geo", "date"))
  
  # Merge WDI --------------------------------------------------------------------
  gtrends_df <- gtrends_df %>%
    left_join(wdi_df, by = "geo")
  
  # Merge Oxford Policy Response Data --------------------------------------------
  gtrends_df <- gtrends_df %>%
    left_join(ox_nat_timeseries_df, by = c("geo", "date"))
  
  # Days Since Oxford Policies ---------------------------------------------------
  gtrends_df <- gtrends_df %>%
    left_join(ox_earliest_measure_df, by = "geo")
  
  gtrends_df <- gtrends_df %>%
    mutate(days_since_c1_school_closing = date - c1_school_closing_first_date,
           days_since_c2_workplace_closing = date - c2_workplace_closing_first_date,
           days_since_c3_cancel_public_events = date - c3_cancel_public_events_first_date,
           days_since_c4_restrictions_on_gatherings = date - c4_restrictions_on_gatherings_first_date,
           days_since_c5_close_public_transport = date - c5_close_public_transport_first_date,
           days_since_c6_stay_at_home_requirements = date - c6_stay_at_home_requirements_first_date,
           days_since_c7_restrictions_on_internal_movement = date - c7_restrictions_on_internal_movement_first_date,
           days_since_c8_international_travel_controls = date - c8_international_travel_controls_first_date,
           days_since_c_policy = date - c_policy_first_date) %>%
    dplyr::select(-c(c1_school_closing_first_date,
                     c2_workplace_closing_first_date,
                     c3_cancel_public_events_first_date,
                     c4_restrictions_on_gatherings_first_date,
                     c5_close_public_transport_first_date,
                     c6_stay_at_home_requirements_first_date,
                     c7_restrictions_on_internal_movement_first_date,
                     c7_restrictions_on_internal_movement_first_date)) %>%
    
    # Don't analyzie these (only c_policy), so can remove for now
    dplyr::select(-c(days_since_c1_school_closing,
                     days_since_c2_workplace_closing,
                     days_since_c3_cancel_public_events,
                     days_since_c4_restrictions_on_gatherings,
                     days_since_c5_close_public_transport,
                     days_since_c6_stay_at_home_requirements,
                     days_since_c7_restrictions_on_internal_movement,
                     days_since_c8_international_travel_controls))
  
  # Google Mobility --------------------------------------------------------------
  gmobility_df <- gmobility_df %>%
    # dplyr::filter(sub_region_1 %in% "",
    #               sub_region_2 %in% "",
    #               metro_area %in% "",
    #               !is.na(country_region_code)) %>%
    dplyr::filter(is.na(sub_region_1),
                  is.na(sub_region_2),
                  is.na(metro_area),
                  !is.na(country_region_code)) %>%
    dplyr::select(country_region_code, date,
                  retail_and_recreation_percent_change_from_baseline,
                  grocery_and_pharmacy_percent_change_from_baseline,
                  parks_percent_change_from_baseline,
                  transit_stations_percent_change_from_baseline,
                  workplaces_percent_change_from_baseline,
                  residential_percent_change_from_baseline) %>% 
    dplyr::rename(geo = country_region_code) %>%
    rename_at(vars(-geo, -date), ~ paste0("gmobility_", .)) %>%
    dplyr::mutate(date = date %>% as.Date)
  
  gtrends_df <- gtrends_df %>%
    left_join(gmobility_df,
              by = c("geo", "date"))
  
  # Add continent/regions --------------------------------------------------------
  gtrends_df$un_regionsub_name <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "un.regionsub.name")
  gtrends_df$wb_region <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "region")
  gtrends_df$country <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "country.name")
  gtrends_df$continent <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "continent")
  
  # Export -----------------------------------------------------------------------
  saveRDS(gtrends_df,
          file.path(data_dir, "google_trends", "FinalData",
                    "gtrends_full_timeseries", 
                    paste0("gtrends_otherdata_complete_",keyword_type,".Rds")))
}




