# Merge Google Trends Data with Other Data Sources

# gtrends_df %>%
#   dplyr::filter(geo == "GB",
#                 keyword_en == "loss of smell") %>%
#   ggplot() +
#   geom_line(aes(x = date, y = hits))

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
  
  ## Oxford Policy - Vaccine
  #vac_earliest_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_earliest_vaccine_dates.Rds"))
  
  #vac_timeseries_df <- readRDS(file.path(oxpol_dir, "FinalData", "OxCGRT_vaccine_national_timeseries.Rds"))
  
  ## WDI
  wdi_df <- readRDS(file.path(wdi_dir, "FinalData", "wdi_data.Rds"))
  
  ## Google Mobility
  gmobility_df <- read.csv(file.path(dropbox_file_path, "Data", "google_mobility", "RawData",
                                     "Global_Mobility_Report.csv"),
                           stringsAsFactors = F)
  
  # Merge Cases ------------------------------------------------------------------
  cases_df <- cases_df %>%
    dplyr::select(-c(country))
  
  gtrends_df <- gtrends_df %>%
    left_join(cases_df, by = c("geo", "date"))
  
  # No covid data for Jan 1 and 2
  #gtrends_df$cases_new[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0
  #gtrends_df$death_new[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0
  #gtrends_df$cases[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0
  #gtrends_df$death[gtrends_df$date %in% as.Date(c("2020-01-01", "2020-01-02"))] <- 0

  # Global Vaccine Dosage Data ---------------------------------------------------
  # gvac_df <- read.csv(file.path(dropbox_file_path, "Data", "global_vaccine", "RawData", 
  #                               "covid-vaccination-doses-per-capita.csv"),
  #                     stringsAsFactors = F)
  # 
  # ## Add vaccination rate
  # gvac_df <- gvac_df %>%
  #   dplyr::filter(!is.na(Code),
  #                 Code != "") %>%
  #   dplyr::mutate(geo = countrycode(Code, origin = "iso3c", destination = "iso2c")) %>%
  #   dplyr::mutate(geo = case_when(
  #     Entity %in% "Kosovo" ~ "XK",
  #     TRUE ~ geo
  #   )) %>%
  #   dplyr::filter(!is.na(geo)) %>%
  #   dplyr::rename(date = Day) %>%
  #   dplyr::select(date, geo, total_vaccinations_per_hundred) %>%
  #   dplyr::mutate(date = ymd(date))
  # 
  # ## First date of vaccinations
  # gvac_first_df <- gvac_df %>%
  #   dplyr::filter(total_vaccinations_per_hundred > 0) %>%
  #   arrange(date) %>%
  #   distinct(geo, .keep_all = T) %>%
  #   dplyr::select(date, geo) %>%
  #   dplyr::rename(date_first_vaccine_given = date) 
  # 
  # ## Merge
  # gtrends_df <- gtrends_df %>%
  #   left_join(gvac_df, by = c("geo", "date"))
  # 
  # gtrends_df <- gtrends_df %>%
  #   left_join(gvac_first_df, by = "geo")
  # 
  # ## Days since first vaccine given
  # gtrends_df <- gtrends_df %>%
  #   mutate(days_since_first_vaccine_given = date - date_first_vaccine_given)
  # 
  # Merge WDI --------------------------------------------------------------------
  gtrends_df <- gtrends_df %>%
    left_join(wdi_df, by = "geo")
  
  # Merge Oxford Policy Response Data --------------------------------------------
  gtrends_df <- gtrends_df %>%
    left_join(ox_nat_timeseries_df, by = c("geo", "date"))
  
  # Merge Oxford Vaccine Timeseries ----------------------------------------------
  # gtrends_df <- gtrends_df %>%
  #   left_join(vac_timeseries_df, by = c("geo", "date"))
  # 
  # Days Since Vaccines ----------------------------------------------------------
  # gtrends_df <- gtrends_df %>%
  #   left_join(vac_earliest_df, by = "geo")
  
  # gtrends_df <- gtrends_df %>%
  #   mutate(days_since_v1_vaccine_1 = date - v1_vaccine_1_first_date,
  #          days_since_v1_vaccine_2 = date - v1_vaccine_2_first_date,
  #          days_since_v2_vaccine_1 = date - v2_vaccine_1_first_date,
  #          days_since_v2_vaccine_2 = date - v2_vaccine_2_first_date,
  #          days_since_v2_vaccine_3 = date - v2_vaccine_3_first_date,) %>%
  #   dplyr::select(-c(v1_vaccine_1_first_date,
  #                    v1_vaccine_2_first_date,
  #                    v2_vaccine_1_first_date,
  #                    v2_vaccine_2_first_date,
  #                    v2_vaccine_3_first_date))
  # 
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
    dplyr::filter(sub_region_1 %in% "",
                  sub_region_2 %in% "",
                  metro_area %in% "",
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
          file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries", 
                    paste0("gtrends_otherdata_complete_",keyword_type,".Rds")))
}




