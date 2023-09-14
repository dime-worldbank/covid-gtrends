# DiD: Pooled Results

min_narm <- function(x){
  out <- min(x, na.rm = T)
  out[out %in% c(Inf, -Inf)] <- NA
  return(out)
}

max_narm <- function(x){
  out <- max(x, na.rm = T)
  out[out %in% c(Inf, -Inf)] <- NA
  return(out)
}

for(days_thresh in c(30, 60, 90, 120, 180)){
  print(paste(days_thresh, "================================================="))
  
  # Load Data --------------------------------------------------------------------
  df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                          "did_pooled_data.Rds"))
  
  ## Further restrict dates
  df <- df %>%
    dplyr::filter(abs(days_since_c_policy_yearcurrent) <= days_thresh)
  
  names(df) <- names(df) %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("/", "_")
  
  # Prep Data --------------------------------------------------------------------
  df <- df %>%
    dplyr::mutate(ln_cases_total = log(cases_total),
                  ln_gdp_pc = log(gdp_pc),
                  income_high = income == "High income",
                  income_low = income == "Low income",
                  income_low_middle = income == "Lower middle income",
                  income_upper_middle = income == "Upper middle income") %>%
    dplyr::filter(!is.na(ln_cases_total),
                  ln_cases_total > 0) %>%
    dplyr::group_by(geo) %>%
    dplyr::mutate(gm_retail_min = 
                    min_narm(gmobility_retail_and_recreation_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                            (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    dplyr::mutate(gm_grocery_min = 
                    min_narm(gmobility_grocery_and_pharmacy_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                           (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    dplyr::mutate(gm_parks_min = 
                    min_narm(gmobility_parks_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                            (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    dplyr::mutate(gm_transit_min = 
                    min_narm(gmobility_transit_stations_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                       (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    dplyr::mutate(gm_workplace_min = 
                    min_narm(gmobility_workplaces_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                 (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    dplyr::mutate(gm_residential_max = 
                    max_narm(gmobility_residential_percent_change_from_baseline[(pandemic_time == 1) & 
                                                                                  (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    
    dplyr::mutate(StringencyIndex_max = 
                    max_narm(StringencyIndex[(pandemic_time == 1) & 
                                               (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    
    dplyr::mutate(GovernmentResponseIndex_max = 
                    max_narm(GovernmentResponseIndex[(pandemic_time == 1) & 
                                                       (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    dplyr::mutate(EconomicSupportIndex_max = 
                    max_narm(EconomicSupportIndex[(pandemic_time == 1) & 
                                                    (days_since_c_policy_yearcurrent_post %in% T)])) %>%
    ungroup() %>%
    mutate(EconomicSupportIndex_max_cat = case_when(
      EconomicSupportIndex_max == 0 ~ 0,
      EconomicSupportIndex_max > 0 & EconomicSupportIndex_max <= 50 ~ 1,
      EconomicSupportIndex_max > 50 ~ 2
    )) %>%
    dplyr::mutate(gm_retail_min = gm_retail_min * -1,
                  gm_grocery_min = gm_grocery_min * -1,
                  gm_parks_min = gm_parks_min * -1,
                  gm_transit_min = gm_transit_min * -1,
                  gm_workplace_min = gm_workplace_min * -1) %>%
    dplyr::mutate(gm_avg_min = (gm_retail_min +
                                  gm_grocery_min +
                                  gm_parks_min +
                                  gm_transit_min +
                                  gm_workplace_min) / 5) %>%
    dplyr::mutate_at(vars(gm_retail_min,
                          gm_grocery_min,
                          gm_parks_min,
                          gm_transit_min,
                          gm_workplace_min,
                          gm_avg_min,
                          StringencyIndex_max, 
                          GovernmentResponseIndex_max, 
                          EconomicSupportIndex_max,
                          ln_gdp_pc), scale) %>%
    dplyr::mutate_at(vars(gm_retail_min,
                          gm_grocery_min,
                          gm_parks_min,
                          gm_transit_min,
                          gm_workplace_min,
                          gm_avg_min,
                          StringencyIndex_max, 
                          GovernmentResponseIndex_max, 
                          EconomicSupportIndex_max,
                          ln_gdp_pc), as.numeric) %>%
    dplyr::mutate(did_ln_cases_total = days_since_c_policy_yearcurrent_post_X_year2020 * ln_cases_total,
                  did_per_pop_using_internet = days_since_c_policy_yearcurrent_post_X_year2020 * per_pop_using_internet,
                  did_mobile_cell_sub_per100 = days_since_c_policy_yearcurrent_post_X_year2020 * mobile_cell_sub_per100,
                  did_gm_retail_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_retail_min,
                  did_gm_grocery_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_grocery_min,
                  did_gm_parks_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_parks_min,
                  did_gm_transit_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_transit_min,
                  did_gm_workplace_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_workplace_min,
                  did_gm_avg_min = days_since_c_policy_yearcurrent_post_X_year2020 * gm_avg_min,
                  did_gm_residential_max = days_since_c_policy_yearcurrent_post_X_year2020 * gm_residential_max,
                  
                  did_income_high = days_since_c_policy_yearcurrent_post_X_year2020 * income_high,
                  did_income_low = days_since_c_policy_yearcurrent_post_X_year2020 * income_low,
                  did_income_low_middle = days_since_c_policy_yearcurrent_post_X_year2020 * income_low_middle,
                  did_income_upper_middle = days_since_c_policy_yearcurrent_post_X_year2020 * income_upper_middle,
                  
                  did_ln_gdp_pc = days_since_c_policy_yearcurrent_post_X_year2020 * ln_gdp_pc,
                  
                  did_StringencyIndex_max = days_since_c_policy_yearcurrent_post_X_year2020 * StringencyIndex_max,
                  did_GovernmentResponseIndex_max = days_since_c_policy_yearcurrent_post_X_year2020 * GovernmentResponseIndex_max,
                  did_EconomicSupportIndex_max = days_since_c_policy_yearcurrent_post_X_year2020 * EconomicSupportIndex_max,
                  did_EconomicSupportIndex_max_cat = days_since_c_policy_yearcurrent_post_X_year2020 * EconomicSupportIndex_max_cat) %>%
    
    dplyr::mutate(did_gm_avg_min_X_did_EconomicSupportIndex_max = did_gm_avg_min * did_EconomicSupportIndex_max,
                  did_StringencyIndex_max_X_did_EconomicSupportIndex_max = did_StringencyIndex_max * did_EconomicSupportIndex_max,
                  
                  did_gm_avg_min_X_did_EconomicSupportIndex_max_cat = did_gm_avg_min * did_EconomicSupportIndex_max_cat,
                  did_StringencyIndex_max_X_did_EconomicSupportIndex_max_cat = did_StringencyIndex_max * did_EconomicSupportIndex_max_cat)
  
  # Analysis ---------------------------------------------------------------------
  run_regs <- function(keyword_i, df){
    print(keyword_i)
    
    out1 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "Overall")
    
    out2 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   
                   gm_avg_min*days_since_c_policy_yearcurrent_post +
                   gm_avg_min*pandemic_time +
                   did_gm_avg_min +
                   
                   EconomicSupportIndex_max*days_since_c_policy_yearcurrent_post +
                   EconomicSupportIndex_max*pandemic_time +
                   did_EconomicSupportIndex_max | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_gm_avg_min_AND_did_EconomicSupportIndex_max")
    
    out3 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   gm_avg_min*days_since_c_policy_yearcurrent_post +
                   gm_avg_min*pandemic_time +
                   did_gm_avg_min +
                   
                   EconomicSupportIndex_max*days_since_c_policy_yearcurrent_post +
                   EconomicSupportIndex_max*pandemic_time +
                   did_EconomicSupportIndex_max +
                   
                   ln_gdp_pc*days_since_c_policy_yearcurrent_post +
                   ln_gdp_pc*pandemic_time +
                   did_ln_gdp_pc | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_gm_avg_min_AND_did_EconomicSupportIndex_max_gdppc")
    
    out4 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   
                   StringencyIndex_max*days_since_c_policy_yearcurrent_post +
                   StringencyIndex_max*pandemic_time +
                   did_StringencyIndex_max +
                   
                   EconomicSupportIndex_max*days_since_c_policy_yearcurrent_post +
                   EconomicSupportIndex_max*pandemic_time +
                   did_EconomicSupportIndex_max | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max")
    
    out5 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   
                   StringencyIndex_max*days_since_c_policy_yearcurrent_post +
                   StringencyIndex_max*pandemic_time +
                   did_StringencyIndex_max +
                   
                   EconomicSupportIndex_max*days_since_c_policy_yearcurrent_post +
                   EconomicSupportIndex_max*pandemic_time +
                   did_EconomicSupportIndex_max +
                   
                   ln_gdp_pc*days_since_c_policy_yearcurrent_post +
                   ln_gdp_pc*pandemic_time +
                   did_ln_gdp_pc | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_gdppc")
    
    
    out6 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   
                   gm_avg_min*days_since_c_policy_yearcurrent_post +
                   gm_avg_min*pandemic_time +
                   did_gm_avg_min | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_gm_avg_min")
    
    out7 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   
                   StringencyIndex_max*days_since_c_policy_yearcurrent_post +
                   StringencyIndex_max*pandemic_time +
                   did_StringencyIndex_max | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_StringencyIndex_max")
    
    out8 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   
                   EconomicSupportIndex_max*days_since_c_policy_yearcurrent_post +
                   EconomicSupportIndex_max*pandemic_time +
                   did_EconomicSupportIndex_max | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_EconomicSupportIndex_max")
    
    out9 <- felm(hits_log ~ pandemic_time + 
                   days_since_c_policy_yearcurrent_post + 
                   days_since_c_policy_yearcurrent_post_X_year2020 +
                   
                   ln_gdp_pc*days_since_c_policy_yearcurrent_post +
                   ln_gdp_pc*pandemic_time +
                   did_ln_gdp_pc | geo + week | 0 | 0, 
                 data = df[df$keyword_en %in% keyword_i,]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "did_ln_gdp_pc")
    
    
    #### By country groups -- wb_region
    out10 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$wb_region %in% "North America"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "wb_region",
             wb_region = "North America")
    
    out11 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$wb_region %in% "East Asia & Pacific"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "wb_region",
             wb_region = "East Asia & Pacific")
    
    out12 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$wb_region %in% "Latin America & Caribbean"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "wb_region",
             wb_region = "Latin America & Caribbean")
    
    out13 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$wb_region %in% "Sub-Saharan Africa"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "wb_region",
             wb_region = "Sub-Saharan Africa")
    
    out14 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$wb_region %in% "South Asia"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "wb_region",
             wb_region = "South Asia")
    
    out15 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$wb_region %in% "Europe & Central Asia"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "wb_region",
             wb_region = "Europe & Central Asia")
    
    out16 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$wb_region %in% "Middle East & North Africa"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "wb_region",
             wb_region = "Middle East & North Africa")
    
    #### By country groups -- income
    check_nrow <- df[(df$keyword_en %in% keyword_i) & (df$income %in% "Low income"),]
    
    if(nrow(check_nrow) > 0){
      out17 <- felm(hits_log ~ pandemic_time + 
                      days_since_c_policy_yearcurrent_post + 
                      days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                    data = df[(df$keyword_en %in% keyword_i) & (df$income %in% "Low income"),]) %>%
        lm_post_confint_tidy() %>%
        mutate(type = "income",
               income = "Low income")
    } else{
      out17 <- data.frame(NULL)
    }
    
    out18 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$income %in% "Lower middle income"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "income",
             income = "Lower middle income")
    
    out19 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$income %in% "Upper middle income"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "income",
             income = "Upper middle income")
    
    out20 <- felm(hits_log ~ pandemic_time + 
                    days_since_c_policy_yearcurrent_post + 
                    days_since_c_policy_yearcurrent_post_X_year2020  | geo + week | 0 | 0, 
                  data = df[(df$keyword_en %in% keyword_i) & (df$income %in% "High income"),]) %>%
      lm_post_confint_tidy() %>%
      mutate(type = "income",
             income = "High income")
    
    out_all <- bind_rows(
      out1,
      out2,
      out3,
      out4,
      out5,
      out6,
      out7,
      out8,
      out9,
      out10,
      out11,
      out12,
      out13,
      out14,
      out15,
      out16,
      out17,
      out18,
      out19,
      out20
    )
    
    out_all$keyword <- keyword_i
    
    return(out_all)
  }
  
  coef_df <- map_df(unique(df$keyword_en), run_regs, df)
  
  # Export -----------------------------------------------------------------------
  saveRDS(coef_df,
          file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                    paste0("did_pooled_results_",days_thresh,".Rds")))
  
  saveRDS(df,
          file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                    paste0("did_pooled_data_",days_thresh,".Rds")))
  
}





