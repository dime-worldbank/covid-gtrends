# Calculate correlations

# Adds correlations to dataset, indicating best correlation and lag/lead of
# that correlation.

# 1. Add moving averages
# 2. Add leads/lags
# 3. Compute max correlations
# 4. Merge correlations with main data
# 5. Cleanup data
# 6. Export

# https://gist.github.com/drsimonj/2038ff9f9c67063f384f10fac95de566

begin_day <- c("2020-01-01",
               "2020-07-01",
               "2021-01-01",
               "2021-07-01",
               "2022-01-01",
               "2022-07-01")

end_day <- c("2020-06-30",
             "2020-12-31",
             "2021-06-30",
             "2021-12-31",
             "2022-06-30",
             "2022-12-31")

OVERWRITE_DATA <- F

keyword_type = "contain"
begin_day_i = "2020-01-01"
end_day_i = "2022-12-31"

for(begin_day_i in begin_day){
  for(end_day_i in rev(end_day)){
    for(keyword_type in c("contain", "symptoms")){
      
      if(end_day_i < begin_day_i) next
      
      #### Check if processed
      OUT_PATH_1 <- file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              paste0("gtrends_since",begin_day_i,"_until",end_day_i,"_",keyword_type,".Rds"))
      
      if(!file.exists(OUT_PATH_1) | OVERWRITE_DATA){
        
        if(end_day_i < begin_day_i) next
        
        print(paste(keyword_type, begin_day_i, end_day_i, "===================="))
        
        # Load Data --------------------------------------------------------------------
        gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                        "gtrends_full_timeseries", 
                                        paste0("gtrends_otherdata_varclean_complete_",keyword_type,".Rds")))
        
        gtrends_df <- gtrends_df %>%
          dplyr::filter(date >= as.Date(begin_day_i),
                        date <= as.Date(end_day_i)) %>%
          
          ## Remove if no variation in hits; these will return NA correlation,
          # so ignore now.
          group_by(geo, keyword_en) %>%
          dplyr::mutate(hits_min = min(hits, na.rm = T),
                        hits_max = max(hits, na.rm = T)) %>%
          ungroup() %>%
          dplyr::filter(  !((hits_min == 0) & (hits_max == 0))  ) %>%
          
          ## Restrict variables and sort by date
          dplyr::select(geo, date, keyword_en,
                        cases_new_ma7, death_new_ma7,
                        hits_ma7, hits,
                        cases_new, death_new) %>%
          dplyr::arrange(date)
        
        gc(); gc(); gc();
        
        # Correlations across different leads/lags -----------------------------------
        compute_cor <- function(leadlag, gtrends_df){
          
          print(leadlag)
          
          ## Prep lead/lag hits variable
          leadlag_abs <- abs(leadlag)
          
          if(leadlag < 0){
            
            gtrends_df <- gtrends_df %>%
              dplyr::arrange(date) %>%
              dplyr::group_by(geo, keyword_en) %>%
              dplyr::mutate(hits_leadlag = dplyr::lag(hits, leadlag_abs),
                            hits_ma7_leadlag = dplyr::lag(hits_ma7, leadlag_abs)) %>%
              dplyr::ungroup()
            
          } else if(leadlag > 0){
            
            gtrends_df <- gtrends_df %>%
              dplyr::arrange(date) %>%
              dplyr::group_by(geo, keyword_en) %>%
              dplyr::mutate(hits_leadlag = dplyr::lead(hits, leadlag_abs),
                            hits_ma7_leadlag = dplyr::lead(hits_ma7, leadlag_abs)) %>%
              dplyr::ungroup()
            
          } else if(leadlag == 0){
            
            gtrends_df <- gtrends_df %>%
              dplyr::arrange(date) %>%
              dplyr::group_by(geo, keyword_en) %>%
              dplyr::mutate(hits_leadlag = hits,
                            hits_ma7_leadlag = hits_ma7) %>%
              dplyr::ungroup()
            
          }
          
          gtrends_cor_df_i <- gtrends_df %>%
            dplyr::arrange(date) %>%
            dplyr::filter(!is.na(hits_leadlag),
                          !is.na(hits_ma7_leadlag),
                          !is.na(cases_new_ma7),
                          !is.na(cases_new),
                          !is.na(death_new)) %>%
            group_by(geo, keyword_en) %>%
            dplyr::summarise(cor_cases_hits = cor(cases_new, hits_leadlag),
                             cor_death_hits = cor(death_new, hits_leadlag),
                             cor_casesMA7_hitsMA7 = cor(cases_new_ma7, hits_ma7_leadlag),
                             cor_deathMA7_hitsMA7 = cor(death_new_ma7, hits_ma7_leadlag)) %>%
            ungroup() %>%
            dplyr::mutate(leadlag = leadlag)
          
          gtrends_df[gtrends_df$geo == "AD" & gtrends_df$keyword_en == "anxiety",]
          
          return(gtrends_cor_df_i)
          
        }
        
        gtrends_cor_long_df <- map_df(-21:21, compute_cor, gtrends_df)
        
        # gtrends_cor_long_df <- map_df(-21:21, function(leadlag){
        #   
        #   print(leadlag)
        #   
        #   ## Prep lead/lag hits variable
        #   leadlag_abs <- abs(leadlag)
        #   
        #   if(leadlag < 0){
        #     
        #     gtrends_df <- gtrends_df %>%
        #       dplyr::arrange(date) %>%
        #       dplyr::group_by(geo, keyword_en) %>%
        #       dplyr::mutate(hits_leadlag = dplyr::lag(hits, leadlag_abs),
        #                     hits_ma7_leadlag = dplyr::lag(hits_ma7, leadlag_abs)) %>%
        #       dplyr::ungroup()
        #     
        #   } else if(leadlag > 0){
        #     
        #     gtrends_df <- gtrends_df %>%
        #       dplyr::arrange(date) %>%
        #       dplyr::group_by(geo, keyword_en) %>%
        #       dplyr::mutate(hits_leadlag = dplyr::lead(hits, leadlag_abs),
        #                     hits_ma7_leadlag = dplyr::lead(hits_ma7, leadlag_abs)) %>%
        #       dplyr::ungroup()
        #     
        #   } else if(leadlag == 0){
        #     
        #     gtrends_df <- gtrends_df %>%
        #       dplyr::arrange(date) %>%
        #       dplyr::group_by(geo, keyword_en) %>%
        #       dplyr::mutate(hits_leadlag = hits,
        #                     hits_ma7_leadlag = hits_ma7) %>%
        #       dplyr::ungroup()
        #     
        #   }
        #   
        #   gtrends_cor_df_i <- gtrends_df %>%
        #     dplyr::arrange(date) %>%
        #     dplyr::filter(!is.na(hits_leadlag),
        #                   !is.na(hits_ma7_leadlag),
        #                   !is.na(cases_new_ma7),
        #                   !is.na(cases_new),
        #                   !is.na(death_new)) %>%
        #     group_by(geo, keyword_en) %>%
        #     dplyr::summarise(cor_cases_hits = cor(cases_new, hits_leadlag),
        #                      cor_death_hits = cor(death_new, hits_leadlag),
        #                      cor_casesMA7_hitsMA7 = cor(cases_new_ma7, hits_ma7_leadlag),
        #                      cor_deathMA7_hitsMA7 = cor(death_new_ma7, hits_ma7_leadlag)) %>%
        #     ungroup() %>%
        #     dplyr::mutate(leadlag = leadlag)
        #   
        #   return(gtrends_cor_df_i)
        # })
        
        # Summarize to geo/keyword level ---------------------------------------------
        
        #### Summarize to geo/keyword level
        gtrends_cor_df <- gtrends_cor_long_df %>%
          ungroup() %>%
          dplyr::group_by(geo, keyword_en) %>%
          dplyr::summarise(cor_cases_hits_nolag = cor_cases_hits[leadlag == 0][1],
                           cor_death_hits_nolag = cor_death_hits[leadlag == 0][1],
                           
                           cor_casesMA7_hitsMA7_nolag = cor_casesMA7_hitsMA7[leadlag == 0][1],
                           cor_deathMA7_hitsMA7_nolag = cor_deathMA7_hitsMA7[leadlag == 0][1],
                           
                           cor_casesMA7_hitsMA7_max = max(cor_casesMA7_hitsMA7, na.rm=T),
                           cor_deathMA7_hitsMA7_max = max(cor_deathMA7_hitsMA7, na.rm=T),
                           
                           cor_cases_hits_max = max(cor_cases_hits, na.rm=T),
                           cor_death_hits_max = max(cor_death_hits, na.rm=T),
                           
                           cor_cases_hits_mean = mean(cor_cases_hits, na.rm=T),
                           cor_death_hits_mean = mean(cor_death_hits, na.rm=T),
                           
                           cor_cases_hits_sd = sd(cor_cases_hits, na.rm=T),
                           cor_death_hits_sd = sd(cor_death_hits, na.rm=T),
                           
                           cor_casesMA7_hitsMA7_lag = leadlag[cor_casesMA7_hitsMA7 %in% max(cor_casesMA7_hitsMA7)][1],
                           cor_deathMA7_hitsMA7_lag = leadlag[cor_deathMA7_hitsMA7 %in% max(cor_deathMA7_hitsMA7)][1],
                           
                           cor_cases_hits_lag = leadlag[cor_cases_hits %in% max(cor_cases_hits)][1],
                           cor_death_hits_lag = leadlag[cor_death_hits %in% max(cor_death_hits)][1]) %>%
          ungroup() %>%
          dplyr::mutate(cor_cases_hits_zscore = (cor_cases_hits_max - cor_cases_hits_mean) / cor_cases_hits_sd,
                        cor_death_hits_zscore = (cor_death_hits_max - cor_death_hits_mean) / cor_death_hits_sd,
                        
                        cor_cases_hits_nolag_zscore = (cor_cases_hits_nolag - cor_cases_hits_mean) / cor_cases_hits_sd,
                        cor_death_hits_nolag_zscore = (cor_death_hits_nolag - cor_death_hits_mean) / cor_death_hits_sd) 
        
        gtrends_cor_df <- gtrends_cor_df %>%
          dplyr::filter(!is.na(cor_cases_hits_nolag))
        
        #### Stack Cases/Deaths together
        cor_max_df <- bind_rows(gtrends_cor_df %>%
                                  dplyr::rename(cor_nolag = cor_cases_hits_nolag,
                                                cor = cor_cases_hits_max,
                                                lag = cor_cases_hits_lag,
                                                zscore = cor_cases_hits_zscore,
                                                zscore_nolag = cor_cases_hits_nolag_zscore) %>%
                                  mutate(type = "Cases"),
                                
                                gtrends_cor_df %>%
                                  dplyr::rename(cor_nolag = cor_death_hits_nolag,
                                                cor = cor_death_hits_max,
                                                lag = cor_death_hits_lag,
                                                zscore = cor_death_hits_zscore,
                                                zscore_nolag = cor_death_hits_nolag_zscore) %>%
                                  mutate(type = "Deaths")) %>%
          dplyr::select(geo, keyword_en, cor, cor_nolag, lag, zscore, zscore_nolag, type) 
        
        # Merge Correlations with main data --------------------------------------------
        gtrends_full_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                             "gtrends_full_timeseries", 
                                             paste0("gtrends_otherdata_varclean_complete_",keyword_type,".Rds")))
        
        gtrends_full_df <- gtrends_full_df %>%
          dplyr::select(-contains(".y")) %>%
          dplyr::select(-contains(".x")) %>%
          dplyr::select(-contains("hits_tm")) %>%
          dplyr::select(-contains("hits_tp")) %>%
          dplyr::select(-contains("hits_t0")) %>%
          dplyr::select(-contains("gmobility")) %>%
          dplyr::select(-contains("days_since_")) %>%
          dplyr::select(-c(lastupdated, business_ease_index, status, h2_testing_policy,
                           pandemic_time, lending, StringencyIndex,
                           GovernmentResponseIndex, EconomicSupportIndex,
                           c8_international_travel_controls_first_date,
                           c_policy_2019, c_policy_2020, c_policy_first_date,
                           working_age_pop, mm_dd, population, urban_pop,
                           un_regionsub_name, year, cases, death)) %>%
          dplyr::filter(date >= as.Date(begin_day_i),
                        date <= as.Date(end_day_i))
        
        gc(); gc(); gc();
        
        gtrends_panel_df <- merge(gtrends_full_df, gtrends_cor_df, by = c("geo", "keyword_en"), all.x=T, all.y=F)
        
        # Merge "other day" with correlation data ------------------------------------
        # Merge non google trends data (eg, wdi) with correlations data
        gtrends_otherdata <- gtrends_full_df %>%
          dplyr::select(-contains("hits")) %>%
          dplyr::select(-contains("cases_new")) %>%
          dplyr::select(-contains("death_new")) %>%
          dplyr::select(-contains("days_since")) %>%
          dplyr::select(-contains("gmobility_")) %>%
          dplyr::select(-c(keyword, keyword_en, date)) %>% # cases, death
          distinct(geo, .keep_all = T)
        
        # gtrends_otherdata_sum <- gtrends_full_df %>%
        #   group_by(geo) %>%
        #   dplyr::summarise(h2_testing_policy_max = max(h2_testing_policy, na.rm=T),
        #                    h2_testing_policy_median = median(h2_testing_policy, na.rm=T)) %>%
        #   ungroup()
        # gtrends_otherdata_sum$h2_testing_policy_max[gtrends_otherdata_sum$h2_testing_policy_max %in% -Inf] <- NA
        # gtrends_otherdata_sum$h2_testing_policy_median[gtrends_otherdata_sum$h2_testing_policy_median %in% -Inf] <- NA
        # 
        cor_max_df <- cor_max_df %>%
          left_join(gtrends_otherdata, by = "geo") #%>%
        #left_join(gtrends_otherdata_sum, by = "geo")
        
        # Export ---------------------------------------------------------------------
        saveRDS(gtrends_panel_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                            "gtrends_full_timeseries",
                                            "correlation_datasets",
                                            paste0("gtrends_since",begin_day_i,"_until",end_day_i,"_",keyword_type,".Rds")))
        
        saveRDS(cor_max_df, file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                      "gtrends_full_timeseries",
                                      "correlation_datasets",
                                      paste0("correlations_gtrends_since",begin_day_i,"_until",end_day_i,"_",keyword_type,".Rds")))
        
        rm(gtrends_panel_df)
        rm(cor_max_df)
        rm(gtrends_full_df)
        gc(); gc(); gc();
        
      }
    }
  }
}
