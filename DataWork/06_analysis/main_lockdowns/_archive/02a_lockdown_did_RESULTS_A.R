# Lockdown Difference-in-Difference Analysis

lm_post_confint_tidy <- function(lm){
  
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint$tvalue <- summary(lm)$coefficients[,3] %>% as.vector()
  lm_confint$pvalue <- summary(lm)$coefficients[,4] %>% as.vector()
  
  return(lm_confint)
}

# Load / Prep Data -------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_df <- gtrends_df %>%
  dplyr::mutate(days_since_c_policy_yearcurrent_post_X_year2020 = 
                  days_since_c_policy_yearcurrent_post*pandemic_time) %>%
  dplyr::mutate(week = date %>% week,
                wday = date %>% wday)

# https://stackoverflow.com/questions/14169620/add-a-month-to-a-date
max_lockdown_date <- gtrends_df$lockdown_date_min %>% 
  max(na.rm=T) %m+% 
  months(1) %>%
  str_replace("2020-", "")

#gtrends_df <- gtrends_df[gtrends_df$mm_dd <= max_lockdown_date,]

# Log
gtrends_df$hits_ma7_log <- gtrends_df$hits_ma7 + abs(min(gtrends_df$hits_ma7, na.rm=T))
gtrends_df$hits_ma7_log <- log(gtrends_df$hits_ma7_log+1)

# Regressions ------------------------------------------------------------------
keywords_en_use <- c("social distance", "stay at home", "boredom", "anxiety", "suicide",
                     "insomnia", "social isolation", "loneliness", "divorce",
                     "panic attack",
                     "fever",
                     #"worried health", 
                     "hysteria", "overwhelmed", "anxiety symptoms",
                     "anxiety attack", "symptoms of panic attack",
                     "depressed", "lonely", "suicidal", "abuse",
                     "therapist near me", 
                     "online therapist",
                     "deep breathing", "body scan meditation",
                     "unemployment", "unemployment insurance")

keywords_en_use <- c("social distance",
                     "stay at home",
                     "unemployment",
                     "unemployment insurance",
                     "boredom",
                     "anxiety",
                     "anxiety attack",
                     "anxiety symptoms",
                     "overwhelmed", # panic
                     "hysteria",
                     "suicide",
                     "insomnia",
                     "overwhelmed",
                     "social isolation",
                     "lonely",
                     "loneliness",
                     "divorce")

run_reg <- function(keyword_i, region_i){
  # Function to estimate model for keyword_i and region_i
  print(region_i)
  print(keyword_i)
  
  df_i <- gtrends_df[gtrends_df$keyword_en %in% keyword_i,]
  if(region_i != "Global"){
    df_i <- df_i[df_i$wb_region %in% region_i,]
  }
  
  df_i <- df_i[!is.na(df_i$hits_ma7),]
  df_i <- df_i[!is.na(df_i$days_since_c_policy_yearcurrent_post_X_year2020),] # must be a lockdown
  if((nrow(df_i) > 0) & (length(unique(df_i$year2020)) > 1)){
    
    # FE: mm_dd
    
    out <- felm(hits_ma7_log ~ year2020 + 
                  days_since_c_policy_yearcurrent_post + 
                  days_since_c_policy_yearcurrent_post_X_year2020 | week + wday + geo | 0 | geo, 
                data = df_i) %>%
      lm_post_confint_tidy() %>%
      dplyr::mutate(keyword = keyword_i,
                    region = region_i)
  } else{
    out <- data.frame(NULL)
  }
  
  return(out)
}

## Grab list of regions
wb_regions_all <- gtrends_df$wb_region %>% 
  unique() %>% 
  na.omit() %>% 
  as.character()

global_results_df <- map_df(keywords_en_use, run_reg, "Global")
region_results_df <- map_df(wb_regions_all, function(region_i){
  map_df(keywords_en_use, run_reg, region_i)
})

results_all <- bind_rows(global_results_df, region_results_df)

# Export Results ---------------------------------------------------------------
saveRDS(results_all,
        file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                  "did_results.Rds"))


