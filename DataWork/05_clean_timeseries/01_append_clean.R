# Append and Clean Google Trends Data

# Load Data --------------------------------------------------------------------
# Load data. Scraped from multiple time periods, which load separately. Use
# data that uses Jan 1 2020 as "base" (0), then have datasets m[inus] 1 and 2
# from base and datasets p[lus] 1 and 2 from base.

clean_google_data <- function(df, keyword_type_vec){
  # Cleaning individual google trends files
  
  df$hits[df$hits %in% "<1"] <- "1"
  df$hits <- df$hits %>% as.numeric()
  
  df <- df %>%
    dplyr::select(date, keyword_en, geo, language, hits) %>%
    dplyr::mutate(keyword_en = tolower(keyword_en)) %>%
    dplyr::filter(keyword_en %in% keyword_type_vec) %>%
    tidyr::complete(date,
                    keyword_en = keyword_type_vec,
                    nesting(geo, language),
                    fill = list(hits = 0))
  
  df$keyword_en <- df$keyword_en %>% tolower()
  
  return(df)
}

for(keyword_type in c("symptoms", "contain", "vaccine")){
  print(paste(keyword_type, "================================================"))
  
  if(keyword_type == "symptoms") keyword_type_vec <- tolower(KEYWORDS_SYMTPOMS)
  if(keyword_type == "contain")  keyword_type_vec <- tolower(KEYWORDS_CONTAIN_USE)
  if(keyword_type == "vaccine")  keyword_type_vec <- tolower(VACCINE_KEYWORDS)
  
  gtrends_m3_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData", "timeseries") %>%
    list.files(pattern = "gtrends_date2018-09-01 2019-05-28*", full.names = T) %>%
    map_df(readRDS) %>%
    clean_google_data(keyword_type_vec = keyword_type_vec) %>%
    dplyr::rename(hits_tm3 = hits) 
  
  gtrends_m2_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData", "timeseries") %>%
    list.files(pattern = "gtrends_date2019-01-01 2019-09-27*", full.names = T) %>%
    map_df(readRDS) %>%
    clean_google_data(keyword_type_vec = keyword_type_vec) %>%
    dplyr::rename(hits_tm2 = hits) 
  
  gtrends_m1_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData", "timeseries") %>%
    list.files(pattern = "gtrends_date2019-07-01 2020-03-26*", full.names = T) %>%
    map_df(readRDS) %>%
    clean_google_data(keyword_type_vec = keyword_type_vec) %>%
    dplyr::rename(hits_tm1 = hits)
  
  gtrends_0_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData", "timeseries") %>%
    list.files(pattern = "gtrends_date2020-01-01 2020-09-26*", full.names = T) %>%
    map_df(readRDS) %>%
    clean_google_data(keyword_type_vec = keyword_type_vec) %>%
    dplyr::rename(hits_t0 = hits) 
  
  gtrends_p1_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData", "timeseries") %>%
    list.files(pattern = "gtrends_date2020-07-05 2021-03-31*", full.names = T) %>%
    map_df(readRDS) %>%
    clean_google_data(keyword_type_vec = keyword_type_vec) %>%
    dplyr::rename(hits_tp1 = hits) 
  
  gtrends_p2_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData", "timeseries") %>%
    list.files(pattern = "gtrends_date2021-01-04 2021-09-30*", full.names = T) %>%
    map_df(readRDS) %>%
    clean_google_data(keyword_type_vec = keyword_type_vec) %>%
    dplyr::rename(hits_tp2 = hits) 
  
  gtrends_p3_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData", "timeseries") %>%
    list.files(pattern = "gtrends_date2021-04-06 2021-12-31*", full.names = T) %>%
    map_df(readRDS) %>%
    clean_google_data(keyword_type_vec = keyword_type_vec) %>%
    dplyr::rename(hits_tp3 = hits) 
  
  # Merge ------------------------------------------------------------------------
  gtrends_df <- gtrends_0_df %>%
    full_join(gtrends_m3_df, by = c("date", "keyword_en", "geo", "language")) %>%
    full_join(gtrends_m2_df, by = c("date", "keyword_en", "geo", "language")) %>%
    full_join(gtrends_m1_df, by = c("date", "keyword_en", "geo", "language")) %>%
    full_join(gtrends_p1_df, by = c("date", "keyword_en", "geo", "language")) %>%
    full_join(gtrends_p2_df, by = c("date", "keyword_en", "geo", "language")) %>%
    full_join(gtrends_p3_df, by = c("date", "keyword_en", "geo", "language"))
  
  # Blend hits -------------------------------------------------------------------
  # https://stats.stackexchange.com/questions/281162/scale-a-number-between-a-range
  
  make_consistent_hits_var <- function(gtrends_df, 
                                       hits_1, 
                                       hits_2, 
                                       newname = NULL){
    # Scale [hits_2] to be like [hits_1]
    # ARGS
    # --gtrends_df: Dataframe
    # --hits_1: Name of first hits variable (string)
    # --hits_2: Name of first hits variable (string) [this variable will be adjusted]
    # --newname: New variable name for new/adjusted hits variables (if NULL, named "hits")
    
    ## Add vars
    gtrends_df$hits_1 <- gtrends_df[[hits_1]]
    gtrends_df$hits_2 <- gtrends_df[[hits_2]]
    
    ## Scale hits_2 to be like hits_1
    gtrends_df <- gtrends_df %>%
      dplyr::mutate(overlap = !is.na(hits_1) & !is.na(hits_2)) %>%
      dplyr::group_by(keyword_en, geo, language) %>%
      dplyr::mutate(hits_1_o_max = max(hits_1[overlap %in% T]),
                    hits_1_o_min = min(hits_1[overlap %in% T]),
                    hits_2_o_max = max(hits_2[overlap %in% T]),
                    hits_2_o_min = min(hits_2[overlap %in% T])) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(hits_2_scaled_like_hits1 = 
                      ((hits_2 - hits_2_o_min) / (hits_2_o_max - hits_2_o_min)) *
                      (hits_1_o_max - hits_1_o_min) +
                      hits_1_o_min)
    
    ## Do certain keywords not overlap for any dates?
    gtrends_df <- gtrends_df %>%
      dplyr::group_by(keyword_en, geo, language) %>%
      dplyr::mutate(overlap_anytime = max(overlap)) %>%
      ungroup()
    
    ## Create fill time series of hits value
    # Use hits_1 versus scaled version
    gtrends_df$hits <- gtrends_df$hits_1
    gtrends_df$hits[is.na(gtrends_df$hits)] <- gtrends_df$hits_2_scaled_like_hits1[is.na(gtrends_df$hits)]
    
    # If no overlap, use the original value
    nooverlap_time1 <- (gtrends_df$overlap_anytime %in% 0) & !is.na(gtrends_df$hits_1)
    nooverlap_time2 <- (gtrends_df$overlap_anytime %in% 0) & !is.na(gtrends_df$hits_2)
    
    gtrends_df$hits[nooverlap_time1] <- gtrends_df$hits_1[nooverlap_time1]
    gtrends_df$hits[nooverlap_time2] <- gtrends_df$hits_2[nooverlap_time2]
    
    ## Cleanup Variables
    gtrends_df <- gtrends_df %>%
      mutate(hits = hits %>% as.numeric(),
             date = date %>% as.Date()) %>%
      dplyr::select(-c(hits_1_o_max, hits_1_o_min,
                       hits_2_o_max, hits_2_o_min,
                       hits_2_scaled_like_hits1,
                       hits_1, hits_2,
                       overlap_anytime,
                       overlap)) %>%
      ungroup()
    
    gtrends_df$hits[gtrends_df$hits %in% Inf] <- NA
    
    if(!is.null(newname)){
      gtrends_df[[newname]] <- gtrends_df$hits
      gtrends_df$hits <- NULL
    }
    
    
    return(gtrends_df)
  }
  
  # Dividing by hits value in some cases, where hits can range from 0 to 100.
  # Add 1 so don't have zeros
  #gtrends_df <- gtrends_df %>%
  #  dplyr::mutate(hits_t1 = hits_t1 + 1,
  #                hits_t2 = hits_t2 + 1)
  gtrends_df <- make_consistent_hits_var(gtrends_df, "hits_t0",      "hits_tm1", "hits_tm1_adj")
  gtrends_df <- make_consistent_hits_var(gtrends_df, "hits_tm1_adj", "hits_tm2", "hits_tm2_adj")
  gtrends_df <- make_consistent_hits_var(gtrends_df, "hits_tm2_adj", "hits_tm3", "hits_tm3_adj")
  gtrends_df <- make_consistent_hits_var(gtrends_df, "hits_tm3_adj", "hits_tp1", "hits_tp1_adj")
  gtrends_df <- make_consistent_hits_var(gtrends_df, "hits_tp1_adj", "hits_tp2", "hits_tp2_adj")
  gtrends_df <- make_consistent_hits_var(gtrends_df, "hits_tp2_adj", "hits_tp3", "hits_tp3_adj")
  
  # Cleanup ----------------------------------------------------------------------
  gtrends_df <- gtrends_df %>%
    dplyr::select(keyword_en, geo, date, language,
                  hits_tm1, hits_tm2, hits_tm3, hits_tp1, hits_tp2, hits_tp3, hits_t0,
                  hits_tp3_adj) %>%
    dplyr::mutate(date = date %>% as.Date()) %>%
    dplyr::rename(hits = hits_tp3_adj) 
  
  gtrends_df$hits[is.na(gtrends_df$hits)] <- 0
  
  ## Merge in original keyword (in original language
  keywords_df <- readRDS(file.path(keywords_dir, "FinalData", "covid_keywords_alllanguages.Rds"))
  
  keywords_df <- keywords_df %>%
    dplyr::select(-c(category, scrape, priority_to_scrape, reference)) %>%
    pivot_longer(-c(id, keyword_en)) %>%
    dplyr::rename(keyword = value,
                  language = name) %>%
    dplyr::mutate(language = language %>% str_replace_all("keyword_", ""),
                  keyword_en = keyword_en %>% tolower()) %>%
    dplyr::select(-id) %>%
    distinct() 
  
  gtrends_df <- left_join(gtrends_df, keywords_df,
                          by = c("language", "keyword_en"))
  
  gtrends_df$keyword[gtrends_df$language %in% "en"] <- 
    gtrends_df$keyword_en[gtrends_df$language %in% "en"]
  
  # Subset for select keyword types --------------------------------------------
  if(keyword_type == "vaccine"){
    gtrends_df <- gtrends_df %>%
      dplyr::filter(date >= ymd("2020-06-01"))
  }
  
  # Save Data --------------------------------------------------------------------
  saveRDS(gtrends_df, file.path(gtrends_dir, "FinalData",
                                "gtrends_full_timeseries", 
                                paste0("gtrends_complete_",keyword_type,".Rds")))
}



