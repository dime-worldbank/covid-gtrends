# Scrape Data from Google Trends

ISO_SCRAPE <- "BR"

# Grab admin codes to scrape ---------------------------------------------------
isocodes_all <- ISO_3166_2

if(ISO_SCRAPE %in% "BR"){
  isocodes <- isocodes_all %>% 
    janitor::clean_names() %>% 
    filter(str_detect(code, "BR-"),
           code != "BR-FN") %>% 
    dplyr::rename(sub_code = code) %>% 
    dplyr::select(sub_code, name)
}

if(ISO_SCRAPE %in% "MZ"){
  isocodes <- isocodes_all %>% 
    janitor::clean_names() %>% 
    filter(str_detect(code, "MZ-"),
           code != "MZ-MPM") %>% 
    dplyr::rename(sub_code = code) %>% 
    dplyr::select(sub_code, name)
}

# Scrape Data ------------------------------------------------------------------

## Initialize dataframe
trends_df_all <- data.frame(NULL)

## Loop through search terms
for(term in c("meus olhos doem",
              "estou com febre",
              "febre",
              "tosse",
              "sintomas do coronavírus",
              "isolamento")){
  
  ## Scrape for specific localities
  trends_df <- lapply(isocodes$sub_code, function(iso_i){
  
    
    tryCatch({  
      
      out <- gtrends(term, 
                     category = "0",
                     geo = iso_i,
                     time = "today 3-m",
                     onlyInterest=T,
                     low_search_volume=T)
      
      print(iso_i)
      
      Sys.sleep(.1)
      
      out_df <- out$interest_over_time
      out_df$iso <- iso_i
      for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
      
      return(out_df)
    }
    , 
    error = function(e) return(NULL)
    )
    
    
  }) %>% 
    bind_rows()
  
  trends_df_all <- bind_rows(trends_df_all, trends_df)
}

# Export -----------------------------------------------------------------------
saveRDS(trends_df_all, file.path(dropbox_file_path, "Data", "google_trends", "RawData", paste0(ISO_SCRAPE, "_timeseries_raw.Rds")))
write.csv(trends_df_all, file.path(dropbox_file_path, "Data", "google_trends", "RawData", paste0(ISO_SCRAPE, "_timeseries_raw.csv")), row.names = F)


