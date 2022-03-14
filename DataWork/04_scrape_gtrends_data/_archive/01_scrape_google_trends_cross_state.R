# Scrape Data from Google Trends

# Grab admin codes to scrape ---------------------------------------------------
data("countries")
brazil_search <- countries[grepl("BR-", countries$sub_code),] 
brazil_search$sub_code <- brazil_search$sub_code %>% as.character()
brazil_search$name <- brazil_search$name %>% as.character()
brazil_search$id <- 1:nrow(brazil_search)

# Parameters -------------------------------------------------------------------
search_terms <- c(
  "tosse",
  "febre",
  "dificuldade ao respirar",
  "perda de olfato",
  "dor nos olhos"
)

# Within for loop, date got converted to numeric - so make character here.
start_dates <- seq("2020-01-01" %>% as.Date(), 
                   "2020-04-21" %>% as.Date(), 
                   by="week") %>%
  as.character()

# Scraping ---------------------------------------------------------------------
for(start_date_i in start_dates){
  
  print(paste(start_date_i, "------------------------------------------------"))
  
  end_date_i <- as.Date(start_date_i) + 6
  search_dates <- paste(start_date_i, end_date_i)

  results_df <- lapply(search_terms, 
                       function(term){
                         
                         # Where are we?
                         print(term)
                         
                         # Scrape data
                         out_all <- gtrends(term, 
                                            category = "0",
                                            geo = "BR",
                                            time = search_dates,
                                            onlyInterest=F,
                                            low_search_volume=T)
                         
                         # Grab relevant elements
                         out_region <- out_all$interest_by_region
                         out_city <- out_all$interest_by_city
                         
                         out_region$unit_level <- "region"
                         out_city$unit_level <- "city"
                         
                         out <- bind_rows(out_region,
                                          out_city) %>%
                           mutate(dates = search_dates)
                         
                         return(out)
                         
                       }) %>%
    bind_rows()
  
  # Export -----------------------------------------------------------------------
  saveRDS(results_df, file.path(dropbox_file_path, "Data", "google_trends", "RawData", "cross_state_weekly", paste0("brazil_extract_crosstate_",search_dates,".Rds")))
  write.csv(results_df, file.path(dropbox_file_path, "Data", "google_trends", "RawData", "cross_state_weekly", paste0("brazil_extract_crosstate_",search_dates,".csv")), row.names=F)
}
