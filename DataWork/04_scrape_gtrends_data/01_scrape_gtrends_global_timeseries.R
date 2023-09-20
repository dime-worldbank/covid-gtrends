# Scrape Google Trends Timeseries

# DESCRIPTION: Loops through countries and search terms and saves the search
# interest time series for a specified time period. Saves a separate dataset for
# each search; if alredy searched, then skips. (Saving datasets separately and skiping
# enables rerunning the code when new search terms have been added and having the code
# only scrape those search terms).

#Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Setup ------------------------------------------------------------------------
## Parameters
SLEEP_TIME      <- 5 # number of seconds to pause after each scrape
overwrite_files <- F # overwrite data?
rev_language_codes <- T
LANGUAGE_SUBSET <- "all" # "all", "en_only", "no_en"

## Timeframe to scrape
# -- [timeseries]_[begin date]_[end date] to scrape time series data for all 
#   countries
# -- [timeseries_regions]_[begin date]_[end date] to only scrape data for the US
#    for select keywords and to capture data at the State level; does not 
#    capture the time series

GTRENDS_TO_SCRAPE <- c("timeseries_2018-09-01_2019-05-28",
                       "timeseries_2019-01-01_2019-09-27",
                       "timeseries_2019-07-01_2020-03-26",
                       "timeseries_2020-01-01_2020-09-26",
                       "timeseries_2020-07-05_2021-03-31",
                       "timeseries_2021-01-04_2021-09-30",
                       "timeseries_2021-04-06_2021-12-31",
                       "timeseries_2021-10-01_2022-06-27", 
                       "timeseries_2022-04-06_2022-12-31") %>%
  rev()

## Which countries to use when scraping [timeseries_region]
regions_countries_vec <- c("US")

## Which keywords to scrape for [timeseries]. 
keywords_en_timeseries <- KEYWORDS_TIMESERIES_ALL

# Function to Scrape Google Data -----------------------------------------------
extract_trends <- function(iso_i,
                           term_i, 
                           term_en_i,
                           language,
                           start_end_date,
                           onlyInterest,
                           sleep_time){
  # DESCRIPTION: Given an country, term, and start/end date, scrapes the data. 
  # Asks for the language code to add to the resulting dataset. sleep_time is done
  # because of google rate limits.
  # ARGS
  # -- iso_i: Two-letter country code
  # -- term_i: Term to scrape
  # -- term_en_i: English version of the search term to scrape
  # -- language: Language code
  # -- start_end_date: Start/end date to scrape [yyyy-mm-dd yyyy-mm-dd]
  # -- onlyInterest: T/F; where to only grab search interest data
  # -- sleep_time: Time to pause code after each scrape (do b/c of rate limiting)
  
  # 1. Scrape
  if(iso_i == "all"){
    out <- gtrends(term_i, 
                   time = start_end_date,
                   onlyInterest = onlyInterest,
                   low_search_volume=T)
  } else{
    out <- gtrends(term_i, 
                   geo = iso_i,
                   time = start_end_date,
                   onlyInterest = onlyInterest,
                   low_search_volume=T)
  }
  
  if(onlyInterest %in% T){
    
    # 2. Grab data, and convert variables to character to avoid type conflict late
    out_df <- out$interest_over_time
    for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
    
    # 3. Error check
    # Didn't return error, but no hits. Object will be null, which will cause
    # error later. In this case, we just want to skip.
    if((class(out)[1] %in% "gtrends") & is.null(out_df)){
      out_df <- data.frame(NULL)
    } else{
      out_df$language <- language
      out_df$keyword_en <- term_en_i
    }
    
    # 4. Take a quick nap b/c of google rate limits
    Sys.sleep(sleep_time + runif(1))
    
    print(iso_i)
    print(nrow(out_df))
  } else{
    out_df <- out
  }
  
  return(out_df)
}

# Load / Initial Data Prep -----------------------------------------------------

## Keywords Dataset
keywords_df <- readRDS(file.path(keywords_dir, "FinalData", "covid_keywords_alllanguages.Rds"))

keywords_en_timeseries <- c(keywords_en_timeseries) %>% unique()

## Language Dataset
# Indicates which language to use for each country. 
languages <- readRDS(file.path(prim_lang_dir, 
                               "FinalData",
                               "country_language.Rds"))

language_codes_all <- languages$language_best %>% unique()
language_codes_all <- language_codes_all[!is.na(language_codes_all)]
language_codes_all <- language_codes_all[language_codes_all != ""]
language_codes_all <- language_codes_all %>% sort()

if(LANGUAGE_SUBSET %in% "en_only"){
  language_codes_all <- "en"
}

if(LANGUAGE_SUBSET %in% "no_en"){
  language_codes_all <- language_codes_all[language_codes_all != "en"]
}

# Prep Parameters Based on Folder Name -----------------------------------------
for(GTRENDS_TO_SCRAPE_i in GTRENDS_TO_SCRAPE){
  
  if(grepl("timeseries_regions_", GTRENDS_TO_SCRAPE_i)){
    ALL_TERMS <- T # T
    ALL_COUNTRIES <- F
    onlyInterest <- F
  } else if(grepl("timeseriesALL_", GTRENDS_TO_SCRAPE_i)){
    ALL_TERMS <- T
    ALL_COUNTRIES <- T
    onlyInterest <- F
  } else{
    ALL_TERMS <- T
    ALL_COUNTRIES <- T
    onlyInterest <- T
  }
  
  ALL_TERMS <- F # TO DELETE!!
  
  start_end_date <- GTRENDS_TO_SCRAPE_i %>% 
    str_replace_all("timeseries_regions_", "") %>% 
    str_replace_all("timeseriesALL_", "") %>%
    str_replace_all("timeseries_", "") %>%
    str_replace_all("_", " ")
  
  if(ALL_TERMS){
    keywords_sub_df <- keywords_df[keywords_df$keyword_en %in% keywords_en_timeseries,]
  } else{
    
    # keywords_sub_df <- keywords_df %>%
    #   dplyr::filter(category %in% c("coronavirus general",
    #                                 "treatment",
    #                                 "vaccine", 
    #                                 "missinformation",
    #                                 "vaccine misinformation"))
    
    keywords_sub_df <- keywords_df %>%
      dplyr::filter(keyword_en %in% c("loss of smell",
                                      "loss of taste",
                                      "covid symptoms",
                                      "i can't smell",
                                      "anosmia",
                                      "ageusia",
                                      "covid-19",
                                      "i can't taste",
                                      "shortness of breath",
                                      "fever",
                                      "pneumonia",
                                      "how to treat coronavirus",
                                      "cough",
                                      "coronavirus"))
    
  }
  
  ## Check if root folder exists; if not, create
  # (dir.create only creates if doesn't already exist)
  OUT_FOLDER <- GTRENDS_TO_SCRAPE_i %>%
    str_replace_all(str_replace_all(start_end_date, " ", "_"),"") %>%
    str_replace_all("_$", "")
  
  dir.create(file.path(dropbox_file_path, "Data", "google_trends", "RawData", OUT_FOLDER))
  
  if(grepl("timeseriesALL_", GTRENDS_TO_SCRAPE_i)) language_codes_all <- "en"
  
  # Loop through languages, countries and terms --------------------------------
  if(rev_language_codes){
    language_codes_all <- rev(language_codes_all)
  }
  
  for(language in language_codes_all){
    
    ## KEYWORDS
    # Grab keyword for the language and cleanup keyword vector. Remove missing and
    # remove newlines
    keywords_sub_df_i <- keywords_sub_df
    
    keywords_sub_df_i$term_to_scrape <- keywords_sub_df[[paste0("keyword_", language)]] %>% 
      tolower() %>% 
      as.character() %>% 
      str_replace_all("\\n", "")
    
    keywords_sub_df_i <- keywords_sub_df_i[keywords_sub_df_i$term_to_scrape != "",]
    
    ## ISO CODES
    # Grab iso/country codes where the selected language is the main language
    iso2 <- languages$geo[languages$language_best %in% language]
    iso2 <- iso2[!is.na(iso2)]
    
    # In some cases, subset iso2 vector
    if(!ALL_COUNTRIES) iso2 <- iso2[iso2 %in% regions_countries_vec]
    if(grepl("timeseriesALL_", GTRENDS_TO_SCRAPE_i)) iso2 <- "all"
    
    ## SCRAPE DATA
    for(term_id_i in 1:nrow(keywords_sub_df_i)){
      for(iso_i in iso2){
        
        term_i <- keywords_sub_df_i$term_to_scrape[term_id_i]
        term_en_i <- keywords_sub_df_i$keyword_en[term_id_i]
        
        out_path <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                              OUT_FOLDER,
                              paste0("gtrends_date",
                                     start_end_date,
                                     "_iso",
                                     iso_i, 
                                     "_termen",
                                     term_en_i,
                                     "_language",
                                     language,
                                     ".Rds"))
        
        if(!file.exists(out_path) | overwrite_files){
          print(start_end_date)
          print(paste(language, iso_i, term_i, term_en_i, "------------------"))
          
          tryCatch({
            
            term_df <- extract_trends(iso_i,
                                      term_i,
                                      term_en_i,
                                      language,
                                      start_end_date,
                                      onlyInterest,
                                      SLEEP_TIME)
            
            saveRDS(term_df, out_path)
            
            Sys.sleep(0.01) # pause after each term
            
          }, error=function(e){})
          
        } 
        
      }
    }
    
    # end language loop
  }
}




