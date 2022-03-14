# Scrape Google Trends Timeseries

# DESCRIPTION: Loops through countries and search terms and saves the search
# interest time series for a specified time period. Saves a separate dataset for
# each search; if alredy searched, then skips. (Saving datasets separately and skiping
# enables rerunning the code when new search terms have been added and having the code
# only scrape those search terms).

#Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

library(dplyr)
library(stringr)
library(tidyr)
library(gtrendsR)

# Setup ------------------------------------------------------------------------
## Parameters
SLEEP_TIME      <- 0.2 # number of seconds to pause after each scrape
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
                       "timeseries_2021-01-04_2021-09-30") 

GTRENDS_TO_SCRAPE <- GTRENDS_TO_SCRAPE[1]

#"timeseries_regions_2020-12-01_2021-05-31",
#"timeseries_regions_2020-12-01_2021-07-31"

## Which countries to use when scraping [timeseries_region]
select_countries_vec <- c("US")

scraped_files_df <- readRDS(file.path("TO_UPLOAD", "scraped_files.Rds"))

# Keywords ---------------------------------------------------------------------
# Keywords to use to evaluate COVID containement policies
KEYWORDS_CONTAIN_USE <- c("social distance",
                          "stay at home",
                          
                          "unemployment",
                          "unemployment insurance",
                          "unemployment benefits",
                          "unemployment office",
                          "file for unemployment",
                          "debt",
                          
                          "boredom",
                          "anxiety",
                          "anxiety attack",
                          "anxiety symptoms",
                          "overwhelmed", 
                          "panic",
                          "hysteria",
                          "suicide",
                          "insomnia",
                          "social isolation",
                          "lonely",
                          "loneliness",
                          "divorce",
                          #"condom",
                          "emergency pill",
                          "pregnancy test", 
                          #"abortion",
                          #"plan child",
                          #"plan other children",
                          "tinder",
                          #"relationship",
                          # "break up", 
                          "wedding", 
                          "dating app") 

KEYWORDS_SYMTPOMS <- c("loss of smell",
                       "loss of taste",
                       "i can't smell",
                       "i can't taste",
                       "ageusia",
                       "anosmia",
                       "pneumonia",
                       "cough",
                       "fever",
                       "shortness of breath",
                       "how to treat coronavirus",
                       "covid symptoms",
                       "coronavirus",
                       "covid-19")

VACCINE_KEYWORDS <- c("antivaccines",
                      "ivermectin",
                      "is vaccine approved",
                      "vaccine",
                      "vaccine conspiracy",
                      "vaccine allergy",
                      "vaccine mercury",
                      "vaccine aluminum",
                      "vaccine dna",
                      "vaccine appointment",
                      "vaccine approved",
                      #"vaccine damage",
                      #"vaccine fraud",
                      "vaccine harm",
                      "vaccine injuries",
                      "vaccine near me",
                      "vaccines are poison",
                      "vaccines kill",
                      "can i get the vaccine",
                      "covid vaccine",
                      "covid vaccine side effects",
                      "covid vaccine safety",
                      "covid vaccine infertility",
                      "covid vaccine cause infertility",
                      "does covid vaccine change dna",
                      "covid vaccine change dna",
                      "safety of covid vaccine",
                      "covid vaccine safety",
                      "covid vaccine dangerous",
                      "covid microchip")

## Which keywords to scrape for [timeseries]. For [timeseries_region], uses
# vaccine and missinformation related keywords
keywords_en_timeseries <- c(VACCINE_KEYWORDS) #%>% sort()

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
keywords_df <- readRDS(file.path("TO_UPLOAD", "covid_keywords_alllanguages.Rds"))

## Language Dataset
# Indicates which language to use for each country. 
languages <- readRDS(file.path("TO_UPLOAD",
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
    ALL_TERMS <- F
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
  
  start_end_date <- GTRENDS_TO_SCRAPE_i %>% 
    str_replace_all("timeseries_regions_", "") %>% 
    str_replace_all("timeseriesALL_", "") %>%
    str_replace_all("timeseries_", "") %>%
    str_replace_all("_", " ")
  
  vac_words <- keywords_df$keyword_en[keywords_df$category %>% str_detect("vaccine")]
  
  #keywords_en_timeseries <- c(keywords_en_timeseries, vac_words) %>% unique()
  keywords_en_timeseries <- c(keywords_en_timeseries, vac_words) %>% unique()
  
  if(ALL_TERMS){
    keywords_sub_df <- keywords_df[keywords_df$keyword_en %in% keywords_en_timeseries,]
  } else{
    keywords_sub_df <- keywords_df %>%
      dplyr::filter(category %in% c("vaccine", "missinformation", "us election missinformation"))
  }
  
  ## Check if root folder exists; if not, create
  # (dir.create only creates if doesn't already exist)
  OUT_FOLDER <- GTRENDS_TO_SCRAPE_i %>%
    str_replace_all(str_replace_all(start_end_date, " ", "_"),"") %>%
    str_replace_all("_$", "")
  
  dir.create(file.path("TO_UPLOAD", OUT_FOLDER))
  
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
    if(!ALL_COUNTRIES) iso2 <- iso2[iso2 %in% select_countries_vec]
    if(grepl("timeseriesALL_", GTRENDS_TO_SCRAPE_i)) iso2 <- "all"
    
    ## SCRAPE DATA
    for(term_id_i in 1:nrow(keywords_sub_df_i)){
      for(iso_i in iso2){
        
        term_i <- keywords_sub_df_i$term_to_scrape[term_id_i]
        term_en_i <- keywords_sub_df_i$keyword_en[term_id_i]
        
        out_path <- file.path("TO_UPLOAD",
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
        
        EXISTS <- paste0("gtrends_date",
                         start_end_date,
                         "_iso",
                         iso_i, 
                         "_termen",
                         term_en_i,
                         "_language",
                         language,
                         ".Rds") %in% scraped_files_df$file_name
        
        if(!EXISTS){
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
}




