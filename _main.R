# The Evolution of the COVID-19 Pandemic Through the Lens of Google Searches
# Main Script

# Parameters -------------------------------------------------------------------
RUN_CODE <- T

# Whether to translate google keywords; requires a Google API key
TRANSLATE_GOOGLE_KEYWORDS <- F

# Whether to run code in a "fresh state"; that is deleting all (1) outputs (i.e.,
# figures and tables) and (2) final data (i.e., data transformed by code). Raw 
# data, or data manually downloaded, will not be deleted.
DELETE_OUTPUT    <- T
DELETE_FINALDATA <- T

# Whether to produce a .txt file that indicates how long it took the code to run.
EXPORT_TXT_REPORT_CODE_DURATION <- T

START_TIME <- Sys.time()

# Filepaths --------------------------------------------------------------------
#### Root Paths
if(Sys.info()[["user"]] == "robmarty"){ 
  dropbox_file_path <- "~/Dropbox/World Bank/Replication Packages/COVID Google Trends"
  github_file_path  <- "~/Documents/Github/covid-gtrends"
  paper_tables      <- "~/Dropbox/Apps/Overleaf/COVID-19 and Google Trends Paper/tables"
  paper_figures     <- "~/Dropbox/Apps/Overleaf/COVID-19 and Google Trends Paper/figures"
}

#### Paths from root
## Data
data_dir      <- file.path(dropbox_file_path, "Data")
who_covid_dir <- file.path(data_dir, "who_covid")
gtrends_dir   <- file.path(data_dir, "google_trends")
vaccine_dir   <- file.path(data_dir, "usa_vaccine")
oxpol_dir     <- file.path(data_dir, "oxford_covid_policy_tracker")
prim_lang_dir <- file.path(data_dir, "country_primary_language")
keywords_dir  <- file.path(data_dir, "google_keywords")
wdi_dir       <- file.path(data_dir, "wdi")
ex_mort_dir   <- file.path(data_dir, "excess_mortality")

## Code
datawork_dir <- file.path(github_file_path, "DataWork")

# Google API Key ---------------------------------------------------------------
# Load Google API key where the Google Translate API is enabled.
# "api_key" should be a character with the Google API key
if(TRANSLATE_GOOGLE_KEYWORDS){
  api_key <- "API-KEY-HERE"
}

# Packages ---------------------------------------------------------------------
## Install/Load Package Dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(gtrendsR, countrycode, parallel, pbmcapply, ggplot2, jsonlite,
               stringr, raster, scales, rmapshaper, sparkline, magick, magrittr,
               htmltools, data.table, plotly, ISOcodes, stringi, lubridate,
               purrr, tidytext, quanteda, qdap, SentimentAnalysis, sentimentr,
               tm, tokenizers, wordcloud, ggwordcloud, ggpubr, dplyr, sf,
               geofacet, readstata13, strucchange, forcats, ISOcodes, hrbrthemes,
               lexiconPT, textdata, tidyr, rgeos, tidylog, TTR, sparkline,
               shinydashboard, RColorBrewer, shinythemes, DT, rmarkdown, shiny,
               wesanderson, shinyWidgets, zoo, bcrypt, shinyjs, ngram, rtweet,
               stringdist, stringr, rgdal, rgeos, geosphere, htmlwidgets,
               tidyverse, sf, raster, leaflet, leaflet.extras, plotly,
               geosphere, data.table, formattable, tidyr, viridis, data.table,
               WDI, scales, rnaturalearth, sp, utf8, ggtext, stargazer, lfe,
               ggrepel, Rfast, tikzDevice, ISOcodes, ggthemes, gghalves,
               rnaturalearthdata, janitor, readxl, tsbox)

## User defined functions
source("https://raw.githubusercontent.com/ramarty/r_google_translate/main/r_google_translate.R")

# https://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

lm_post_confint_tidy <- function(lm){
  # Extract coefficients and confidence interval from regression coefficients
  
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint$tvalue <- summary(lm)$coefficients[,3] %>% as.vector()
  lm_confint$pvalue <- summary(lm)$coefficients[,4] %>% as.vector()
  
  return(lm_confint)
}

# Keywords ---------------------------------------------------------------------
# Define keywords to query from Google Trends API

# Keywords to use to evaluate COVID containment policies
KEYWORDS_CONTAIN_USE <- c("debt",
                          "file for unemployment",
                          "unemployment",
                          "unemployment insurance",
                          "unemployment benefits",
                          "unemployment office",
                          "anxiety",
                          "anxiety attack",
                          "boredom",
                          "insomnia",
                          "lonely",
                          "panic",
                          "social isolation",
                          "suicide",
                          "divorce",
                          "emergency pill",
                          "pregnancy test", 
                          "wedding", 
                          "social distance",
                          "stay at home") %>%
  tolower()

KEYWORDS_SYMTPOMS <- c("loss of smell",
                       "loss of taste",
                       "I can't smell",
                       "I can't taste",
                       "ageusia",
                       "anosmia",
                       "pneumonia",
                       "cough",
                       "fever",
                       "shortness of breath",
                       "how to treat coronavirus",
                       "covid symptoms",
                       "coronavirus",
                       "covid-19") %>%
  tolower()

KEYWORDS_TIMESERIES_ALL <- c(KEYWORDS_CONTAIN_USE, 
                             KEYWORDS_SYMTPOMS) 

# Run Fresh --------------------------------------------------------------------
# Whether to run code "fresh"; that is, where all figures and processed/final
# data are deleted, except data downloaded from Google trends.

if(DELETE_OUTPUT){
  
  TO_DELETE <- list.files(paper_figures, full.names = T)
  for(file_i in TO_DELETE) file.remove(file_i)
  
  TO_DELETE <- list.files(paper_tables, full.names = T)
  for(file_i in TO_DELETE) file.remove(file_i)
  
}

if(DELETE_FINALDATA){
  
  FINALDATA_FILES <- list.files(dropbox_file_path, 
                                pattern = "*.Rds", 
                                full.names = T,
                                recursive = T) %>%
    str_subset("FinalData")
  
  # Don't delete: covid_keywords_alllanguages.Rds; relies on Google API key
  FINALDATA_FILES <- FINALDATA_FILES[!(FINALDATA_FILES %>% str_detect("covid_keywords_alllanguages.Rds"))]
  
  # Don't delete data downloaded from Google
  DONT_DELETE <- file.path(prim_lang_dir, "FinalData", "gtrends_data") %>%
    list.files(pattern = "*.Rds",
               full.names = T)
  
  TO_DELETE <- FINALDATA_FILES[!(FINALDATA_FILES %in% DONT_DELETE)]
  for(file_i in TO_DELETE) file.remove(file_i)
}

# Scripts ----------------------------------------------------------------------
# List all variables in workspace until now. Before running some code below, 
# delete everything except this and clear the memory to avoid a memory error.
ORIGINAL_VARIABLES <- NA
ORIGINAL_VARIABLES <- ls()

if(RUN_CODE){
  
  # Process ancillary data -----------------------------------------------------
  process_anc_dir <- file.path(datawork_dir, "01_process_ancillary_data")
  
  ## COVID Case Data
  source(file.path(process_anc_dir, "01_covid_cases", "01_clean_data.R"))
  
  ## Oxford Policy Data
  source(file.path(process_anc_dir, "01_oxford_policy", "01_clean_data.R"))
  source(file.path(process_anc_dir, "01_oxford_policy", "02_create_first_date_lockdown_data.R"))
  source(file.path(process_anc_dir, "01_oxford_policy", "02_ox_policy_national_clean.R"))
  #source(file.path(process_anc_dir, "01_oxford_policy", "02_vaccine_clean.R"))
  #source(file.path(process_anc_dir, "01_oxford_policy", "02_create_first_date_vaccine_avail.R"))
  
  ## WDI
  source(file.path(process_anc_dir, "01_wdi", "01_download_wdi_data.R"))
  
  ## Country Language Data
  source(file.path(process_anc_dir, "02_countries_language_data", "01_clean_country_language_data.R"))
  
  # Translate Google Search Terms ----------------------------------------------
  if(TRANSLATE_GOOGLE_KEYWORDS){
    source(file.path(datawork_dir, "02_translate_search_terms", "01_translate_search_terms.R"))
  }
  
  # Determine Most Common Language ---------------------------------------------
  source(file.path(datawork_dir, "03_determine_most_common_language", "01_scrape_gtrends_mult_languages_per_country.R"))
  source(file.path(datawork_dir, "03_determine_most_common_language", "02_append_data.R"))
  
  # Scrape Google Trends Timeseries & US Data ----------------------------------
  source(file.path(datawork_dir, "04_scrape_gtrends_data", "01_scrape_gtrends_global_timeseries.R"))
  
  # Scrape Google Trends Data Across Terms in US -------------------------------
  # scrape_gtrends_us <- file.path(datawork_dir, "04_scrape_gtrends_us_data_across_terms")
  # 
  # #source(file.path(scrape_gtrends_us, "01_scrape_clean.R"))
  # 
  # source(file.path(scrape_gtrends_us, "01_scrape_clean_relative_ivermectin.R"))
  # source(file.path(scrape_gtrends_us, "02_append_usa_refivermectin.R"))
  
  # Clean Google Trends: Regional Data -----------------------------------------
  # source(file.path(datawork_dir, "05_clean_regions", "01_append_regional.R"))
  # source(file.path(datawork_dir, "05_clean_regions", "02_add_variables.R"))
  
  # Clean Google Trends: Global Timeseries Data --------------------------------
  # These scripts are memory intensive; before running each script, only keep
  # needed variables and clear memory.
  
  TO_DELETE <- ls()[!(ls() %in% ORIGINAL_VARIABLES)]
  rm(TO_DELETE); gc(); gc()
  source(file.path(datawork_dir, "05_clean_gtrends", "01_append_clean.R"))
  
  TO_DELETE <- ls()[!(ls() %in% ORIGINAL_VARIABLES)]
  rm(TO_DELETE); gc(); gc()
  source(file.path(datawork_dir, "05_clean_gtrends", "02_merge_other_data.R"))
  
  TO_DELETE <- ls()[!(ls() %in% ORIGINAL_VARIABLES)]
  rm(TO_DELETE); gc(); gc()
  source(file.path(datawork_dir, "05_clean_gtrends", "03_variable_construction.R"))
  
  TO_DELETE <- ls()[!(ls() %in% ORIGINAL_VARIABLES)]
  rm(TO_DELETE); gc(); gc()
  source(file.path(datawork_dir, "05_clean_gtrends", "04_correlations.R"))
  
  TO_DELETE <- ls()[!(ls() %in% ORIGINAL_VARIABLES)]
  rm(TO_DELETE); gc(); gc()
  
  # Analysis: Correlation with cases -------------------------------------------
  
  # Boxplots showing distribution of correlations and best lags
  # OUTPUTS:
  # -- cor_lag_fig.png
  # -- cor_corbest_fig.png
  source(file.path(datawork_dir, "06_analysis", "main_correlations", "correlation_boxplots.R"))
  
  # Figures showing trends in search interest & cases
  # OUTPUTS:
  # -- cases_vs_loss_of_smell_trends_topcountries.png
  # -- cases_vs_loss_of_smell_trends_allcountries.png
  source(file.path(datawork_dir, "06_analysis", "main_correlations", "cases_searchinterest_trends.R"))
  
  # Figures showing trends in search interest & cases for select countries
  # with recent surges in cases
  # OUTPUTS:
  # -- cases_vs_loss_of_smell_trends_omicron.png
  source(file.path(datawork_dir, "06_analysis", "main_correlations", "cases_searchinterest_trends_omicron.R"))
  
  
  # Map showing correlations across countries
  # OUTPUT:
  # -- cor_map.png
  source(file.path(datawork_dir, "06_analysis", "main_correlations", "map.R"))
  
  # Table of correlations and lags (for SI)
  # OUTPUT:
  # -- cor_lag_table.tex
  source(file.path(datawork_dir, "06_analysis", "main_correlations", "correlation_table.R"))
  
  # Regression explaining variation in correlation
  # OUTPUT:
  # -- lm_cor_[loss_of_smell/loss_of_taste/covid_symptoms].tex
  source(file.path(datawork_dir, "06_analysis", "main_correlations", "reg_explain_correlation.R"))
  
  # Scatterplots of covariates with correlations
  # OUTPUT:
  # -- cor_gdp_scatter.png
  # -- cor_internet_scatter.png
  # -- cor_mobilecell_scatter.png
  # -- cor_casestotal_scatter.png
  source(file.path(datawork_dir, "06_analysis", "main_correlations", "cor_covariate_scatterplot.R"))
  
  # Analysis: Impact of containment policies -----------------------------------
  
  # Contaiment policy analysis: Difference-in-Differences
  # OUTPUT:
  # -- did_pooled.png
  source(file.path(datawork_dir, "06_analysis", "main_lockdowns", "01_lockdown_did_pooled.R"))
  source(file.path(datawork_dir, "06_analysis", "main_lockdowns", "02_lockdown_did_pooled.R"))
  source(file.path(datawork_dir, "06_analysis", "main_lockdowns", "03_lockdown_did_pooled.R"))
  
  # Contaiment policy analysis: Event Study
  # OUTPUT:
  # -- did_pooled.png
  source(file.path(datawork_dir, "06_analysis", "main_lockdowns", "05_lockdown_eventstudy_global.R"))
  
  # Analysis: Vaccines ---------------------------------------------------------
  
  # # Global analysis of vaccines
  # # OUTPUT:
  # # -- vax_cor.png
  # source(file.path(datawork_dir, "06_analysis", "main_vaccines", "vaccine_global.R"))
  # 
  # # USA analysis of vaccines
  # # OUTPUT:
  # # -- vaccine_panels_[BEGIN_DATE]_[END_DATE].png
  # source(file.path(datawork_dir, "06_analysis", "main_vaccines", "vaccine_usa.R"))
  # 
  # Analysis: SI ---------------------------------------------------------------
  source(file.path(datawork_dir, "06_analysis", "si", "consistent_timeseries_example.R"))
  source(file.path(datawork_dir, "06_analysis", "si", "language_used_for_gtrends.R"))
  source(file.path(datawork_dir, "06_analysis", "si", "terms_queried_summary.R"))
  
}

#### Export: info on last code run
if(EXPORT_TXT_REPORT_CODE_DURATION){
  END_TIME <- Sys.time()
  
  sink(file.path(dropbox_file_path, "last_code_run_time.txt"))
  cat("Details from latest time script was run \n")
  cat("\n")
  cat("START TIME: ", as.character(START_TIME), "\n", sep = "")
  cat("END TIME: ", as.character(END_TIME), "\n", sep = "")
  cat("DURATION: ",
      difftime(END_TIME, START_TIME, units = "mins") %>%
        as.numeric() %>%
        round(2),
      " minutes \n", sep = "")
  cat("\n")
  cat("PARAMETERS\n")
  cat("RUN_CODE: ", RUN_CODE, "\n", sep = "")
  cat("TRANSLATE_GOOGLE_KEYWORDS: ", TRANSLATE_GOOGLE_KEYWORDS, "\n", sep = "")
  cat("DELETE_OUTPUT: ", DELETE_OUTPUT, "\n", sep = "")
  cat("DELETE_FINALDATA: ", DELETE_FINALDATA, "\n", sep = "")
  
  sink()
}

