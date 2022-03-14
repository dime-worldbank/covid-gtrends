# Scrapes google trends, using both a comparison state and without a comparison 
# state

#### PARAMETERS
comparison_iso <- "BR-SP"
scrape_group <- 31:37 # can be integer or vector: eg, 1 or 1:5

overwrite_files <- F

# Terms to Scrape --------------------------------------------------------------
keywords <- read.csv(file.path(dropbox_file_path, "Data", "google_trends", "covid_keywords.csv"),
                     stringsAsFactors = F)

keywords <- keywords[keywords$scrape %in% "yes",]

# Clean keyword
keywords$keyword <- keywords$keyword %>% tolower()

# ISO Codes --------------------------------------------------------------------
isocodes <- ISO_3166_2 # from ISOcodes package

br_isocodes <- isocodes %>% 
  janitor::clean_names() %>% 
  filter(str_detect(code, "BR-"),
         code != "BR-FN") %>% 
  dplyr::rename(sub_code = code) %>% 
  dplyr::select(sub_code, name)

# Function to Scrape Data ------------------------------------------------------
extract_trends <- function(iso_i,
                           term_i, 
                           comparison_iso, 
                           sleep_time = 10,
                           also_scrape_without_cstate = T){
  
  print(iso_i)
  
  #tryCatch({  
  
  #### 1. Scrape
  
  # Without comparison state
  if(also_scrape_without_cstate){
    
    out <- gtrends(term_i, 
                   geo = iso_i,
                   time = "2020-01-01 2020-06-30",
                   onlyInterest=T,
                   low_search_volume=T)
    
    out_df <- out$interest_over_time
    for(var in names(out_df)) out_df[[var]] <- out_df[[var]] %>% as.character()
  }
  
  
  # Didn't return error, but no hits? Object will be null, which will cause
  # error later. In this case, we just want to skip.
  if((class(out)[1] %in% "gtrends") & is.null(out_df)){
    out_all_df <- NULL
  } else{
    
    # With comparison state
    out_cstate <- gtrends(term_i, 
                          geo = c(iso_i, 
                                  comparison_iso) %>%
                            unique(),
                          time = "2020-01-01 2020-06-30",
                          onlyInterest=T,
                          low_search_volume=T)
    
    out_cstate_df <- out_cstate$interest_over_time
    for(var in names(out_cstate_df)) out_cstate_df[[var]] <- out_cstate_df[[var]] %>% as.character()
    
    
    
    #### 2. Prep comparison state output
    out_cstate_df <- out_cstate_df %>%
      dplyr::rename(hits_with_compstate = hits)
    
    ## Add hits of comparison state as variable (go from long to wide)
    if(iso_i != comparison_iso){
      
      # Grab hits of comparison state
      out_cstate_compstate_df <- out_cstate_df %>%
        filter(geo == comparison_iso) %>%
        dplyr::select(date, hits_with_compstate) %>%
        dplyr::rename(hits_compstate = hits_with_compstate)
      
      # Restrict to state of interest (remove comparison state), and merge
      # hits of comparison state
      out_cstate_df <- out_cstate_df %>%
        filter(geo != comparison_iso) %>%
        left_join(out_cstate_compstate_df, by = "date")
    } else{
      out_cstate_df$hits_compstate = out_cstate_df$hits_with_compstate
    }
    
    
    
    #### 3. Merge datasets with comp state and without comp state
    if(also_scrape_without_cstate){
      out_all_df <- out_df %>%
        dplyr::select(date, hits) %>%
        left_join(out_cstate_df, by = "date")
    } else{
      out_all_df <- out_cstate_df
    }
    
  }
  #### 4. Take a quick nap b/c of google rate limits
  Sys.sleep(sleep_time + runif(1)*2)
  
  print(nrow(out_all_df))
  return(out_all_df)
  #}, 
  #error = function(e) return(NULL)
  #)
  
}

# Scrape Data ------------------------------------------------------------------
# Nested for loop isn't ideal, but works so oh well.
for(term_i in keywords$keyword[keywords$scrape_group %in% scrape_group]){
  
  out_path <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                        "brazil_with_ref_state_by_keyword",
                        paste0("br_gtrends_ref",comparison_iso,
                               "_term",term_i,
                               ".Rds"))
  
  if(!file.exists(out_path) | overwrite_files){
    print(paste(term_i, "------------------------------------------------------"))
    
    
    
    tryCatch({
      
      
      
      term_df <- lapply(br_isocodes$sub_code,
                        extract_trends,
                        term_i,
                        comparison_iso) %>%
        bind_rows()
      
      saveRDS(term_df, out_path)
      
      Sys.sleep(60) # pause after each term
      
      
      
      
      
    }, error=function(e){})
    
    
    
    
    
  }
  
  
}


