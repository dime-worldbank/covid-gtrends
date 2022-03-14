# Append and clean search interest data
# US search interest; relative to ivermectin

google_df <- file.path(gtrends_dir, "RawData", "search_interest_refivermectin_across_terms_us") %>%
  list.files(full.names = T) %>%
  map_df(function(file_i){
    
    df <- readRDS(file_i)
    
    df_sum <- df$interest_over_time %>%
      dplyr::mutate(hits = hits %>% as.character(),
                    hits = case_when(hits %in% "<1" ~ "0.5",
                                     TRUE ~ hits),
                    hits = hits %>%
                      as.character() %>%
                      as.numeric()) %>%
      group_by(keyword) %>%
      dplyr::summarise(hits = mean(hits)) 
    
    value_notiver <- df_sum$hits[df_sum$keyword != "ivermectin"]
    value_iver    <- df_sum$hits[df_sum$keyword == "ivermectin"]
    
    value <- value_notiver/value_iver
    
    df_out <- data.frame(hits_rel_iver = value,
                         keyword = df_sum$keyword[df_sum$keyword != "ivermectin"],
                         time = df$interest_over_time$time[1])
    
    return(df_out)
  })

## Add in ivermectin (reference)
iver_df <- google_df %>%
  distinct(time, .keep_all = T)
iver_df$hits_rel_iver <- 1
iver_df$keyword <- "ivermectin"

google_df <- bind_rows(google_df, iver_df)
google_df$keyword_en <- google_df$keyword

# Categorize -------------------------------------------------------------------
google_df <- google_df %>%
  make_vax_cat() %>%
  dplyr::filter(!is.na(keyword_cat))

# Cleanup ----------------------------------------------------------------------
google_df <- google_df %>%
  dplyr::mutate(keyword_en = keyword_en %>%
                  tools::toTitleCase() %>%
                  str_replace_all("\\bi\\b", "I") %>%
                  str_replace_all("^where\\b", "Where") %>%
                  str_replace_all("^can\\b", "Can"))

# Export -----------------------------------------------------------------------
saveRDS(google_df, file.path(gtrends_dir, "FinalData", "gtrends_usa_ref_ivermectin", 
                             "gtrends_usa_ref_ivermectin.Rds"))




