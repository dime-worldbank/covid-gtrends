# Make clean dataset of languages

# Load/Append data -------------------------------------------------------------
df <- file.path(prim_lang_dir, 
                "FinalData", 
                "gtrends_data") %>%
  list.files(pattern = ".Rds",
             full.names = T) %>%
  map_df(readRDS)

languages_df <- readRDS(file.path(data_dir, 
                                  "country_primary_language", "FinalData",
                                  "countries_modified_clean.Rds"))

df <- df[df$geo %in% languages_df$geo,]

# General Cleaning -------------------------------------------------------------
df$hits[df$hits %in% "<1"] <- "0.5"
df$hits <- as.numeric(df$hits)

df$language <- df$language %>% str_replace_all("keyword_", "")

# Split Dataframe by Type ------------------------------------------------------
df_nohits <- df[df$gtrends_nohits %in% T,]
df_0_lng <- df[df$n_languages %in% 0,]
df_1_lng <- df[df$n_languages %in% 1,]
df_2m_lng <- df[df$n_languages %in% 2:10,]

# Sometime no search interest in any language for one of the keywords
table(df_2m_lng$geo %in% df_nohits$geo)

# Dataframe: If 2/more languages, best language --------------------------------

##### (1) In some cases, word is same in multiple languages; split these out
df_2m_lng <- df_2m_lng %>%
  mutate(group_id = paste(language, geo, keyword_en))

df_2m_lng <- map_df(unique(df_2m_lng$group_id), function(group_id_i){
  
  df_2m_lng_i <- df_2m_lng[df_2m_lng$group_id %in% group_id_i,]
  languages_str <- df_2m_lng_i$language[1]
  
  # If more than one language, repeat the dataframe across languages
  if(languages_str %>% str_detect(";")){
    
    languages_vec <- languages_str %>% 
      str_split(";") %>%
      unlist()
    
    df_out <- map_df(languages_vec, function(language_vec_i){
      df_2m_lng_i$language <- language_vec_i
      return(df_2m_lng_i)
    })
    
  } else{
    df_out <- df_2m_lng_i
  }
  
  return(df_out)
})

##### (2) Averages
df_2m_lng_sum <- df_2m_lng %>%
  group_by(language, geo, keyword_en) %>%
  dplyr::summarise(hits_mean = mean(hits)) %>%
  group_by(geo, keyword_en) %>%
  mutate(language_i = paste0("id_", 1:n())) %>%
  ungroup() %>%
  arrange(geo)

##### (3) Pivot --> Wider
df_2m_wide <- df_2m_lng_sum %>%
  pivot_wider(id_cols = c(geo, keyword_en),
              names_from = language_i,
              values_from = c(hits_mean, language))

##### (4) Standardize translations so max is 100
df_2m_wide$max_hits <- df_2m_wide %>%
  dplyr::select(contains("hits_mean")) %>%
  apply(1, max, na.rm = T) 

df_2m_wide <- df_2m_wide %>%
  mutate_at(vars(contains("hits_mean")),  ~ . / max_hits * 100)

##### (5) Average across search terms
lng_df <- df_2m_wide %>%
  distinct(geo, .keep_all = T) %>%
  dplyr::select(geo, contains("language_id_"))

df_2m_best <- df_2m_wide %>%
  group_by(geo) %>%
  summarise_at(vars(contains("hits_mean")), mean) %>%
  left_join(lng_df, by = "geo")

##### (6) Choose best language
df_2m_best$max_id <- df_2m_best %>%
  dplyr::select(contains("hits_mean")) %>%
  apply(1, which.max)

df_2m_best$language_best <- NA

for(i in unique(df_2m_best$max_id)){
  df_2m_best$language_best[df_2m_best[[paste0("max_id")]] %in% i] <-
    df_2m_best[[paste0("language_id_", i)]][df_2m_best[[paste0("max_id")]] %in% i]
}

##### (7) Value for best and second best score
# Helps to evaluate if there's a tie

max_second <- function(x){
  # Second largest value
  
  x <- x[!is.na(x)]
  x <- sort(x, decreasing = T)
  
  return(x[2])
}

df_2m_best$largest_value <- df_2m_best %>%
  dplyr::select(contains("hits_mean")) %>%
  apply(1, max, na.rm = T)

df_2m_best$second_largest_value <- df_2m_best %>%
  dplyr::select(contains("hits_mean")) %>%
  apply(1, max_second)

df_2m_best$tie <- df_2m_best$largest_value == df_2m_best$second_largest_value
df_2m_best$n_language <- ">1"

# Append (1) 1 langage and (2) 2/more language datsets -------------------------
df_1_lng <- df_1_lng %>%
  dplyr::select(geo, language) %>%
  dplyr::rename(language_best = language) %>%
  dplyr::mutate(language_id_1 = language_best) %>%
  mutate(n_language = "1") %>%
  
  # Distinct b/c have same dataframe across search terms
  distinct(geo, .keep_all = T)

df_best <- bind_rows(
  df_1_lng,
  df_2m_best
)

# Cleanup ----------------------------------------------------------------------
df_best <- df_best %>%
  dplyr::select(geo, language_best, n_language,
                contains("language_id_"),
                contains("hits_mean_id_")) %>%
  arrange(geo)

# Export -----------------------------------------------------------------------
saveRDS(df_best, file.path(prim_lang_dir, 
                           "FinalData",
                           "country_language.Rds"))

