# Lockdown Analysis

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

# 1. Load / Prep Data ----------------------------------------------------------
gtrends_cases_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                      "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_cases_df$geo_name <- gtrends_cases_df$geo %>% countrycode(origin = "iso2c", destination = "iso.name.en")

gtrends_cases_df <- gtrends_cases_df %>%
  mutate(days_since_lockdown_min_fact = factor(days_since_lockdown_min),
         days_since_lockdown_min_fact = days_since_lockdown_min_fact %>% relevel(ref = "-1")) 

# 2.1: Before/After Locdown Average --------------------------------------------
lockdown_ba_df <- gtrends_cases_df %>%
  dplyr::filter(abs(days_since_lockdown_min) <= 31) %>%
  mutate(post_lockdown = (days_since_lockdown_min >= 0)) %>%
  group_by(geo, keyword_en, post_lockdown) %>%
  summarise(hits_ma7 = mean(hits_ma7)) %>% 
  pivot_wider(names_from = post_lockdown,
              values_from = hits_ma7) %>%
  dplyr::rename(pre_lockdown = "FALSE",
                post_lockdown = "TRUE") %>%
  dplyr::mutate(change = post_lockdown - pre_lockdown,
                per_change = ((post_lockdown - pre_lockdown) / pre_lockdown) * 100,
                per_change = case_when(pre_lockdown == post_lockdown ~ 0,
                                       TRUE ~ per_change)) %>%
  ungroup()

lockdown_ba_keyavg <- lockdown_ba_df %>%
  group_by(keyword_en) %>%
  summarise(change     = mean(change, na.rm=T),
            per_change = mean(per_change, na.rm=T))




# Event Study: All Countries ---------------------------------------------------
make_es_figure <- function(keyword, df){
  
  df <- gtrends_cases_df %>%
    filter(keyword_en %in% !!keyword,
           abs(days_since_lockdown_min) <= 31) 
  
  if(nrow(df) > 0){
    
    title <- paste0("Search Term: ",
                    keyword, "; N Countries: ",
                    length(unique(df$geo)))
    
    data_lm <- df %>%
      felm(hits_ma7 ~ days_since_lockdown_min_fact | geo | 0 | 0, data = .) %>%
      lm_post_confint_tidy() %>%
      filter(variable != "(Intercept)") %>%
      mutate(variable = variable %>%
               str_replace_all("days_since_lockdown_min_fact", "") %>%
               as.numeric()) 
    
    p <- data_lm %>%
      ggplot(aes(x = variable,
                 y = b,
                 ymin = p025,
                 ymax = p975)) +
      geom_vline(xintercept = 0,
                 color = "red") +
      geom_point() +
      geom_linerange() +
      labs(x = "Days Since Lockdown",
           y = "Coef +/- 95% CI",
           title = title) +
      theme_minimal()
  } else{
    p <- ggplot()
  }
  
  return(p)
}

for(keyword_i in unique(gtrends_cases_df$keyword_en)){
  p <- make_es_figure(keyword_i, gtrends_cases_df)
  ggsave(p, filename = file.path("~/Desktop/es", paste0(keyword_i, ".png")),
         height = 3.5, width = 5.5)
}

# Example Countries: Boredom ---------------------------------------------------
p <- gtrends_cases_df %>%
  filter(geo_name %in% c("United States of America (the)",
                         "Spain",
                         "India",
                         "Angola",
                         "South Africa",
                         "Peru",
                         "Nigeria",
                         "France",
                         "Ghana",
                         "Kenya",
                         "Colombia",
                         "Uganda",
                         "United Kingdom",
                         "Australia",
                         "Botswana",
                         "New Zealand",
                         "Trinidad and Tobago",
                         "Venezuela (Bolivarian Republic of)",
                         "Dominican Republic (the)",
                         "Zimbabwe")) %>%
  mutate(geo_name = case_when(geo_name %in% "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                              geo_name %in% "Dominican Republic (the)" ~ "Dominican Republic",
                              geo_name %in% "United States of America (the)" ~ "USA",
                              TRUE ~ geo_name)) %>%
  filter(keyword_en %in% c("boredom")) %>%
  filter(abs(days_since_lockdown_min) <= 31) %>%
  ggplot() +
  geom_vline(xintercept = 0,
             color = "red") +
  geom_line(aes(x = days_since_lockdown_min,
                y = hits_ma7)) +
  facet_wrap(~geo_name,
             scales = "free_y") +
  labs(x = "Days Since Lockdown",
       y = "Search Interest") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size =7))
ggsave(p, filename = file.path("~/Desktop/boredom.png"),
       height = 5, width = 7)

# Example Countries: Unemployment ---------------------------------------------------
p <- gtrends_cases_df %>%
  filter(geo_name %in% c("United States of America (the)",
                         "Spain",
                         "Argentina",
                         "Bahamas (the)",
                         "Belize",
                         "Bulgaria",
                         "Canada",
                         "Colombia",
                         "Chile",
                         "Iceland",
                         "Iran (Islamic Republic of)",
                         "Italy",
                         "Luxembourg",
                         "Malta",
                         "North Macedonia",
                         "Poland",
                         "Russian Federation (the)",
                         "Thailand",
                         "United Kingdom",
                         "Uruguay"
  )) %>%
  mutate(geo_name = case_when(geo_name %in% "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                              geo_name %in% "Bahamas (the)" ~ "Bahamas",
                              geo_name %in% "Russian Federation (the)" ~ "Russia",
                              geo_name %in% "Iran (Islamic Republic of)" ~ "Iran",
                              geo_name %in% "Dominican Republic (the)" ~ "Dominican Republic",
                              geo_name %in% "United States of America (the)" ~ "USA",
                              TRUE ~ geo_name)) %>%
  filter(keyword_en %in% c("unemployment")) %>%
  filter(abs(days_since_lockdown_min) <= 31) %>%
  ggplot() +
  geom_vline(xintercept = 0,
             color = "red") +
  geom_line(aes(x = days_since_lockdown_min,
                y = hits_ma7)) +
  facet_wrap(~geo_name,
             scales = "free_y") +
  labs(x = "Days Since Lockdown",
       y = "Search Interest") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size =7))
ggsave(p, filename = file.path("~/Desktop/unemployment.png"),
       height = 5, width = 7)



p <- gtrends_cases_df %>%
  mutate(geo_name = case_when(geo_name %in% "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                              geo_name %in% "Dominican Republic (the)" ~ "Dominican Republic",
                              geo_name %in% "United States of America (the)" ~ "USA",
                              TRUE ~ geo_name)) %>%
  filter(keyword_en %in% c("unemployment")) %>%
  filter(abs(days_since_lockdown_min) <= 31) %>%
  ggplot() +
  geom_vline(xintercept = 0,
             color = "red") +
  geom_line(aes(x = days_since_lockdown_min,
                y = hits_ma7)) +
  facet_wrap(~geo_name,
             scales = "free_y") +
  labs(x = "Days Since Lockdown",
       y = "Search Interest") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size =7))
ggsave(p, filename = file.path("~/Desktop/unemployment_l.png"),
       height = 20, width = 20)
