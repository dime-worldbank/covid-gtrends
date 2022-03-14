# Vaccine figures

# https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md#vaccination-policies

# v1: Reports the existence of a prioritised plan for vaccine rollout

# "v1_vaccine"                                                  
# "v2_vaccine"                                                  
# "v3_vaccine"                                                  
# "days_since_v1_vaccine_1"                                     
# "days_since_v1_vaccine_2"                                     
# "days_since_v2_vaccine_1"                                     
# "days_since_v2_vaccine_2"                                     
# "days_since_v2_vaccine_3"  

# v1_vaccine_1: prioritization plan in place
# v1_vaccine_2: universal eligibility; no prioritization

# v2_vaccine_1: vaccines are available to some categories
# v2_vaccine_2: vaccines are available to anyone over the age of 16 yrs
# v2_vaccine_3: vaccines are available to anyone over the age of 16 yrs PLUS one or both of 5-15 yrs and 0-4 yrs

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", 
                                "gtrends_otherdata_varclean_complete_vaccine.Rds"))

# Limit data and scale between 0-100 -------------------------------------------
gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_first_vaccine_given))

gtrends_sum_df$hits_ma7 <- gtrends_sum_df$hits

gtrends_sum_df <- gtrends_sum_df %>%
  dplyr::filter(keyword_en %in% VACCINE_KEYWORDS,
                days_since_first_vaccine_given >= -60,
                days_since_first_vaccine_given <= 60) %>%
  dplyr::select(geo, keyword_en, days_since_first_vaccine_given, hits_ma7) %>%
  dplyr::mutate(days_since_first_vaccine_given = days_since_first_vaccine_given %>% as.numeric) %>%
  tidyr::complete(geo = unique(gtrends_sum_df$geo), 
                  keyword_en = VACCINE_KEYWORDS, 
                  days_since_first_vaccine_given = -60:60, 
                  fill = list(hits_ma7 = 0)) 

## Delete if no hits 
gtrends_sum_df <- gtrends_sum_df %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(hits_ma7_geoSUM = sum(hits_ma7)) %>%
  ungroup() %>%
  dplyr::filter(hits_ma7_geoSUM > 0)

gtrends_sum_df <- gtrends_sum_df %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::group_by(keyword_en, days_since_first_vaccine_given) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  #dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase() %>% paste0("\nN Countries = ", N_countries_keyword))
  dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase())

# Figures ----------------------------------------------------------------------
p <- gtrends_sum_df %>%
  ggplot() +
  geom_line(aes(x = days_since_first_vaccine_given,
                y = hits_ma7_std)) +
  geom_vline(aes(xintercept = 0)) +
  facet_wrap(~keyword_en, scales = "free_y")

ggsave(p, filename = file.path("~/Desktop/since_vac.png"),
       height = 12, width = 12)

# Regressions: Vaccinations vs Interest ----------------------------------------
p <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "covid vaccine side effects",
                #geo %in% c("US", "UK"),
               date >= ymd("2021-01-01")) %>%
  group_by(country) %>%
  mutate(hits_std = sd(hits, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(hits_std > 0) %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line() +
  facet_wrap(~country, scales = "free_y")
ggsave(p, filename = "~/Desktop/trends.png", height = 15, width = 15)


gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(abs(days_since_first_vaccine_given) <= 100)

gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(abs(days_since_first_vaccine_given) <= 100)

cor_df <- gtrends_sum_df %>%
  dplyr::filter(!is.na(total_vaccinations_per_hundred),
                !is.na(hits)) %>%
  group_by(geo, keyword_en) %>%
  dplyr::summarise(cor = cor(total_vaccinations_per_hundred, hits))

cor_df %>%
  ggplot() +
  geom_histogram(aes(x = cor)) +
  facet_wrap(~keyword_en)

# Regressions: Interest Post Vaccine -------------------------------------------
gtrends_df$days_since_first_vaccine_given <- difftime(gtrends_df$date, ymd("2021-03-15"), units = "days")

gtrends_sum_df <- gtrends_df %>%
  dplyr::filter(keyword_en %in% VACCINE_KEYWORDS,
                days_since_first_vaccine_given >= -15,
                days_since_first_vaccine_given <= 15) 

gtrends_sum_df <- gtrends_sum_df %>%
  dplyr::mutate(post_vaccine = as.numeric(days_since_first_vaccine_given >= 0)) %>%
  dplyr::mutate(hits_log = log(hits + 1))

coefs_df <- map_df(unique(gtrends_sum_df$keyword_en), function(keyword_en_i){
  print(keyword_en_i)
  
  felm(hits_log ~ post_vaccine | geo, data = gtrends_sum_df %>%
         dplyr::filter(keyword_en %in% keyword_en_i)) %>%
    lm_post_confint_tidy() %>%
    dplyr::mutate(keyword_en = keyword_en_i)
  
})

coefs_df <- coefs_df %>%
  mutate(keyword_en = fct_reorder(keyword_en, b))

coefs_df %>%
  ggplot(aes(x = b,
             xmin = p025,
             xmax = p975,
             y = keyword_en)) +
  geom_point() +
  geom_linerange()

# Trends: Examples -------------------------------------------------------------
gtrends_sum_df <- gtrends_df %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(hits_ma7_geoSUM = sum(hits)) %>%
  ungroup() %>%
  dplyr::filter(hits_ma7_geoSUM > 0)

gtrends_sum_df <- gtrends_sum_df %>%
  dplyr::filter(abs(days_since_first_vaccine_given) <= 60)

gtrends_sum_df <- gtrends_sum_df %>%
  

head(gtrends_sum_df)


for(i in unique(gtrends_sum_df$keyword_en)){
  p <- gtrends_sum_df %>%
    dplyr::filter(keyword_en %in% i) %>%
    ggplot() +
    geom_line(aes(x = days_since_first_vaccine_given,
                  y = hits)) + 
    facet_wrap(~country)
  
  ggsave(p, filename = file.path("~/Desktop", "tmp",
                                 paste0(i, ".png")),
         height = 12, width = 12)
}

gtrends_sum_df %>%
  dplyr::filter(keyword_en %in% "covid vaccine change dna") %>%
  ggplot() +
  geom_line(aes(x = days_since_first_vaccine_given,
                y = hits)) + 
  facet_wrap(~country)

gtrends_sum_df %>%
  dplyr::filter(keyword_en %in% "covid vaccine change dna") %>%
  ggplot() +
  geom_line(aes(x = days_since_first_vaccine_given,
                y = hits)) + 
  facet_wrap(~country)



##
for(word_i in unique(gtrends_df$keyword_en)){
  p <- gtrends_df %>%
    dplyr::filter(keyword_en %in% word_i) %>%
    dplyr::filter(abs(days_since_v1_vaccine_1) <= THRESH) %>%
    ggplot(aes(x = days_since_v1_vaccine_1,
               y = hits_ma7)) +
    geom_line() +
    geom_vline(xintercept = 0,
               color = "red") +
    facet_wrap(~country,
               scales = "free_y")
  
  ggsave(p, filename = file.path("~/Desktop/v1_vaccine_1",
                                 paste0(word_i, ".png")))
  
}

##
for(word_i in unique(gtrends_df$keyword_en)){
  p <- gtrends_df %>%
    dplyr::filter(keyword_en %in% word_i) %>%
    dplyr::filter(abs(days_since_v1_vaccine_2) <= THRESH) %>%
    ggplot(aes(x = days_since_v1_vaccine_2,
               y = hits_ma7)) +
    geom_line() +
    geom_vline(xintercept = 0,
               color = "red") +
    facet_wrap(~country,
               scales = "free_y")
  
  ggsave(p, filename = file.path("~/Desktop/v1_vaccine_2",
                                 paste0(word_i, ".png")))
  
}

##
for(word_i in unique(gtrends_df$keyword_en)){
  p <- gtrends_df %>%
    dplyr::filter(keyword_en %in% word_i) %>%
    dplyr::filter(abs(days_since_v2_vaccine_1) <= THRESH) %>%
    ggplot(aes(x = days_since_v2_vaccine_1,
               y = hits_ma7)) +
    geom_line() +
    geom_vline(xintercept = 0,
               color = "red") +
    facet_wrap(~country,
               scales = "free_y")
  
  ggsave(p, filename = file.path("~/Desktop/v2_vaccine_1",
                                 paste0(word_i, ".png")))
  
}

##
for(word_i in unique(gtrends_df$keyword_en)){
  p <- gtrends_df %>%
    dplyr::filter(keyword_en %in% word_i) %>%
    dplyr::filter(abs(days_since_v2_vaccine_2) <= THRESH) %>%
    ggplot(aes(x = days_since_v2_vaccine_2,
               y = hits_ma7)) +
    geom_line() +
    geom_vline(xintercept = 0,
               color = "red") +
    facet_wrap(~country,
               scales = "free_y")
  
  ggsave(p, filename = file.path("~/Desktop/v2_vaccine_2",
                                 paste0(word_i, ".png")))
  
}

##
for(word_i in unique(gtrends_df$keyword_en)){
  p <- gtrends_df %>%
    dplyr::filter(keyword_en %in% word_i) %>%
    dplyr::filter(abs(days_since_v2_vaccine_3) <= THRESH) %>%
    ggplot(aes(x = days_since_v2_vaccine_3,
               y = hits_ma7)) +
    geom_line() +
    geom_vline(xintercept = 0,
               color = "red") +
    facet_wrap(~country,
               scales = "free_y")
  
  ggsave(p, filename = file.path("~/Desktop/v2_vaccine_3",
                                 paste0(word_i, ".png")))
  
}



