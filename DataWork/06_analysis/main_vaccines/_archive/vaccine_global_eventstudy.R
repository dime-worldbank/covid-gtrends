# Vaccine Analysis

# TODO: How to deal with some not having much data...
# Show N by continent? 
# Heterogenity by continent!

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", 
                                "gtrends_otherdata_varclean_complete_vaccine.Rds"))

## Remove Keywords
gtrends_df <- gtrends_df %>%
  dplyr::filter(!(keyword_en %in% c("vaccine approval",
                                    "vaccine conspiracy",
                                    "pharmacy",
                                    "vaccine dna",
                                    "vaccine failure",
                                    "vaccine fraud",
                                    "vaccine injuries",
                                    "vaccine mercury",
                                    "vaccine toxins",
                                    "vaccines are poison",
                                    "vaccines kill",
                                    "can i get the vaccine",
                                    "blood clots",
                                    "medical freedom",
                                    "where covid vaccine",
                                    "covid vaccine austism",
                                    "covid vaccine cause autism",
                                    "negative side effects of covid vaccine",
                                    "covid vaccine is poison",
                                    "covid vaccine second dose sick",
                                    "do you get sick after covid vaccine",
                                    "is covid vaccine the mark of the beast",
                                    "can covid vaccine cause infertility",
                                    "where can i get the covid vaccine",
                                    "get covid vaccine near me",
                                    "vaccine aluminum",
                                    "where get covid vaccine near me",
                                    "where to get vaccine covid near me",
                                    "does covid vaccine cause infertility", 
                                    "covid vaccine ineffective",
                                    "covid vaccine unsafe",
                                    "covid vaccine appointment near me",
                                    "sick from covid vaccine",
                                    "does covid vaccine cause infertility")))

# Prep data --------------------------------------------------------------------
# Prep data that will be relevant for all analysis

## If standard deviation of hits is zero, remove
gtrends_df <- gtrends_df %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_sd = sd(hits)) %>%
  ungroup() %>%
  dplyr::filter(hits_sd > 0)

## Limit data
gtrends_es_df <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_first_vaccine_given)) %>%
  dplyr::filter(days_since_first_vaccine_given >= -90,
                days_since_first_vaccine_given <= 90) %>%
  dplyr::mutate(days_since_first_vaccine_given = days_since_first_vaccine_given %>% as.numeric) %>%
  dplyr::mutate(post_vaccine = as.numeric(days_since_first_vaccine_given > 0)) %>%
  dplyr::mutate(hits_ma7_log = log(hits_ma7 + 1)) %>%
  
  # Interaction variables
  dplyr::mutate(total_vaccinations_per_hundred_max_X_post_vaccine = post_vaccine*max(total_vaccinations_per_hundred, na.rm=T),
                total_vaccinations_per_hundred_max = max(total_vaccinations_per_hundred, na.rm=T)) %>%
  dplyr::mutate(keyword_en = keyword_en %>% 
                  tools::toTitleCase() %>%
                  str_replace_all("^can", "Can") %>%
                  str_replace_all("^where", "Where") %>%
                  str_replace_all("\\bi\\b", "I")) 

## Scaled between 0-100  - Average
gtrends_es_scaled_avg_df <- gtrends_es_df %>%
  group_by(keyword_en, geo) %>%
  #dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
  #              hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
  #ungroup() %>%
  #dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
  dplyr::mutate(hits_ma7_std = range01(hits_ma7)*100) %>%
  dplyr::group_by(keyword_en, keyword_cat, days_since_first_vaccine_given) %>%
  dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
  ungroup() %>%
  #dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase() %>% paste0("\nN Countries = ", N_countries_keyword))
  dplyr::mutate(keyword_en = case_when(
    keyword_en == "Is the Covid Vaccine the Mark of the Beast" ~ "Is the Covid Vaccine\nthe Mark of the Beast",
    keyword_en == "" ~ "",
    TRUE ~ keyword_en
  )) #%>%
  #dplyr::mutate(hits_ma7_std = range01(hits_ma7)*100) %>%
  
#gtrends_es_scaled_avg_df$keyword_en %>% unique()

# Figure: Trends ---------------------------------------------------------------

## Order Keyword / Factor
keyword_order <- gtrends_es_scaled_avg_df %>%
  arrange(keyword_cat) %>%
  pull(keyword_en) %>%
  unique()

gtrends_es_scaled_avg_df$keyword_en <- gtrends_es_scaled_avg_df$keyword_en %>%
  factor(levels = keyword_order)

p_es_trends <- gtrends_es_scaled_avg_df %>%
  ggplot() +
  geom_line(aes(x = days_since_first_vaccine_given,
                y = hits_ma7_std,
                color = keyword_cat)) +
  geom_vline(aes(xintercept = 0)) +
  facet_wrap(~keyword_en) + # , scales = "free_y"
  labs(color = "Category",
       x = "Days Since First Vaccine Given",
       y = "Average Search Interest",
       title = "A. Trends in Search Interest") + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "top",
        plot.title = element_text(face = "bold")) 

# Event Study Coefficients -----------------------------------------------------
gtrends_es_df <- gtrends_es_df %>%
  mutate(days_since_first_vaccine_given = days_since_first_vaccine_given %>%
           as.factor() %>%
           relevel("-1"))

coefs_es_df <- map_df(unique(gtrends_es_df$keyword_en), function(keyword_en_i){
  print(keyword_en_i)
  
  df_out <- felm(hits_ma7_log ~ days_since_first_vaccine_given | geo, data = gtrends_es_df %>%
                   dplyr::filter(keyword_en %in% keyword_en_i)) %>%
    lm_post_confint_tidy() %>%
    dplyr::mutate(keyword_en = keyword_en_i)
  
  df_out$keyword_cat <- gtrends_es_df$keyword_cat[gtrends_es_df$keyword_en %in% keyword_en_i][1]
  
  return(df_out)
})

coefs_es_df <- coefs_es_df %>%
  mutate(keyword_en = keyword_en %>%
           as.character() %>%
           tools::toTitleCase() %>%
           str_replace_all("^can", "Can") %>%
           str_replace_all("\\bi\\b", "I")) %>%
  mutate(keyword_en = fct_reorder(keyword_en, b),
         days_since_first_vax = variable %>% 
           str_replace_all("days_since_first_vaccine_given", "") %>%
           as.character() %>%
           as.numeric())

p_es_coef_simple <- coefs_es_df %>%
  ggplot(aes(y = b,
             ymin = p025,
             ymax = p975,
             x = days_since_first_vax,
             color = keyword_cat)) +
  geom_vline(xintercept = 0,
             alpha = 0.5) +
  geom_hline(yintercept = 0,
             alpha = 0.5) +
  geom_point(size = 0.4) +
  geom_linerange(size = 0.1) +
  labs(x = "Coefficient (+/-95% CI)",
       color = "Category",
       y = NULL,
       title = "B. Event Study Results: Impact of Vaccine Availability on Search Interest") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "bold", color = "black"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  #guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  facet_wrap(~keyword_en, scales = "free_y")

ggsave(p_es_coef_simple, 
       filename = "~/Desktop/es.png",
       height = 10, width = 10)

# Figure: Coefficients - Simple ------------------------------------------------
coefs_df <- map_df(unique(gtrends_es_df$keyword_en), function(keyword_en_i){
  print(keyword_en_i)
  
  df_out <- felm(hits_ma7_log ~ post_vaccine | geo, data = gtrends_es_df %>%
                   dplyr::filter(keyword_en %in% keyword_en_i)) %>%
    lm_post_confint_tidy() %>%
    dplyr::mutate(keyword_en = keyword_en_i)
  
  df_out$keyword_cat <- gtrends_es_df$keyword_cat[gtrends_es_df$keyword_en %in% keyword_en_i][1]
  
  return(df_out)
})

coefs_df <- coefs_df %>%
  mutate(keyword_en = keyword_en %>%
           as.character() %>%
           tools::toTitleCase() %>%
           str_replace_all("^can", "Can") %>%
           str_replace_all("\\bi\\b", "I")) %>%
  mutate(keyword_en = fct_reorder(keyword_en, b))

p_es_coef_simple <- coefs_df %>%
  ggplot(aes(x = b,
             xmin = p025,
             xmax = p975,
             y = keyword_en,
             color = keyword_cat)) +
  geom_vline(xintercept = 0,
             alpha = 0.5) +
  geom_point() +
  geom_linerange() +
  labs(x = "Coefficient (+/-95% CI)",
       color = "Category",
       y = NULL,
       title = "B. Event Study Results: Impact of Vaccine Availability on Search Interest") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "bold", color = "black"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

# Figure: Coefficients - Interaction -------------------------------------------
# coefs_inter_df <- map_df(unique(gtrends_es_df$keyword_en), function(keyword_en_i){
#   print(keyword_en_i)
#   
#   df_out <- felm(hits_ma7_log ~ post_vaccine + total_vaccinations_per_hundred_max_X_post_vaccine, data = gtrends_es_df %>%
#                    dplyr::filter(keyword_en %in% keyword_en_i)) %>%
#     lm_post_confint_tidy() %>%
#     dplyr::mutate(keyword_en = keyword_en_i)
#   
#   df_out$keyword_cat <- gtrends_es_df$keyword_cat[gtrends_es_df$keyword_en %in% keyword_en_i][1]
#   
#   return(df_out)
# })
# 
# coefs_inter_df <- coefs_inter_df %>%
#   dplyr::filter(variable != "(Intercept)") %>%
#   #  dplyr::select(variable == c("post_vaccine",
#   #                              "post_vaccine:total_vaccinations_per_hundred_max")) %>%
#   mutate(keyword_en = fct_reorder(keyword_en, b))
# 
# p_es_coef_inter <- coefs_inter_df %>%
#   ggplot(aes(x = b,
#              xmin = p025,
#              xmax = p975,
#              y = keyword_en,
#              color = variable)) +
#   geom_point() +
#   geom_linerange()

# Figures: Arrange -------------------------------------------------------------
p_es <- ggarrange(p_es_trends,
                  p_es_coef_simple,
                  common.legend = T,
                  legend = "top",
                  widths = c(0.55, 0.45)) 

ggsave(p_es, filename = file.path(paper_figures, "vax_eventstudy.png"), height = 8, width = 15)





