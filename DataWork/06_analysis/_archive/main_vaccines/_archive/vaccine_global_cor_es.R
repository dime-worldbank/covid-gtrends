# Vaccine Global Analysis

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

## Vaccinations First Difference
gtrends_df <- gtrends_df %>%
  arrange(date) %>%
  group_by(keyword_en, geo, keyword_cat) %>%
  dplyr::mutate(total_vaccinations_per_hundred_diff = c(NA, diff(total_vaccinations_per_hundred))) %>%
  ungroup() 

# CORRELATION ==================================================================

## Aggregate by Correlation
gtrends_vaccor_df <- gtrends_df %>%
  dplyr::filter(!is.na(total_vaccinations_per_hundred),
                !is.na(total_vaccinations_per_hundred_diff),
                !is.na(hits_ma7),
                total_vaccinations_per_hundred > 0) %>%
  dplyr::filter(date_first_vaccine_given <= ymd("2021-06-01")) %>%
  group_by(keyword_en, geo, keyword_cat) %>%
  dplyr::summarise(cor = cor(hits_ma7, total_vaccinations_per_hundred),
                   cor_diff = cor(hits_ma7, total_vaccinations_per_hundred_diff),
                   total_vaccinations_per_hundred_max = max(total_vaccinations_per_hundred, na.rm = T),
                   N_vax_data = sum(!is.na(total_vaccinations_per_hundred))) %>%
  dplyr::filter(!is.na(cor_diff),
                
                # Must have at least 10 observations
                N_vax_data >= 10) %>%
  dplyr::mutate(keyword_en_cap = keyword_en %>% 
                  tools::toTitleCase() %>%
                  str_replace_all("^can", "Can") %>%
                  str_replace_all("^where", "Where") %>%
                  str_replace_all("\\bi\\b", "I"))

## Add correlation to main data (not aggregated)
gtrends_vaccor_df_tomerge <- gtrends_vaccor_df %>%
  dplyr::select(keyword_en, geo, cor, cor_diff)

gtrends_df <- gtrends_df %>%
  left_join(gtrends_vaccor_df_tomerge, by = c("keyword_en", "geo"))

# Correlation: Figure ----------------------------------------------------------
p_cor_coef <- gtrends_vaccor_df %>%
  ggplot(aes(x = cor_diff, 
             y = reorder(keyword_en_cap, cor_diff, FUN = median),
             fill = keyword_cat)) +
  geom_vline(aes(xintercept = 0)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width=0.3,
                                             dodge.width = 0.85),
             pch = 21,
             size = 0.9, # 0.7
             stroke = 0.2, # 0.1
             color = "black") +
  labs(fill = "Category",
       x = "Correlation",
       y = "",
       title = "A. Within Country Correlation of Daily\nVaccination Rates and Search Interest") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        plot.title.position = "plot",
        axis.text.y = element_text(face = "bold", color = "black")) #+
#guides(fill=guide_legend(nrow=2,byrow=TRUE))

# Trends Figure ----------------------------------------------------------------
## Standardize

gtrends_df <- gtrends_df %>%
  group_by(keyword_en, geo, keyword_cat) %>%
  dplyr::mutate(total_vaccinations_per_hundred_diff_std = range01(total_vaccinations_per_hundred_diff, na.rm=T),
                hits_ma7_std = range01(hits_ma7, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(title = paste0(country, "\nCorrelation: ",
                               round(cor_diff, 2))) %>%
  dplyr::mutate(title = fct_reorder(title,
                                    cor_diff,
                                    .desc = T))

p_cor_trends <- gtrends_df %>%
  dplyr::filter(cor_diff >= 0.2) %>%
  dplyr::filter(keyword_en %in% "covid vaccine appointment") %>%
  ggplot() +
  geom_col(aes(x = date,
               y = total_vaccinations_per_hundred_diff_std),
           fill = "#ff9900", # red
           color = "#ff9900") +
  geom_line(aes(x = date,
                y = hits_ma7_std),
            color = "#3AA959",
            size = 0.3) + # green
  labs(x = "",
       y = "",
       title ="<span style='font-size:12pt'><span style='color:#000000;'>B. Trends in Google Search Interest in</span>
             <br><span style='color:#3AA959;'>'COVID Vaccine Appointment'</span>
             <span style='color:#000000;'>and</span>
  <span style='color:#ff9900;'>Daily Vaccinations</span>
  <br>
  </span>") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=10),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0, face = "bold",size=5)) +
  facet_wrap(~title) 

# EVENT STUDY ==================================================================

# Event Study: Prep Data -------------------------------------------------------

## Limit data
gtrends_es_df <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_first_vaccine_given)) %>%
  dplyr::filter(days_since_first_vaccine_given >= -90,
                days_since_first_vaccine_given <= 90) %>%
  dplyr::mutate(days_since_first_vaccine_given = days_since_first_vaccine_given %>% as.numeric) %>%
  dplyr::mutate(post_vaccine = as.numeric(days_since_first_vaccine_given > 0)) %>%
  dplyr::mutate(hits_ma7_log = log(hits_ma7 + 1)) %>%
  dplyr::mutate(keyword_en = keyword_en %>% 
                  tools::toTitleCase() %>%
                  str_replace_all("^can", "Can") %>%
                  str_replace_all("^where", "Where") %>%
                  str_replace_all("\\bi\\b", "I")) %>%
  mutate(days_since_first_vaccine_given = days_since_first_vaccine_given %>%
           as.factor() %>%
           relevel("-1"))

# Event Study Coefficients -----------------------------------------------------
coefs_es_df <- map_df(unique(gtrends_es_df$keyword_en), function(keyword_en_i){
  print(keyword_en_i)
  
  df_out <- felm(hits_ma7_log ~ days_since_first_vaccine_given | geo, data = gtrends_es_df %>%
                   dplyr::filter(keyword_en %in% keyword_en_i)) %>%
    lm_post_confint_tidy() %>%
    dplyr::mutate(keyword_en = keyword_en_i)
  
  df_out$keyword_cat <- gtrends_es_df$keyword_cat[gtrends_es_df$keyword_en %in% keyword_en_i][1]
  
  return(df_out)
})

## Cleanup Coefficient Data
coefs_es_df <- coefs_es_df %>%
  mutate(days_since_first_vax = variable %>% 
           str_replace_all("days_since_first_vaccine_given", "") %>%
           as.character() %>%
           as.numeric()) %>%
  dplyr::mutate(keyword_en = keyword_en %>% as.character(),
                keyword_en = case_when(
                  keyword_en == "Is the Covid Vaccine the Mark of the Beast" ~ "Is the Covid Vaccine the\nMark of the Beast",
                  keyword_en == "Long Term Effects of Covid Vaccine" ~ "Long Term Effects\nof Covid Vaccine",
                  keyword_en == "Covid Vaccine Cause Infertility" ~ "Covid Vaccine\nCause Infertility",
                  keyword_en == "Does Covid Vaccine Change Dna" ~ "Does Covid\nVaccine Change Dna",
                  keyword_en == "Covid Vaccine Priority List" ~ "Covid Vaccine\nPriority List",
                  keyword_en == "Can I Get the Covid Vaccine" ~ "Can I Get the\nCovid Vaccine",
                  keyword_en == "Covid Vaccine Side Effects" ~ "Covid Vaccine\nSide Effects",
                  keyword_en == "Is Covid Vaccine Approved" ~ "Is Covid Vaccine\nApproved",
                  keyword_en == "Covid Vaccine Second Dose" ~ "Covid Vaccine\nSecond Dose",
                  keyword_en == "Covid Vaccine Infertility" ~ "Covid Vaccine\nInfertility",
                  keyword_en == "Covid Vaccine Blood Clots" ~ "Covid Vaccine\nBlood Clots",
                  keyword_en == "Covid Vaccine Appointment" ~ "Covid Vaccine\nAppointment",
                  keyword_en == "Covid Vaccine Change Dna" ~ "Covid Vaccine\nChange Dna",
                  keyword_en == "Safety of Covid Vaccine" ~ "Safety of\nCovid Vaccine",
                  #keyword_en == "" ~ "",
                  TRUE ~ keyword_en
                ))

#words <- as.character(coefs_es_df$keyword_en) %>% unique()
#words[order(nchar(words))]

keyword_order <- coefs_es_df %>%
  arrange(keyword_cat) %>%
  pull(keyword_en) %>%
  unique() %>%
  as.character()

coefs_es_df <- coefs_es_df %>%
  dplyr::mutate(keyword_en = keyword_en %>%
                  factor(levels = keyword_order))

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
       title = "C. Event Study: Impact of Vaccine Availability on Search Interest") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        #axis.text.y = element_text(face = "bold", color = "black"),
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot") +
  #guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  facet_wrap(~keyword_en, scales = "free_y")

# ARRANGE FIGURES ==============================================================
p_cor <- ggarrange(p_cor_coef,
                   p_cor_trends,
                   ncol=2,
                   common.legend = T,
                   legend = "top")

p <- ggarrange(p_cor,
               p_es_coef_simple,
               ncol = 1,
               heights = c(0.48, 0.52))

ggsave(p, filename = file.path(paper_figures, "vax_cor_es.png"), height = 15, width = 12)



