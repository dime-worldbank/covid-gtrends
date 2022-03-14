# Vaccine Analysis

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
        axis.text.y = element_text(face = "bold", color = "black")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

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
           fill = "#ff9191", # red
           color = "#ff9191") +
  geom_line(aes(x = date,
                y = hits_ma7_std),
            color = "#3AA959",
            size = 0.3) + # green
  labs(x = "",
       y = "",
       title ="<span style='font-size:12pt'><span style='color:#000000;'>B. Trends in Google Search Interest in</span>
             <br><span style='color:#3AA959;'>'COVID Vaccine Appointment'</span>
             <span style='color:#000000;'>and</span>
  <span style='color:#ff9191;'>Daily Vaccinations</span>
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

# Correlation Figures: Arrange -------------------------------------------------
p_cor <- ggarrange(p_cor_coef,
                   p_cor_trends,
                   ncol=2)

ggsave(p_cor, filename = file.path(paper_figures, "vax_cor.png"), height = 8, width = 13)
