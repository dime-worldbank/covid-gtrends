# Vaccine Analysis

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", 
                                "gtrends_otherdata_varclean_complete_vaccine.Rds"))

# PREP DATA: RELEVANT FOR ALL ANALYSIS =========================================
# Prep data that will be relevant for all analysis

## If standard deviation of hits is zero, remove
gtrends_df <- gtrends_df %>%
  group_by(keyword_en, geo) %>%
  dplyr::mutate(hits_sd = sd(hits)) %>%
  ungroup() %>%
  dplyr::filter(hits_sd > 0)

# CORRELATION ==================================================================

# Correlation: Prep Data -------------------------------------------------------

gtrends_df$total_vaccinations_per_hundred[gtrends_df$total_vaccinations_per_hundred %in% 0] <- NA

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
                   N_vax_data = sum(!is.na(total_vaccinations_per_hundred)),
                   gdp_pc = mean(gdp_pc, na.rm = T)) %>%
  dplyr::filter(!is.na(cor_diff),
                
                # Must have at least 10 observations
                N_vax_data >= 10) %>%
  dplyr::mutate(keyword_en_cap = keyword_en %>% 
                  tools::toTitleCase() %>%
                  str_replace_all("^can", "Can") %>%
                  str_replace_all("\\bDna\\b", "DNA") %>%
                  str_replace_all("\\bi\\b", "I"))

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
        axis.text.y = element_text(face = "bold", color = "black")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

# Correlation: Figure - By Income Group ----------------------------------------
## Determine above/below median GDP per capita

geo_distinct <- gtrends_vaccor_df %>%
  distinct(geo, .keep_all = T)
med_gdp <- geo_distinct$gdp_pc %>% median(na.rm = T)

gtrends_vaccor_df$gdp_group <- ifelse(gtrends_vaccor_df$gdp_pc > med_gdp, 
                                      "Above Median GDP (per Capita)", 
                                      "Below Median GDP (per Capita)") %>%
  fct_rev()

p_cor_coef_by_gdp <- gtrends_vaccor_df %>%
  dplyr::filter(!is.na(gdp_group)) %>%
  ggplot(aes(x = cor_diff, 
             y = reorder(keyword_en_cap, cor_diff, FUN = median),
             fill = gdp_group)) +
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
       title = "Within Country Correlation of Daily Vaccination Rates and Search Interest",
       subtitle = "Separating Results by Median per Capita GDP") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.y = element_text(face = "bold", color = "black"),
        strip.text = element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE,reverse = TRUE)) +
  scale_fill_manual(values = c("orange",
                               "dodgerblue")) +
  facet_wrap(~keyword_cat,
             scales = "free_y") 
#                     guide = guide_legend(reverse = TRUE)
ggsave(p_cor_coef_by_gdp, 
       filename = file.path(paper_figures, "vac_cor_by_gdp.png"),
       height = 8, width = 10)

# Trends Figure ----------------------------------------------------------------
## Standardize
# https://stackoverflow.com/questions/5665599/range-standardization-0-to-1-in-r
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

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

# 
countries_to_use <- gtrends_df %>%
  dplyr::filter(date >= ymd("2020-12-01")) %>%
  dplyr::filter(keyword_en %in% "vaccine appointment") %>%
  arrange(-cor_diff) %>%
  distinct(country) %>%
  head(9) %>%
  pull(country)

p_cor_trends_vaxappt <- gtrends_df %>%
  dplyr::filter(date >= ymd("2020-12-01")) %>%
  #dplyr::filter(cor_diff >= 0.34) %>%
  dplyr::filter(keyword_en %in% "vaccine appointment") %>%
  dplyr::filter(country %in% all_of(countries_to_use)) %>%
  ggplot() +
  geom_col(aes(x = date,
               y = total_vaccinations_per_hundred_diff_std),
           fill = "#ffc266", # orange
           color = "#ffc266") +
  geom_line(aes(x = date,
                y = hits_ma7_std),
            color = "#3AA959",
            size = 0.3) + # green
labs(x = "",
     y = "",
     title ="<span style='font-size:12pt'><span style='color:#000000;'>B. Trends in Google Search Interest in</span>
             <br><span style='color:#3AA959;'>'Vaccine Appointment'</span>
             <span style='color:#000000;'>and</span>
  <span style='color:#ff9900;'>Daily Vaccinations</span>
  <br>
  </span>") +
theme_minimal() +
theme(axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(face = "bold", color = "black", size=8),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      plot.title = element_markdown(lineheight = 1.1, hjust = 0, face = "bold",size=5)) +
  facet_wrap(~title, nrow = 3) 


countries_to_use <- gtrends_df %>%
  dplyr::filter(date >= ymd("2020-12-01")) %>%
  dplyr::filter(keyword_en %in% "covid vaccine sick") %>%
  arrange(-cor_diff) %>%
  distinct(country) %>%
  head(9) %>%
  pull(country)

p_cor_trends_sideeffects <- gtrends_df %>%
  dplyr::filter(date >= ymd("2020-12-01")) %>%
  #dplyr::filter(cor_diff >= 0.34) %>%
  dplyr::filter(keyword_en %in% "covid vaccine sick") %>%
  dplyr::filter(country %in% all_of(countries_to_use)) %>%
  ggplot() +
  geom_col(aes(x = date,
               y = total_vaccinations_per_hundred_diff_std),
           fill = "#ffc266", # orange
           color = "#ffc266") +
  geom_line(aes(x = date,
                y = hits_ma7_std),
            color = "#3AA959",
            size = 0.3) + # green
  labs(x = "",
       y = "",
       title ="<span style='font-size:12pt'><span style='color:#000000;'>C. Trends in Google Search Interest in</span>
             <br><span style='color:#3AA959;'>'COVID Vaccine Sick'</span>
             <span style='color:#000000;'>and</span>
  <span style='color:#ff9900;'>Daily Vaccinations</span>
  <br>
  </span>") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=8),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0, face = "bold",size=5)) +
  facet_wrap(~title, nrow = 3)

# Correlation Figures: Arrange -------------------------------------------------
p_cor_trends <- ggarrange(p_cor_trends_vaxappt,
                          p_cor_trends_sideeffects,
                          ncol = 1)

p_cor <- ggarrange(p_cor_coef,
                   p_cor_trends,
                   ncol=2)

ggsave(p_cor, filename = file.path(paper_figures, "vax_cor.png"), height = 10, width = 13)

