# Example Trends

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "gtrends_since2020-01-01_until2022-12-31_symptoms.Rds"))

# Prep Data --------------------------------------------------------------------
## Subset and scale between 0 and 1
gtrends_df <- gtrends_df %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = hits_ma7 / max(hits_ma7, na.rm=T),
         cases_new_ma7 = cases_new_ma7 / max(cases_new_ma7, na.rm=T),
         hits = hits / max(hits, na.rm=T),
         cases_new = cases_new / max(cases_new, na.rm=T))

## Shorten country name
gtrends_df$country[gtrends_df$country %in% "occupied Palestinian territory, including east Jerusalem"] <- "Palestine"
gtrends_df$country[gtrends_df$country %in% "Iran (Islamic Republic of)"] <- "Iran"
gtrends_df$country[gtrends_df$country %in% "Bolivia (Plurinational State of)"] <- "Bolivia"
gtrends_df$country[gtrends_df$country %in% "United Republic of Tanzania"] <- "Tanzania"
gtrends_df$country[gtrends_df$country %in% "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
gtrends_df$country[gtrends_df$country %in% "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
gtrends_df$country[gtrends_df$country %in% "Viet Nam"] <- "Vietnam"

## Remove if correlation is NA
# gtrends_df <- gtrends_df %>%
#   dplyr::filter(!is.na(cor_casesMA7_hitsMA7_nolag))

gtrends_df <- gtrends_df %>%
  dplyr::filter(!is.na(cor_cases_hits_nolag)) %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(cor_casesMA7_hitsMA7_nolag = cor(cases_new_ma7[!is.na(cases_new_ma7)], hits_ma7[!is.na(cases_new_ma7)])) %>%
  ungroup()
 
gtrends_df$country = gtrends_df$country %>% as.factor()

## Correlation in title
gtrends_df <- gtrends_df %>%
  dplyr::mutate(country = paste0(country, "\nCor [7 Day MA]: ",round(cor_casesMA7_hitsMA7_nolag,2), "; Cor: ", cor_cases_hits_nolag %>% round(2)))

gtrends_df$country <- reorder(gtrends_df$country, gtrends_df$cor_casesMA7_hitsMA7_nolag) %>% fct_rev

# Figure: Top Countries: Loss of Smell --------------------------------------------------------
geo_use_smell <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "loss of smell") %>%
  distinct(geo, cor_casesMA7_hitsMA7_nolag) %>%
  arrange(desc(cor_casesMA7_hitsMA7_nolag)) %>%
  head(14) %>%
  pull(geo)

p_top_smell <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "loss of smell") %>%
  dplyr::filter(geo %in% geo_use_smell) %>%
  ggplot() +
  #geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, color = "black") +
  #geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, color = "black") +
  geom_col(aes(x = date, y = cases_new_ma7),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.5) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in</span> 
               <span style='color:#3AA959;'>'Loss of Smell'</span> 
               <span style='color:#000000;'>and</span>
    <span style='color:#ff9900;'>COVID-19 Cases</span>
    <br>
    </span>",
       subtitle = "January 1, 2020 - December 31, 2022") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=14),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5),
        plot.subtitle = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=14)) +
  facet_wrap(~country, 
             ncol = 7,
             scales = "free") 


# Figure: Top Countries: Loss of Smell --------------------------------------------------------
geo_use_symp <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "covid symptoms") %>%
  distinct(geo, cor_cases_hits_nolag) %>%
  arrange(desc(cor_cases_hits_nolag)) %>%
  head(14) %>%
  pull(geo)

p_top_symp <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "covid symptoms") %>%
  dplyr::filter(geo %in% geo_use_symp) %>%
  ggplot() +
  #geom_vline(xintercept = ymd("2021-01-01"), size = 0.5, color = "black") +
  #geom_vline(xintercept = ymd("2022-01-01"), size = 0.5, color = "black") +
  geom_col(aes(x = date, y = cases_new_ma7),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.5) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in </span><span style='color:#3AA959;'>'COVID Symptoms'</span><span style='color:#000000;'>and </span><span style='color:#ff9900;'>COVID-19 Cases</span><br></span>",
       subtitle = "January 1, 2020 - December 31, 2022") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=14),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5),
        plot.subtitle = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=14)) +
  facet_wrap(~country, 
             ncol = 7,
             scales = "free") 
#facet_wrap(~country, 
#           ncol = 4,
#           scales = "free") 

p_top <- ggarrange(p_top_smell,
                   p_top_symp,
                   ncol = 1)

ggsave(p_top, filename = file.path(paper_figures, "cases_vs_loss_of_smell_covid_symptoms_trends_topcountries.png"),
       height = 14.2, width=21) # *1.18
# height = 21, width=14.2

# Figure: All Countries: Loss of Smell --------------------------------------------------------
gtrends_df$country <- reorder(gtrends_df$country, gtrends_df$cor_cases_hits_nolag) %>% fct_rev

p_all <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "loss of smell") %>%
  filter(cases_total > 0) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new_ma7),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.5) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in</span> 
               <span style='color:#3AA959;'>'Loss of Smell'</span> 
               <span style='color:#000000;'>and</span>
    <span style='color:#ff9900;'>COVID-19 Cases</span>
    <br>
    </span>",
       subtitle = "January 1, 2020 - December 31, 2022") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=10),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5),
        plot.subtitle = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=14)) +
  facet_wrap(~country, 
             ncol = 8,
             scales = "free") 

ggsave(p_all, filename = file.path(paper_figures, "cases_vs_loss_of_smell_trends_allcountries.png"),
       heigh = 21, width=18)


# Figure: All Countries: COVID Symptoms --------------------------------------------------------
p_all <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "covid symptoms") %>%
  filter(cases_total > 0) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new_ma7),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.5) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in</span> 
               <span style='color:#3AA959;'>'COVID Symptoms'</span> 
               <span style='color:#000000;'>and</span>
    <span style='color:#ff9900;'>COVID-19 Cases</span>
    <br>
    </span>",
       subtitle = "January 1, 2020 - December 31, 2022") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=10),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5),
        plot.subtitle = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=14)) +
  facet_wrap(~country, 
             ncol = 8,
             scales = "free") 

ggsave(p_all, filename = file.path(paper_figures, "cases_vs_covid_symptoms_trends_allcountries.png"),
       heigh = 23, width=18)

# Figure: All Countries: COVID Symptoms --------------------------------------------------------
p_all <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "coronavirus") %>%
  filter(cases_total > 0) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new_ma7),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.5) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in</span> 
               <span style='color:#3AA959;'>'Coronavirus'</span> 
               <span style='color:#000000;'>and</span>
    <span style='color:#ff9900;'>COVID-19 Cases</span>
    <br>
    </span>",
       subtitle = "January 1, 2020 - December 31, 2022") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=10),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5),
        plot.subtitle = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=14)) +
  facet_wrap(~country, 
             ncol = 8,
             scales = "free") 

ggsave(p_all, filename = file.path(paper_figures, "cases_vs_coronavirus_trends_allcountries.png"),
       heigh = 23, width=18)





