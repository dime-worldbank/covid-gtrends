# Example Trends

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "gtrends_since2020-01-01_until2021-12-31_symptoms.Rds"))

gtrends_df$cases_new <- gtrends_df$cases_new_ma7

# Prep Data --------------------------------------------------------------------
## Subset and scale between 0 and 1
gtrends_df <- gtrends_df %>%
  dplyr::filter(keyword_en %in% "loss of smell") %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = hits_ma7 / max(hits_ma7, na.rm=T),
         cases_new = cases_new / max(cases_new, na.rm=T)) 

## Shorten country name
gtrends_df$country[gtrends_df$country %in% "occupied Palestinian territory, including east Jerusalem"] <- "Palestine"
gtrends_df$country[gtrends_df$country %in% "Iran (Islamic Republic of)"] <- "Iran"
gtrends_df$country[gtrends_df$country %in% "Bolivia (Plurinational State of)"] <- "Bolivia"
gtrends_df$country[gtrends_df$country %in% "United Republic of Tanzania"] <- "Tanzania"
gtrends_df$country[gtrends_df$country %in% "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
gtrends_df$country[gtrends_df$country %in% "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
gtrends_df$country[gtrends_df$country %in% "Viet Nam"] <- "Vietnam"

## Correlation in title
gtrends_df <- gtrends_df %>%
  dplyr::mutate(country = paste0(country, "\nCorrelation: ", cor_casesMA7_hitsMA7_nolag %>% round(2)))

gtrends_df$country = gtrends_df$country %>% as.factor()
gtrends_df$country <- reorder(gtrends_df$country, gtrends_df$cor_casesMA7_hitsMA7_nolag) %>% fct_rev

## Remove if correlation is NA
gtrends_df <- gtrends_df %>%
  dplyr::filter(!is.na(cor_casesMA7_hitsMA7_nolag))

# Figure: Top Countries --------------------------------------------------------
geo_use <- gtrends_df %>%
  distinct(geo, cor_casesMA7_hitsMA7_nolag) %>%
  arrange(desc(cor_casesMA7_hitsMA7_nolag)) %>%
  head(28) %>%
  pull(geo)

p_top <- gtrends_df %>%
  dplyr::filter(geo %in% geo_use) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new),
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
       subtitle = "January 1, 2020 - December 31, 2021") +
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

ggsave(p_top, filename = file.path(paper_figures, "cases_vs_loss_of_smell_trends_topcountries.png"),
       height = 14.2, width=21)
# height = 21, width=14.2

# Figure: All Countries --------------------------------------------------------
p_all <- gtrends_df %>%
  filter(cases_total > 0) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new),
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
       subtitle = "January 1, 2020 - December 31, 2021") +
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

# Figure: Omicron Countries ----------------------------------------------------
# p_omicron <- gtrends_df %>%
#   filter(cases_total > 0) %>%
#   filter(geo %in% c("US", 
#                     "ZA",
#                     "DK",
#                     "GB")) %>%
#   ggplot() +
#   geom_col(aes(x = date, y = cases_new),
#            fill = "#ffc266", # orange3
#            color = "#ffc266") +
#   geom_line(aes(x = date, y = hits_ma7),
#             color = "#3AA959", # green4
#             size=0.5) + # .75 olivedrab3 deepskyblue
#   labs(x = "",
#        y = "",
#        title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in</span> 
#                <span style='color:#3AA959;'>'Loss of Smell'</span> 
#                <span style='color:#000000;'>and</span>
#     <span style='color:#ff9900;'>COVID-19 Cases</span>
#     <br>
#     </span>",
#        subtitle = "January 1, 2020 - December 31, 2021") +
#   theme_minimal() +
#   theme(axis.text.y = element_blank(),
#         axis.text.x = element_blank(),
#         strip.text = element_text(face = "bold", color = "black", size=10),
#         plot.background = element_rect(fill = "white", color = "white"),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_blank(),
#         plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5),
#         plot.subtitle = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=14)) +
#   facet_wrap(~country, 
#              ncol = 8,
#              scales = "free") 
# 
# 
# ggsave(p_all, filename = file.path(paper_figures, "cases_vs_loss_of_smell_trends_allcountries.png"),
#        heigh = 18, width=15.2)
# 
