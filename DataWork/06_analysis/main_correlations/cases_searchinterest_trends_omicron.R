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
  ungroup() %>%
  dplyr::filter(keyword_en %in% c("loss of smell",
                                  "fever")) %>%
  dplyr::filter(date >= ymd("2020-06-01")) %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = hits_ma7 / max(hits_ma7, na.rm=T),
         cases_new = cases_new / max(cases_new, na.rm=T)) 

## Correlation in title
#gtrends_df <- gtrends_df %>%
#  dplyr::mutate(country = paste0(country, "\nCorrelation: ", cor_casesMA7_hitsMA7_nolag %>% round(2)))

#gtrends_df$country = gtrends_df$country %>% as.factor()
#gtrends_df$country <- reorder(gtrends_df$country, gtrends_df$cor_casesMA7_hitsMA7_nolag) %>% fct_rev

## Remove if correlation is NA
#gtrends_df <- gtrends_df %>%
#  dplyr::filter(!is.na(cor_casesMA7_hitsMA7_nolag))

# Figure: Top Countries --------------------------------------------------------
gtrends_sub_df <- gtrends_df %>%
  dplyr::filter(geo %in% c("US", "GB", "ZA", "FR", "IT", "DK", "NO", "AU"),
                keyword_en %in% c("fever",
                                  "loss of smell")) 

p_omicron <- ggplot() +
  geom_col(data = gtrends_sub_df,
           aes(x = date, y = cases_new),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(data = gtrends_sub_df,
            aes(x = date, y = hits_ma7*2,
                color = keyword_en),
            #color = "#3AA959", # green4
            size=0.5) + # .75 olivedrab3 deepskyblue
  # geom_line(data = gtrends_sub_df[gtrends_sub_df$keyword_en %in% "loss of smell",],
  #           aes(x = date, y = hits_ma7),
  #           color = "#3AA959", # green4
  #           size=0.5) + # .75 olivedrab3 deepskyblue
  # geom_line(data = gtrends_sub_df[gtrends_sub_df$keyword_en %in% "fever",],
  #           aes(x = date, y = hits_ma7),
  #           color = "#BD0000", # red
  #           size=0.5) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in (1) Google Search Interest in</span> 
               <span style='color:#3AA959;'>'Loss of Smell'</span> 
               <span style='color:#000000;'>and</span>
               <span style='color:#BD0000;'>'Fever'</span> 
               <span style='color:#000000;'>and (2) </span>
    <span style='color:#ff9900;'>COVID-19 Cases</span>
    <br>
    </span>",
       subtitle = "June 1, 2020 - December 31, 2021") +
  scale_color_manual(values = c("#BD0000",
                                "#3AA959")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=14),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5),
        plot.subtitle = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=14)) +
  facet_wrap(~country, 
             ncol = 2,
             scales = "free") 

ggsave(p_omicron, filename = file.path(paper_figures, "cases_vs_loss_of_smell_trends_omicron.png"),
       heigh = 12, width=12)
