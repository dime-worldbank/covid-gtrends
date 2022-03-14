


FIGURES_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData",
                          "data")

gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor.Rds")))

#### Add variables
gtrends_df <- gtrends_df %>%
  arrange(date) %>%
  group_by(geo, keyword_en) %>%
  dplyr::mutate(cases_total = max(cases, na.rm = T),
                death_total = max(death, na.rm = T)) %>%
  dplyr::mutate(cases_new_ma7 = runMean(cases_new, n = 7),
                death_new_ma7 = runMean(death_new, n = 7),
                hits_ma7 = runMean(hits, n = 7)) %>%
  dplyr::filter(!is.na(hits_ma7)) %>%
  dplyr::mutate(cases_hits_cor = cor(cases_new, hits_ma7),
                death_hits_cor = cor(death_new, hits_ma7)) %>%
  ungroup() %>%
  mutate(keyword_en = keyword_en %>% tools::toTitleCase())

df <- gtrends_df %>%
  filter(keyword_en %in% "Loss of Smell") %>%
  filter(Country %in% "United States of America") 
                

color_cases <- "orange"
color_hits <- "forestgreen"
           

arrow_df <- data.frame(date = c("2020-06-25", "2020-07-10") %>% as.Date(),
                       value = rep(55000,2))
     
p <- ggplot() +
  geom_line(data=df, aes(x = date, y = cases_new_ma7), color=color_cases,
            size = 0.8) +
  
  geom_line(data=df, aes(x = date - 12, y = cases_new_ma7), color=color_cases,
            size = 0.75,
            linetype = "longdash") +

  geom_line(data=df, aes(x = date, y = hits_ma7*1200), color=color_hits,
            size=0.8) +
  
  geom_line(data = arrow_df, aes(x = date, y = value),
            arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")) +
  
  geom_text(aes(x = "2020-06-15" %>% as.Date,
            y = 60000),
            label = "-12 Days") +

  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Search\nPopularity",
                                         breaks = c(0, 60000),
                                         labels = c("Low", "High"))) +
  labs(x = "",
       y = "Cases",
       title = NULL) +
  theme_minimal() +
  theme(axis.title.y.left = element_text(#angle = 0, 
                                         #vjust = 0.5, 
                                         color=color_cases,
                                         face = "bold",
                                         size=13),
        axis.title.y.right = element_text(#angle = 0, 
                                          #vjust = 0.5, 
                                          color=color_hits,
                                          face = "bold",
                                          size=13),
        axis.text.y.left = element_text(color = color_cases,
                                        size=13),
        axis.text.y.right = element_text(color = color_hits,
                                         size=13),
        axis.text = element_text(face = "bold", size=13))

saveRDS(p, file.path(FIGURES_PATH, "us_ex_fig_arrow.Rds"))

