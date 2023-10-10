# Scatterplot Between Select Gtrends + COVID Correlations & GDP Per Capital

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "correlations_gtrends_since2020-01-01_until2021-12-31_symptoms.Rds")) 

gtrends_df <- gtrends_df %>%
  dplyr::mutate(keyword_en = keyword_en %>% 
                  tools::toTitleCase() %>% 
                  str_replace_all("\\bi\\b", "I") %>%
                  str_replace_all("Covid", "COVID"),
                title_gdp = paste0('Search Interest in "', keyword_en, '"\nversus GDP Per Capita, Logged'),
                title_internet = paste0('Search Interest in "', keyword_en, '"\nversus Percent of Pop. Using Internet'),
                title_mobile = paste0('Search Interest in "', keyword_en, '"\nversus Mobile Phone Subs. per 100'),
                title_cases = paste0('Search Interest in "', keyword_en, '"\nversus Total COVID-19 Cases'))

### GDP
p <- gtrends_df %>%
  dplyr::filter(keyword_en %in% c("Loss of Smell", 
                                  "Loss of Taste",
                                  "COVID Symptoms")) %>%
  ggplot(aes(x = log(gdp_pc),
             y = cor)) +
  geom_point(pch = 21,
             color = "black",
             fill = "gray50") +
  geom_smooth(method = "lm",
              color = "wheat4",
              fill = "wheat1",
              size = 0.5) +
  labs(x = "GDP Per Capita, Logged",
       y = "Correlation Between\nCOVID-19 Cases and\nSearch Interest") +
  theme_clean() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
          color = "white"
        ),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~title_gdp, nrow = 1)

ggsave(p, filename = file.path(paper_figures, "cor_gdp_scatter.png"),
       height = 3.5,
       width = 10)

### Internet
p <- gtrends_df %>%
  dplyr::filter(keyword_en %in% c("Loss of Smell", 
                                  "Loss of Taste",
                                  "COVID Symptoms")) %>%
  ggplot(aes(x = per_pop_using_internet,
             y = cor)) +
  geom_point(pch = 21,
             color = "black",
             fill = "gray50") +
  geom_smooth(method = "lm",
              color = "wheat4",
              fill = "wheat1",
              size = 0.5) +
  labs(x = "Percent of Population Using Internet",
       y = "Correlation Between\nCOVID-19 Cases and\nSearch Interest") +
  theme_clean() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
          color = "white"
        ),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~title_internet, nrow = 1)

ggsave(p, filename = file.path(paper_figures, "cor_internet_scatter.png"),
       height = 3.5,
       width = 10)
  
### Mobile
p <- gtrends_df %>%
  dplyr::filter(keyword_en %in% c("Loss of Smell", 
                                  "Loss of Taste",
                                  "COVID Symptoms")) %>%
  ggplot(aes(x = mobile_cell_sub_per100,
             y = cor)) +
  geom_point(pch = 21,
             color = "black",
             fill = "gray50") +
  geom_smooth(method = "lm",
              color = "wheat4",
              fill = "wheat1",
              size = 0.5) +
  labs(x = "Mobile Cell Phone Subscribers per 100",
       y = "Correlation Between\nCOVID-19 Cases and\nSearch Interest") +
  theme_clean() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
          color = "white"
        ),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~title_mobile, nrow = 1)

ggsave(p, filename = file.path(paper_figures, "cor_mobilecell_scatter.png"),
       height = 3.5,
       width = 10)

#### Total Cases
p <- gtrends_df %>%
  dplyr::filter(keyword_en %in% c("Loss of Smell", 
                                  "Loss of Taste",
                                  "COVID Symptoms")) %>%
  ggplot(aes(x = log(cases_total),
             y = cor)) +
  geom_point(pch = 21,
             color = "black",
             fill = "gray50") +
  geom_smooth(method = "lm",
              color = "wheat4",
              fill = "wheat1",
              size = 0.5) +
  labs(x = "Total COVID-19 Cases, as of December 31, 2021",
       y = "Correlation Between\nCOVID-19 Cases and\nSearch Interest") +
  theme_clean() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
          color = "white"
        ),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~title_cases, nrow = 1)

ggsave(p, filename = file.path(paper_figures, "cor_casestotal_scatter.png"),
       height = 3.5,
       width = 10)


