# Tanzania Figures

gtrends_df <- file.path(dropbox_file_path, "Data", "google_trends", "RawData",
                        "global_with_ref_state_by_keyword") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  unique()

# Merge in English Version of Keyword ------------------------------------------
keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                              "keywords", "FinalData","covid_keywords_alllanguages_clean.Rds"))

#### Dataset with english and translated word
keywords <- keywords %>%
  dplyr::select(names(keywords) %>% str_subset("keyword")) %>%
  dplyr::mutate(keyword = keyword_en) %>%
  pivot_longer(cols = -c(keyword)) %>%
  dplyr::rename(keyword_en = keyword) %>%
  dplyr::rename(keyword = value) %>%
  dplyr::select(keyword_en, keyword) %>%
  
  mutate(keyword = keyword %>% tolower(),
         keyword_en = keyword_en %>% tolower())

#### Merge
gtrends_df <- merge(gtrends_df, keywords, by = "keyword", all.x=T, all.y=F)

gtrends_df <- gtrends_df %>%
  dplyr::filter(geo %in% "TZ")

gtrends_df$date <- gtrends_df$date %>% as.Date()
gtrends_df$hits <- gtrends_df$hits %>% as.numeric()

# Moving Average ---------------------------------------------------------------
gtrends_df <- gtrends_df %>%
  arrange(date) %>%
  group_by(geo, keyword) %>%
  mutate(hits_ma7 = runMean(hits, n = 7))

gtrends_df$keyword_lan <- paste0(gtrends_df$language, ": ", gtrends_df$keyword)

# Figures ----------------------------------------------------------------------

library(gridExtra)

gtrends_df$keyword_en %>% unique()

p <- lapply(c("corona symptoms", 
              "coronavirus",
              "fatigue",
              "coronavirus test",
              "covid symptoms",
              "covid-19",
              "fever",
              "insomnia",
              "i have coronavirus",
              "sore throat",
              "loss of smell",
              "loss of taste",
              "pneumonia",
              "worry",
              "shortness of breath",
              "wash hands") %>% sort(), function(keyword_i){
  gtrends_df %>%
    filter(keyword_en %in% all_of(keyword_i)) %>%
    ggplot(aes(x = date, y = hits_ma7, 
               group = keyword_lan,
               size = keyword_lan,
               color = keyword_lan)) +
    geom_line() +
    labs(x = NULL,
         y = "Search Interest",
         title = keyword_i,
         size = NULL,
         color = NULL) + 
    theme_minimal() + 
    scale_color_manual(values = c("orange", "dodgerblue1")) + 
    scale_size_manual(values = c(1.1, 0.5)) +
    theme(plot.title = element_text(face = "bold"))
}) %>%
  do.call(what = "grid.arrange") 

ggsave(p, filename = "~/Desktop/gtrends_tza.png", height = 15, width = 20)





