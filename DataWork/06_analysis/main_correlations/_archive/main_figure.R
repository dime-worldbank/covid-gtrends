# Correlations: Main Figure

keywords_en_use <- KEYWORDS_TIMESERIES_ALL_lw

# BOXPLOT FIGURE ===============================================================

# ** Load Data -------------
cor_1_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2020-01-01_until2020-12-31.Rds")) %>%
  dplyr::mutate(date_since = "2020 [Jan - Dec]")

cor_2_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2021-01-01_until2021-09-30.Rds")) %>%
  dplyr::mutate(date_since = "2021 [Jan - Sept]")

cor_df <- bind_rows(cor_1_df,
                    cor_2_df) %>%
  dplyr::filter(type %in% "Cases") %>%
  dplyr::filter(keyword_en %in% keywords_en_use) %>%
  
  # For ranking correlations show terms with highest median correlation
  group_by(keyword_en) %>%
  dplyr::mutate(cor_sort_all = quantile(cor_nolag, 0.95, na.rm=T)) %>%
  ungroup() %>%
  
  # Cleanup variables
  mutate(keyword_en = keyword_en %>% 
           tools::toTitleCase() %>% 
           str_replace_all("\\bi\\b", "I"),
         date_since = date_since %>% as.factor() %>% fct_rev)

cor_df$keyword_en <- factor(cor_df$keyword_en, 
                            levels=unique(cor_df$keyword_en[order(cor_df$cor_sort_all)]), 
                            ordered=TRUE)

cor_df$date_since <- cor_df$date_since %>% as.factor() %>% fct_rev

#### Order Keywords
cor_avg_df <- cor_df %>%
  dplyr::filter(date_since %in% c("2020 [Jan - Dec]")) %>%
  group_by(keyword_en) %>%
  dplyr::summarise(cor_avg = median(cor_nolag)) %>%
  arrange(desc(cor_avg)) %>%
  dplyr::mutate(keyword_en = keyword_en %>% as.character())

cor_df$keyword_en <- cor_df$keyword_en %>%
  as.character() %>%
  factor(levels = rev(cor_avg_df$keyword_en))

## To long
cor_long_df <- cor_df %>%
  pivot_longer(c(cor, cor_nolag, lag)) 

# Boxplot ======================================================================
make_boxplot <- function(cor_df){
  
  ALPHA_VLINE <- 0.3
  
  p <- cor_df %>%
    ggplot(aes(x = value,
               y = keyword_en,
               fill = date_since)) +
    #geom_vline(xintercept = -1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = -0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = 0, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = 0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = 1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_boxplot(alpha = 0.7) +
    geom_point(position = position_jitterdodge(jitter.width=0.3,
                                               dodge.width = 0.85),
               pch = 21,
               size = 0.9, # 0.7
               stroke = 0.2, # 0.1
               color = "black") +
    geom_vline(xintercept = 0, color = "firebrick1") +
    scale_fill_manual(values = c("orange2", "dodgerblue3")) +
    labs(fill = "Using data since:",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          panel.border = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold", color = "black"),
          axis.text.y = element_text(color = "black")) +
    facet_wrap(~name_full,
               scales = "free_x")
  
  p
  
}

p <- cor_long_df %>%
  dplyr::filter(name %in% c("cor_nolag", "lag")) %>%
  dplyr::mutate(name_full = case_when(
    name == "cor_nolag" ~ "A. Correlation",
    name == "lag" ~ "B. Lag with best correlation"
  )) %>%
  make_boxplot()

ggsave(p, filename = file.path(paper_figures, "cor_lag_fig.png"),
       height = 7, width = 11)

# SI ---------------------------------------------------------------------------
p <- cor_long_df %>%
  dplyr::filter(name %in% c("cor_nolag", "cor")) %>%
  dplyr::mutate(name_full = case_when(
    name == "cor_nolag" ~ "A. Correlation",
    name == "cor" ~ "B. Correlation using best lag"
  )) %>%
  make_boxplot() +
  xlim(c(-1, 1))

ggsave(p, filename = file.path(paper_figures, "cor_corbest_fig.png"),
       height = 8, width = 12)

