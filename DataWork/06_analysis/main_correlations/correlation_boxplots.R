# Correlations: Main Figure

keywords_en_use <- KEYWORDS_SYMTPOMS

# BOXPLOT FIGURE ===============================================================

# ** Load Data -------------
cor_1_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2020-01-01_until2020-12-31_symptoms.Rds")) %>%
  dplyr::mutate(timespan = "2020")

cor_2_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2021-01-01_until2021-12-31_symptoms.Rds")) %>%
  dplyr::mutate(timespan = "2021")

cor_3_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2022-01-01_until2022-12-31_symptoms.Rds")) %>%
  dplyr::mutate(timespan = "2022")

cor_df <- bind_rows(cor_1_df,
                    cor_2_df,
                    cor_3_df) %>%
  dplyr::filter(!is.na(cor_nolag)) %>%
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
         timespan = timespan %>% factor(levels = c("2022", "2021", "2020"))) %>%
  
  # Add number of countries for each keyword / time
  group_by(timespan, keyword_en) %>%
  mutate(n_country = n()) %>%
  ungroup() %>%
  
  group_by(keyword_en) %>%
  mutate(n_country = max(n_country)) %>%
  ungroup() %>%
  
  mutate(keyword_en = paste0(keyword_en, " [N = ", n_country, "]"))


cor_df$keyword_en <- factor(cor_df$keyword_en, 
                            levels=unique(cor_df$keyword_en[order(cor_df$cor_sort_all)]), 
                            ordered=TRUE)

#### Order Keywords
cor_avg_df <- cor_df %>%
  dplyr::filter(timespan %in% c("2020")) %>%
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
make_boxplot <- function(cor_df, fill_var, facet_var){
  
  ALPHA_VLINE <- 0.3

  cor_df$fill_var <- cor_df[[fill_var]]
  cor_df$facet_var <- cor_df[[facet_var]]

  p <- cor_df %>%
    ggplot(aes(x = value,
               y = keyword_en,
               fill = fill_var)) +
    #geom_vline(xintercept = -1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = -0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = 0, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = 0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    #geom_vline(xintercept = 1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
    geom_vline(xintercept = 0, color = "firebrick1") +
    
    geom_boxplot(alpha = 0.7) +
    # geom_point(position = position_jitterdodge(jitter.width=0.3,
    #                                            dodge.width = 0.85),
    #            pch = 21,
    #            size = 0.9, # 0.7
    #            stroke = 0.2, # 0.1
    #            alpha = 0.75,
    #            color = "black") +
    scale_fill_manual(values = c("orange2", "palegreen3", "dodgerblue3"),
                      guide = guide_legend(reverse = TRUE)) +
    labs(fill = "Using\ndata in",
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          panel.border = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold", color = "black", size=12),
          axis.text.y = element_text(color = "black")) +
    facet_wrap(~facet_var,
               scales = "free_x")
  
  p
  
}

p <- cor_long_df %>%
  dplyr::filter(name %in% c("cor_nolag", "lag")) %>%
  dplyr::mutate(name_full = case_when(
    name == "cor_nolag" ~ "A. Correlation",
    #name == "cor" ~ "B. Correlation using best lag",
    name == "lag" ~ "B. Lag with best correlation (days)"
  )) %>%
  make_boxplot(fill_var = "timespan",
               facet_var = "name_full")
p
ggsave(p, filename = file.path(paper_figures, "cor_lag_fig.png"),
       height = 7, width = 11)

# SI ---------------------------------------------------------------------------
p <- cor_long_df %>%
  dplyr::filter(name %in% c("cor_nolag", "cor")) %>%
  dplyr::mutate(name_full = case_when(
    name == "cor_nolag" ~ "Correlation",
    name == "cor" ~ "Correlation using\nbest lag"
  )) %>%
  dplyr::mutate(timespan = case_when(
    timespan == "2020" ~ "A. Using Data in 2020",
    timespan == "2021" ~ "B. Using Data in 2021",
    timespan == "2022" ~ "B. Using Data in 2022")) %>%
  make_boxplot(fill_var = "name_full",
               facet_var = "timespan") +
  ggplot2::xlim(c(-1, 1))

ggsave(p, filename = file.path(paper_figures, "cor_corbest_fig.png"),
       height = 6.5, width = 11)

