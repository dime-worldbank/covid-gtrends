# Lockdown Analysis

lm_post_confint_tidy <- function(lm){
  
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint$tvalue <- summary(lm)$coefficients[,3] %>% as.vector()
  lm_confint$pvalue <- summary(lm)$coefficients[,4] %>% as.vector()
  
  return(lm_confint)
}

# 1. Load / Prep Data ----------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

gtrends_df$geo_name <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "iso.name.en")

gtrends_df <- gtrends_df %>%
  mutate(days_since_lockdown_min_fact = factor(days_since_lockdown_min),
         days_since_lockdown_min_fact = days_since_lockdown_min_fact %>% relevel(ref = "-1")) 

gtrends_df <- gtrends_df[!is.na(gtrends_df$wb_region),]

gtrends_df$wb_region[gtrends_df$wb_region %in% "Europe & Central Asia"]       <- "Europe & Cent. Asia"
gtrends_df$wb_region[gtrends_df$wb_region %in% "Latin America & Caribbean"]   <- "Lat. Am. & Caribbean"
gtrends_df$wb_region[gtrends_df$wb_region %in% "Middle East & North Africa"] <- "Middle East & N Africa"

# 2. Event Study Figures -------------------------------------------------------
#### Make Data
make_es_data <- function(keyword, df){
  print(keyword)
  
  df <- df %>%
    filter(keyword_en %in% !!keyword,
           abs(days_since_lockdown_min) <= 30) 
  
  if(nrow(df) > 0){
    data_lm <- df %>%
      felm(hits_ma7 ~ days_since_lockdown_min_fact | geo | 0 | date, data = .) %>%
      lm_post_confint_tidy() %>%
      filter(variable != "(Intercept)") %>%
      mutate(variable = variable %>%
               str_replace_all("days_since_lockdown_min_fact", "") %>%
               as.numeric()) 
    data_lm$N <- length(unique(df$geo))
    data_lm$keyword <- keyword
    
  } else{
    data_lm <- data.frame(NULL)
  }
  
  return(data_lm)
}

df_fig <- map_df(unique(gtrends_df$wb_region), function(region){
  print(region)
  map_df(c("social distance", "telework", "stay at home", 
           "boredom", "sadness", "suicide", "stress", "divorce", "tired", "insomnia",
           "anxiety", "stress", "worry", "loneliness", "social isolation",
           "unemployment", "debt", "unemployment insurance", "unemployment office",
           "file for unemployment", "unemployment benefits"), make_es_data, gtrends_df[gtrends_df$wb_region %in% region,]) %>%
    dplyr::mutate(region = region)
}) %>%
  dplyr::mutate(region_name = paste0(region, "\nN Countries: ", N),
                sig = (p025 > 0 & p975 > 0) | (p025 < 0 & p975 < 0)) %>%
  dplyr::mutate(sig = ifelse(sig, "Yes", "No"))

#### Make Figure
make_figure <- function(keyword_i, 
                        color = "black",
                        title_i = NULL){
  if(is.null(title_i)) title_i <- keyword_i %>% tools::toTitleCase()

  df_fig %>%
    dplyr::filter(keyword %in% keyword_i) %>%
    ggplot(aes(x = variable,
               y = b,
               ymin = p025,
               ymax = p975,
               color = sig)) +
    geom_vline(xintercept = 0,
               color = "red") +
    geom_hline(yintercept = 0,
               color = "black",
               alpha = 0.5) +
    geom_point() +
    geom_linerange() +
    labs(x = NULL,
         y = "Coef +/- 95% CI",
         color = "Significant (p < 0.05)",
         title = paste0("Search Interest In: ", title_i)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = 10.5),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_color_manual(values = c("black", "red")) +
    facet_wrap(~region_name,
               scales = "free_y",
               nrow = 1) 
}

WIDTH <- 13

## Social distance
p <- ggarrange(
  make_figure("social distance"),
  make_figure("stay at home") + labs(x = "Days Since Lockdown"),
  ncol = 1,
  common.legend = T
)
ggsave(p, filename = file.path(paper_figures, "es_social_distance.png"), height = 7, width = WIDTH)


p <- ggarrange(
  make_figure("boredom"),
  make_figure("anxiety"),
  make_figure("suicide"),
  make_figure("insomnia"),
  make_figure("social isolation"),
  make_figure("loneliness"),
  make_figure("divorce") + labs(x = "Days Since Lockdown"),
  ncol = 1, 
  common.legend = T
)
ggsave(p, filename = file.path(paper_figures, "es_mental_health.png"), height = 20, width = WIDTH)

p <- ggarrange(
  make_figure("unemployment"),
  make_figure("unemployment insurance") + labs(x = "Days Since Lockdown"),
  ncol = 1,
  common.legend = T
)
ggsave(p, filename = file.path(paper_figures, "es_economic.png"), height = 7, width = WIDTH)


