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
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete_contain.Rds"))

#### Adjust Variables
gtrends_df$geo_name <- gtrends_df$geo %>% countrycode(origin = "iso2c", destination = "iso.name.en")

gtrends_df <- gtrends_df %>%
  mutate(days_since_c_policy_fact = factor(days_since_c_policy) %>% relevel(ref = "-1")) 

#gtrends_df <- gtrends_df[!is.na(gtrends_df$wb_region),]
#gtrends_df$wb_region[gtrends_df$wb_region %in% "Europe & Central Asia"]       <- "Europe & Cent. Asia"
#gtrends_df$wb_region[gtrends_df$wb_region %in% "Latin America & Caribbean"]   <- "Lat. Am. & Caribbean"
#gtrends_df$wb_region[gtrends_df$wb_region %in% "Middle East & North Africa"] <- "Middle East & N Africa"

#### Remove if no hits or policy
gtrends_df <- gtrends_df %>%
  dplyr::group_by(geo, keyword_en) %>%
  dplyr::mutate(hits_SUM = sum(hits, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(hits_SUM > 0,
                !is.na(days_since_c_policy))

# 2. Event Study Figures -------------------------------------------------------
#### Make Data
make_es_data <- function(keyword, df, THRESH){
  print(keyword)
  
  df <- df %>%
    filter(keyword_en %in% !!keyword,
           abs(days_since_c_policy) <= THRESH) 
  
  if(nrow(df) > 0){
    data_lm <- df %>%
      felm(hits ~ days_since_c_policy_fact | geo | 0 | date, data = .) %>%
      lm_post_confint_tidy() %>%
      filter(variable != "(Intercept)") %>%
      mutate(variable = variable %>%
               str_replace_all("days_since_c_policy_fact", "") %>%
               as.numeric()) 
    data_lm$N <- length(unique(df$geo))
    data_lm$keyword <- keyword
    
  } else{
    data_lm <- data.frame(NULL)
  }
  
  return(data_lm)
}

for(THRESHOLD_DAYS in c(90)){
  print(paste(THRESHOLD_DAYS, "----------------------------------------------"))
  
  df_fig <- map_df(KEYWORDS_CONTAIN_USE, 
                   make_es_data, 
                   gtrends_df,
                   THRESHOLD_DAYS) %>%
    dplyr::mutate(#region_name = paste0(region, "\nN Countries: ", N),
      sig = (p025 > 0 & p975 > 0) | (p025 < 0 & p975 < 0)) %>%
    dplyr::mutate(sig = ifelse(sig, "Yes", "No")) %>%
    dplyr::mutate(keyword = keyword %>% tools::toTitleCase())
  
  p <- df_fig %>%
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
    geom_point(size = 0.5) +
    geom_linerange(size = 0.2) +
    labs(x = NULL,
         y = "Coef +/- 95% CI",
         color = "Significant (p < 0.05)",
         title = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = 10.5),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_color_manual(values = c("black", "red")) +
    facet_wrap(~keyword,
               scales = "free_y",
               ncol = 4) 
  HEIGHT = 12
  WIDTH = 12
  ggsave(p, filename = file.path(paper_figures, paste0("es_global_",THRESHOLD_DAYS,"days.png")), height = HEIGHT, width = WIDTH)
}
