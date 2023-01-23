# Lockdown Difference-in-Difference Analysis

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                                "did_results_country.Rds"))

results_df <- results_df %>%
  dplyr::filter(variable == "days_since_lockdown_min_yearcurrent_post_X_year2020")

results_df$geo_name <- results_df$geo %>% countrycode(origin = "iso2c", destination = "iso.name.en")
results_df <- results_df %>%
  dplyr::mutate(geo_name = geo_name %>%
                  str_replace_all("\\(the\\)", "") %>%
                  str_replace_all("\\(the Republic of\\)", "") %>%
                  str_squish()) %>%
  dplyr::mutate(geo_name = case_when(
    geo_name == "United Arab Emirates (the)" ~ "United Arab Emirates",
    geo_name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    geo_name == "Bahamas (the)" ~ "Bahamas",
    geo_name == "Congo (the Democratic Republic of the)" ~ "Congo (DRC)",
    geo_name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    geo_name == "United States of America (the)" ~ "United States of America",
    geo_name == "Iran (Islamic Republic of)" ~ "Iran",
    geo_name == "Taiwan (Province of China)" ~ "Taiwan",
    geo_name == "United States of America" ~ "USA",
    geo_name == "Palestine, State of" ~ "Palestine",
    #geo_name == "" ~ "",
    geo_name == "" ~ "",
    TRUE ~ geo_name
  ))

results_df$sig <- ifelse(results_df$pvalue <= 0.05, "Yes", "No")

# Figure -----------------------------------------------------------------------
make_figure <- function(keyword_i, results_df){
  
  results_df_i <- results_df %>%
    dplyr::filter(keyword %in% keyword_i)
  
  results_df_i$geo_name <- fct_reorder(results_df_i$geo_name, results_df_i$b)
  
  results_df_i %>% 
    ggplot(aes(xmin = p025,
               xmax = p975,
               x = b,
               y = geo_name,
               color = sig)) +
    geom_vline(xintercept = 0) +
    geom_linerange() +
    geom_point() +
    labs(color = NULL,
         y = NULL,
         x = "Coefficient (+/- 95% CI)",
         color = "Significant (p < 0.05)",
         title = keyword_i %>% tools::toTitleCase()) +
    scale_color_manual(values = c("black", "firebrick2")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          legend.position = "bottom") 
}

p1 <- make_figure("anxiety", results_df)
p2 <- make_figure("anxiety attack", results_df)
p3 <- make_figure("anxiety symptoms", results_df)
p4 <- make_figure("boredom", results_df)

p5 <- make_figure("divorce", results_df)
p6 <- make_figure("hysteria", results_df)
p7 <- make_figure("insomnia", results_df)
p8 <- make_figure("loneliness", results_df)

p9 <- make_figure("lonely", results_df)
p10 <- make_figure("overwhelmed", results_df)
p11 <- make_figure("social distance", results_df)
p12 <- make_figure("social isolation", results_df)

p13 <- make_figure("stay at home", results_df)
p14 <- make_figure("suicide", results_df)
p15 <- make_figure("unemployment", results_df)
p16 <- make_figure("unemployment insurance", results_df)

p <- ggarrange(p1, p2, p3, p4, nrow = 1, common.legend = T) 
ggsave(p, filename = file.path(paper_figures, "did_country_fig_1.png"),
       height = 15, width = 22)

p <- ggarrange(p5, p6, p7, p8, nrow = 1, common.legend = T) 
ggsave(p, filename = file.path(paper_figures, "did_country_fig_2.png"),
       height = 15, width = 22)

p <- ggarrange(p9, p10, p11, p12, nrow = 1, common.legend = T) 
ggsave(p, filename = file.path(paper_figures, "did_country_fig_3.png"),
       height = 15, width = 22)

p <- ggarrange(p13, p14, p15, p16, nrow = 1, common.legend = T) 
ggsave(p, filename = file.path(paper_figures, "did_country_fig_4.png"),
       height = 15, width = 22)

# Check Figures ----------------------------------------------------------------
if(F){
  gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                  "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))
  
  gtrends_df %>%
    dplyr::filter(geo %in% "GR",
                  keyword_en %in% "loneliness",
                  abs(days_since_lockdown_min_yearcurrent) <= 30,
                  year %in% c(2019,2020)) %>%
    #dplyr::mutate(mm_dd_2020 = paste0("2020-", mm_dd) %>% ymd()) %>%
    ggplot() +
    geom_line(aes(x = days_since_lockdown_min_yearcurrent,
                  y = hits_ma7,
                  color = factor(year)))
}






