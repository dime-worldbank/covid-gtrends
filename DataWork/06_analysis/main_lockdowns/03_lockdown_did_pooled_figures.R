# DiD: Pooled Results

## Parametere
COEF_AXIS_Y_TEXT_SIZE <- 14
COEF_POINT_SIZE <- 3.25
COEF_LINE_SIZE <- 1.25
STRIP_TEXT_SIZE <- 12
LEGEND_TEXT_SIZE <- 14
AXIS_X_TITLE_SIZE <- 12
PLOT_TITLE_SIZE <- 16

NA_COLOR <- "gray40"

keywords_to_use <- c("debt",
                     "file for unemployment",
                     "unemployment",
                     "unemployment benefits",
                     "unemployment insurance",
                     "unemployment office",
                     
                     "anxiety",
                     "anxiety attack",
                     #"anxiety Symptoms",
                     "boredom",
                     #"hysteria",
                     "insomnia",
                     #"loneliness",
                     "lonely",
                     "panic",
                     "social isolation",
                     "suicide",
                     
                     "divorce",
                     "wedding",
                     "emergency pill",
                     "pregnancy test",
                     
                     "social distance",
                     "stay at home")

prep_keywords <- function(df){
  
  df <- df %>%
    dplyr::mutate(keyword_type = case_when(
      keyword_en %in% c("anxiety",
                        "anxiety attack",
                        "anxiety symptoms",
                        "boredom",
                        "hysteria",
                        "insomnia",
                        "loneliness",
                        "lonely",
                        "panic",
                        "social isolation",
                        "overwhelmed",
                        "suicide") ~ "Mental\nHealth",
      
      keyword_en %in% c("debt",
                        "file for unemployment",
                        "unemployment",
                        "unemployment benefits",
                        "unemployment insurance",
                        "unemployment office") ~ "Economic",
      
      keyword_en %in% c("abortion",
                        "break up",
                        "condom",
                        "dating app",
                        "divorce",
                        "emergency pill",
                        "plan child",
                        "plan other children",
                        "pregnancy test",
                        "relationship",
                        "tinder",
                        "wedding") ~ "Relationships &\nFamily Planning",
      
      keyword_en %in% c("social distance",
                        "stay at home") ~ "Social\nDistancing"
    )) %>%
    dplyr::mutate(keyword_en = keyword_en %>% tools::toTitleCase()) %>%
    dplyr::mutate(keyword_en_newline = case_when(
      keyword_en == "File for Unemployment" ~ "File for\nUnemployment",
      keyword_en == "Unemployment Benefits" ~ "Unemployment\nBenefits",
      keyword_en == "Unemployment Insurance" ~ "Unemployment\nInsurance",
      keyword_en == "Unemployment Office" ~ "Unemployment\nOffice",
      #keyword_en == "" ~ "",
      TRUE ~ keyword_en
    ))
  
  keyword_factor_order <- df %>%
    distinct(keyword_en, keyword_type) %>%
    arrange(keyword_type, keyword_en) %>%
    pull(keyword_en)
  
  keyword_newline_factor_order <- df %>%
    distinct(keyword_en_newline, keyword_type) %>%
    arrange(keyword_type, keyword_en_newline) %>%
    pull(keyword_en_newline)
  
  df <- df %>%
    dplyr::mutate(keyword_en = factor(keyword_en, levels = keyword_factor_order),
                  keyword_en_newline = factor(keyword_en_newline, levels = keyword_newline_factor_order))
  
  return(df)
}

# Load/Prep Data [For Trends] --------------------------------------------------
for(days_thresh in c(30, 60, 90, 120, 180)){
  
  # N Countries per keyword
  n_country_df <- readRDS(file.path(data_dir, "google_trends", "FinalData", "results", 
                                    "did_pooled_data.Rds")) %>%
    dplyr::filter(abs(days_since_c_policy_yearcurrent) <= days_thresh) %>%
    distinct(keyword_en, geo) %>%
    group_by(keyword_en) %>%
    summarise(n_country = n()) %>%
    ungroup() %>%
    prep_keywords() %>%
    dplyr::select(keyword_en, n_country)
  
  df <- readRDS(file.path(data_dir, "google_trends", "FinalData", "results", 
                          "did_pooled_data.Rds")) %>%
    
    # Restrict to dates threshold
    dplyr::filter(abs(days_since_c_policy_yearcurrent) <= days_thresh) %>%
    
    # Filter keywords
    dplyr::filter(keyword_en %in% keywords_to_use) %>%
    
    ## Standardized hits value
    group_by(keyword_en, geo) %>%
    dplyr::mutate(hits_ma7_min = min(hits_ma7, na.rm=T),
                  hits_ma7_max = max(hits_ma7, na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(hits_ma7_std = ((hits_ma7 - hits_ma7_min) / (hits_ma7_max - hits_ma7_min))*100) %>%
    dplyr::group_by(keyword_en, pandemic_time, days_since_c_policy_yearcurrent) %>%
    dplyr::summarise(hits_ma7_std = mean(hits_ma7_std, na.rm = T)) %>%
    ungroup() %>%
    
    # Prep variables
    prep_keywords() %>%
    dplyr::mutate(pandemic_time = case_when(
      pandemic_time == 1 ~ "Pandemic",
      pandemic_time == 0 ~ "Pre-Pandemic"
    )) %>%
    left_join(n_country_df, by = "keyword_en") %>%
    mutate(keyword_en_newline = paste0(keyword_en_newline, "\n[N = ", n_country, "]"))
  
  # Load/Prep Regression Results -------------------------------------------------
  coef_df <- readRDS(file.path(data_dir, "google_trends", "FinalData", "results", 
                               paste0("did_pooled_results_",days_thresh,".Rds"))) %>%
    
    ## Prep keywords
    mutate(keyword_en = keyword) %>%
    dplyr::filter(keyword_en %in% keywords_to_use) %>%
    prep_keywords() %>%
    
    ## Prep Coefficients
    dplyr::filter(variable %>% 
                    str_detect("days_since_c_policy_yearcurrent_post_X_year2020|did_")) %>%
    dplyr::filter(type %in% c(
      "did_StringencyIndex_max",
      "did_EconomicSupportIndex_max",
      "did_gm_avg_min",
      "did_ln_gdp_pc",
      "income",
      "wb_region",
      
      #"did_gm_avg_min_AND_did_EconomicSupportIndex_max",
      #"did_StringencyIndex_max_AND_did_EconomicSupportIndex_max",
      #"did_gm_avg_min_AND_did_EconomicSupportIndex_max_gdppc",
      #"did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_gdppc",
      "Overall")) %>%
    mutate(variable = case_when(
      variable == "days_since_c_policy_yearcurrent_post_X_year2020" ~ "Post Policy X Pandemic",
      variable == "did_EconomicSupportIndex_max" ~ "Post Policy X Pandemic X Econ Support",
      variable == "did_gm_avg_min" ~               "Post Policy X Pandemic X Mobility Reduction",
      variable == "did_StringencyIndex_max" ~      "Post Policy X Pandemic X Stringency Index",
      
      variable == "did_ln_gdp_pc" ~ "Post Policy X Pandemic X GDP (per capita)",
      
      variable == "did_gm_avg_min_X_did_EconomicSupportIndex_max" ~               
        "Post Policy X Pandemic X Mobility Reduction X Econ Support",
      variable == "did_StringencyIndex_max_X_did_EconomicSupportIndex_max" ~      
        "Post Policy X Pandemic X Stringency Index X Econ Support"
    )) %>%
    mutate(type = case_when(
      #type == "did_gm_avg_min_AND_did_EconomicSupportIndex_max" ~ "mobility_reduction",
      #type == "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max" ~ "stringency_index",
      type == "did_gm_avg_min_AND_did_EconomicSupportIndex_max_gdppc" ~ "mobility_reduction",
      type == "did_StringencyIndex_max_AND_did_EconomicSupportIndex_max_gdppc" ~ "stringency_index",
      TRUE ~ type,
    )) %>%
    dplyr::mutate(variable = variable %>% fct_rev) %>%
    dplyr::mutate(variable = variable %>%
                    as.character() %>%
                    factor(levels = c("Post Policy X Pandemic",
                                      "Post Policy X Pandemic X Econ Support",
                                      "Post Policy X Pandemic X Mobility Reduction",
                                      "Post Policy X Pandemic X Stringency Index",
                                      "Post Policy X Pandemic X GDP (per capita)")) %>% 
                    fct_rev) %>%
    left_join(n_country_df, by = "keyword_en") %>%
    mutate(keyword_en_smline = paste0(keyword_en, " [N = ", n_country, "]"),
           keyword_en_nwline = paste0(keyword_en, "\n[N = ", n_country, "]"),
           keyword_en_newline = paste0(keyword_en_newline, "\n[N = ", n_country, "]"))
  
  # Trends Figure ----------------------------------------------------------------
  p_trends <- df %>%
    ggplot() +
    geom_vline(xintercept = 0) +
    geom_line(aes(x = days_since_c_policy_yearcurrent,
                  y = hits_ma7_std,
                  color = factor(pandemic_time))) +
    labs(color = "Time Period",
         x = "Days Since Lockdown",
         y = NULL,
         title = "A. Trends in Search Interest") +
    theme_classic() +
    theme(strip.text = element_text(face = "bold", size = STRIP_TEXT_SIZE),
          legend.position = "top",
          legend.text=element_text(size=LEGEND_TEXT_SIZE),
          legend.title=element_text(size=LEGEND_TEXT_SIZE),
          axis.title.x = element_text(size = AXIS_X_TITLE_SIZE),
          strip.background = element_blank()) +
    scale_color_manual(values = c("darkorange", "gray40")) +
    facet_wrap(~keyword_en_newline,
               ncol = 4,
               scales = "free_y") 
  
  # Overall Impact Figure --------------------------------------------------------
  p_overall <- coef_df %>% 
    dplyr::filter(type == "Overall") %>%
    dplyr::mutate(keyword_en_smline = keyword_en_smline %>% fct_rev()) %>%
    ggplot(aes(xmin = p025,
               xmax = p975,
               x = b,
               y = keyword_en_smline,
               color = keyword_type)) +
    geom_point(size = COEF_POINT_SIZE) +
    geom_linerange(size = COEF_LINE_SIZE) +
    geom_vline(xintercept = 0, 
               color = "black") +
    theme_classic() +
    labs(color = "Category",
         x = "Coefficient (+/- 95% CI)",
         y = NULL,
         title = "B. Diff-in-Diff Results: Impact of Contaiment\nPolicies on Search Interest") +
    theme(legend.position = "bottom",
          legend.margin = margin(t = 0, unit = "cm"), 
          legend.box.margin = margin(l = -8, unit = "cm"),
          axis.text.y = element_text(color = "black", size = COEF_AXIS_Y_TEXT_SIZE),
          axis.title.x = element_text(size = AXIS_X_TITLE_SIZE),
          panel.grid.major.y = element_line(color = "gray90", size = 0.5),
          strip.background = element_blank()) 
  
  # Impact by Region -----------------------------------------------------------
  p_region <- coef_df %>% 
    dplyr::filter(type == "wb_region") %>%
    dplyr::mutate(keyword_en_nwline = keyword_en_nwline %>% fct_rev()) %>%
    ggplot(aes(xmin = p025,
               xmax = p975,
               x = b,
               y = wb_region,
               color = keyword_type)) +
    geom_point(size = COEF_POINT_SIZE) +
    geom_linerange(size = COEF_LINE_SIZE) +
    geom_vline(xintercept = 0, 
               color = "black") +
    theme_classic() +
    labs(color = "Category",
         x = "Coefficient (+/- 95% CI)",
         y = NULL,
         title = "A. Results by Geographic Region") +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.text.y = element_text(color = "black", size = COEF_AXIS_Y_TEXT_SIZE),
          axis.title.x = element_text(size = AXIS_X_TITLE_SIZE),
          panel.grid.major.y = element_line(color = "gray90", size = 0.5),
          strip.background = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", size = 14),
          strip.text = element_text(face = "bold", size = 12)) +
    facet_wrap(~keyword_en_nwline)
  
  # Impact by Income Group -----------------------------------------------------
  p_income <- coef_df %>% 
    dplyr::filter(type == "income") %>%
    dplyr::mutate(keyword_en_nwline = keyword_en_nwline %>% fct_rev()) %>%
    dplyr::mutate(income = income %>% factor(levels = c("Low income",
                                                        "Lower middle income",
                                                        "Upper middle income",
                                                        "High income")) %>% fct_rev()) %>%
    ggplot(aes(xmin = p025,
               xmax = p975,
               x = b,
               y = income,
               color = keyword_type)) +
    geom_point(size = COEF_POINT_SIZE) +
    geom_linerange(size = COEF_LINE_SIZE) +
    geom_vline(xintercept = 0, 
               color = "black") +
    theme_classic() +
    labs(color = "Category",
         x = "Coefficient (+/- 95% CI)",
         y = NULL,
         title = "B. Results by Income Group") +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          axis.text.y = element_text(color = "black", size = COEF_AXIS_Y_TEXT_SIZE),
          axis.title.x = element_text(size = AXIS_X_TITLE_SIZE),
          panel.grid.major.y = element_line(color = "gray90", size = 0.5),
          strip.background = element_blank(),
          plot.title = element_text(face = "bold", size = 14),
          plot.title.position = "plot",
          strip.text = element_text(face = "bold", size = 12)) +
    facet_wrap(~keyword_en_nwline)
  
  # Interaction Figures --------------------------------------------------------
  p_interact <- coef_df %>% 
    dplyr::filter(type != "Overall") %>%
    dplyr::filter(variable != "Post Policy X Pandemic") %>%
    #dplyr::filter(type == type_i) %>%
    ggplot(aes(xmin = p025,
               xmax = p975,
               x = b,
               y = variable,
               color = keyword_type)) +
    geom_point(size = COEF_POINT_SIZE) +
    geom_linerange(size = COEF_LINE_SIZE) +
    geom_vline(xintercept = 0, 
               color = "black") +
    labs(x = "Coefficient (+/- 95% CI)",
         title = "A. Diff-in-Diff Results: Heterogeneity of Impacts of Containment Policies on Search Interest by Levels of Economic Support, Containment\nPolicy Restrictions, and per capita GDP",
         y = NULL,
         color = "Category") +
    theme_classic() +
    theme(strip.text = element_text(face = "bold",
                                    size = STRIP_TEXT_SIZE),
          axis.text.y = element_text(color = "black", size = COEF_AXIS_Y_TEXT_SIZE),
          axis.title.x = element_text(size = AXIS_X_TITLE_SIZE),
          legend.position = "bottom",
          legend.text=element_text(size=LEGEND_TEXT_SIZE),
          legend.title=element_text(size=LEGEND_TEXT_SIZE),
          panel.background = element_rect(fill = "gray95"),
          strip.background = element_blank(),
          plot.title = element_text(face = "bold", size = 14),
          plot.title.position = "plot") +
    facet_wrap(~keyword_en_newline,
               scales = "free_x")
  
  # Interactions Maps ----------------------------------------------------------
  #### Prep Data
  world_sp <- ne_countries(type = "countries", scale=50)
  
  world_sp <- world_sp[world_sp$continent != "Antarctica",]
  
  world_sp@data <- world_sp@data %>%
    dplyr::select(name, continent, iso_a2) %>%
    dplyr::rename(geo = iso_a2)
  
  world_sp$geo <- world_sp$geo %>% as.character()
  
  #### Merge in Data
  df_map <- readRDS(file.path(data_dir, "google_trends", "FinalData", "results", 
                              paste0("did_pooled_data_",days_thresh,".Rds")))
  
  df_map <- df_map %>%
    dplyr::mutate(ln_gdp_pc = ln_gdp_pc %>% as.numeric,
                  EconomicSupportIndex_max = EconomicSupportIndex_max %>% as.numeric,
                  StringencyIndex_max = StringencyIndex_max %>% as.numeric,
                  gm_avg_min = gm_avg_min %>% as.numeric) %>%
    distinct(geo, ln_gdp_pc, EconomicSupportIndex_max, StringencyIndex_max, gm_avg_min)
  
  world_sp <- merge(world_sp, df_map, by = "geo", all.x = T, all.y = F)
  
  #### Tidy
  world_sp$id <- row.names(world_sp)
  world_sp_tidy <- tidy(world_sp)
  world_sp_tidy <- merge(world_sp_tidy, world_sp@data, by = "id")
  
  world_sp_tidy_long <- world_sp_tidy %>%
    pivot_longer(cols = c(ln_gdp_pc, 
                          StringencyIndex_max,
                          EconomicSupportIndex_max,
                          gm_avg_min),
                 names_to = "var",
                 values_to = "value") %>%
    dplyr::mutate(var = case_when(
      var == "ln_gdp_pc" ~ "GDP (per capita)",
      var == "StringencyIndex_max" ~ "Stringency Index",
      var == "EconomicSupportIndex_max" ~ "Economic Support Index",
      var == "gm_avg_min" ~ "Mobility Reduction",
      TRUE ~ var
    ) %>%
      fct_rev)
  
  var_i <- "GDP, Per Capita (logged)"
  
  #df_i <- world_sp_tidy_long[world_sp_tidy_long$var %in% var_i,]
  p_map <- ggplot() +
    geom_polygon(data = world_sp_tidy_long,
                 aes(x = long, y = lat, group = group,
                     fill = value)) +
    theme_void() +
    coord_quickmap() +
    labs(fill = "Z-Score",
         title = "B. Maps of Variables",
         color = NULL) +
    scale_fill_gradient2(low = "firebrick", mid = "bisque", high = "forestgreen",
                         midpoint = -2) +
    scale_color_manual(values = c(NA_COLOR)) +
    theme(strip.text = element_text(face = "bold", size = 14),
          plot.title = element_text(face = "bold", hjust = 0, size = 14),
          plot.title.position = "plot") +
    facet_wrap(~var)
  #ggsave(p_map, filename = file.path(paper_figures, "interact_var_map.png"),
  #       height = 7, width = 8)
  
  
  
  # type_i = "stringency_index"
  # 
  # p_interact <- list()
  # for(type_i in c("stringency_index",
  #                 "mobility_reduction")){
  # 
  #   p_interact[[type_i]] <- coef_df %>%
  #     dplyr::filter(type != "Overall") %>%
  #     dplyr::filter(type == type_i) %>%
  #     ggplot(aes(xmin = p025,
  #                xmax = p975,
  #                x = b,
  #                y = variable,
  #                color = keyword_type)) +
  #     geom_point(size = COEF_POINT_SIZE) +
  #     geom_linerange(size = COEF_LINE_SIZE) +
  #     geom_vline(xintercept = 0,
  #                color = "black") +
  #     labs(x = "Coefficient (+/- 95% CI)",
  #          y = NULL,
  #          color = "Category") +
  #     theme_classic() +
  #     theme(strip.text = element_text(face = "bold",
  #                                     size = STRIP_TEXT_SIZE),
  #           axis.text.y = element_text(color = "black", size = COEF_AXIS_Y_TEXT_SIZE),
  #           axis.title.x = element_text(size = AXIS_X_TITLE_SIZE),
  #           legend.position = "bottom",
  #           legend.text=element_text(size=LEGEND_TEXT_SIZE),
  #           legend.title=element_text(size=LEGEND_TEXT_SIZE),
  #           panel.background = element_rect(fill = "gray95"),
  #           strip.background = element_blank()) +
  #     facet_wrap(~keyword_en_newline,
  #                scales = "free_x")
  # 
  # }
  
  # Append Figures -------------------------------------------------------------
  #### Overall
  title_theme <- theme(plot.title = element_text(face = "bold", 
                                                 color = "black",
                                                 size = PLOT_TITLE_SIZE),
                       plot.title.position = "plot")
  
  p_overall_trends <- ggarrange(p_trends + title_theme, 
                                p_overall + title_theme,
                                nrow = 1,
                                widths = c(0.6, 0.4))
  
  ggsave(p_overall_trends, filename = file.path(paper_figures, paste0("did_overall_",days_thresh,".png")),
         height = 8, width = 12)
  
  #### Coutry Groups
  p_income_region <- ggarrange(p_region,
                               p_income, 
                               ncol = 1,
                               heights = c(0.6, 0.4),
                               common.legend = T,
                               legend = "bottom")
  
  ggsave(p_income + labs(title = NULL), filename = file.path(paper_figures, paste0("did_income_",days_thresh,".png")),
         height = 7, width = 14)
  
  ggsave(p_region + labs(title = NULL), filename = file.path(paper_figures, paste0("did_region_",days_thresh,".png")),
         height = 8.5, width = 14)
  
  ggsave(p_income_region, filename = file.path(paper_figures, paste0("did_countrygroups_",days_thresh,".png")),
         height = 14, width = 14)
  
  #### Interact
  p_interact_map <- ggarrange(p_interact,
                              p_map,
                              ncol = 1,
                              heights = c(0.6, 0.4))
  
  ggsave(p_interact_map, filename = file.path(paper_figures, paste0("did_interact_map_",days_thresh,".png")),
         height = 15, width = 13)
  
  ggsave(p_interact + labs(title = NULL), filename = file.path(paper_figures, paste0("did_interact_",days_thresh,".png")),
         height = 8, width = 13)
  
  
  ##### All together
  
  
  # Figure: Contaiment Restrictiveness - Strigency Index -------------------------
  #ggsave(p_interact$stringency_index, filename = file.path(paper_figures, 
  #                                                         paste0("did_pooled_","strigency_",days_thresh,".png")),
  #       height = 8, width = 13)
  
}


