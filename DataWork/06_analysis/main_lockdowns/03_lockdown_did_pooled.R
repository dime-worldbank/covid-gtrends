# DiD: Pooled Results

## Parametere
COEF_AXIS_Y_TEXT_SIZE <- 14
COEF_POINT_SIZE <- 3.25
COEF_LINE_SIZE <- 1.25
STRIP_TEXT_SIZE <- 12
LEGEND_TEXT_SIZE <- 14
AXIS_X_TITLE_SIZE <- 14
PLOT_TITLE_SIZE <- 16

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
                        "suicide") ~ "Mental Health",
      
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
                        "wedding") ~ "Relationships\n&Family Planning",
      
      keyword_en %in% c("social distance",
                        "stay at home") ~ "Social Distancing"
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
for(days_thresh in c(30, 60, 90, 120)){
  
  df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
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
    ))
  
  # Load/Prep Regression Results -------------------------------------------------
  coef_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
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
                    fct_rev)
  
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
    dplyr::mutate(keyword_en = keyword_en %>% fct_rev()) %>%
    ggplot(aes(xmin = p025,
               xmax = p975,
               x = b,
               y = keyword_en,
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
    theme(legend.position = "none",
          axis.text.y = element_text(color = "black", size = COEF_AXIS_Y_TEXT_SIZE),
          axis.title.x = element_text(size = AXIS_X_TITLE_SIZE),
          panel.grid.major.y = element_line(color = "gray90", size = 0.5),
          strip.background = element_blank())
  
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
          strip.background = element_blank()) +
    facet_wrap(~keyword_en_newline,
               scales = "free_x")
  
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
  
  # Append Figures ---------------------------------------------------------------
  title_theme <- theme(plot.title = element_text(face = "bold", 
                                                 color = "black",
                                                 size = PLOT_TITLE_SIZE),
                       plot.title.position = "plot")
  
  p_top <- ggarrange(p_trends + title_theme, 
                     p_overall + title_theme,
                     nrow = 1,
                     widths = c(0.6, 0.4))
  p <- ggarrange(p_top, 
                 p_interact +
                   labs(title = "C. Diff-in-Diff Results: Heterogeneity of Impacts of Contaiment Policies on Search Interest by Levels of Economic\n Support, Contaiment Policy Restrictiveness, and per capita GDP") +
                   title_theme,
                 nrow = 2)
  
  MULT <- 1
  ggsave(p, filename = file.path(paper_figures, paste0("did_pooled_",days_thresh,".png")),
         height = 15*MULT, width = 13*MULT)
  # height = 15, width = 13
  
  # Figure: Contaiment Restrictiveness - Strigency Index -------------------------
  #ggsave(p_interact$stringency_index, filename = file.path(paper_figures, 
  #                                                         paste0("did_pooled_","strigency_",days_thresh,".png")),
  #       height = 8, width = 13)
  
}


