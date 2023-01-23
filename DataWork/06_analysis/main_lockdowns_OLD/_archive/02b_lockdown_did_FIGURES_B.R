# Lockdown Difference-in-Difference Analysis

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                                "did_results.Rds"))

# Prep Data --------------------------------------------------------------------
results_df <- results_df %>%
  
  # Only want DiD variable
  dplyr::filter(variable == "days_since_lockdown_min_yearcurrent_post_X_year2020") %>%
  
  # Sort regions. Global first, then alphabetical
  dplyr::mutate(region = region %>% 
                  factor(levels = c("Global",
                                    "East Asia & Pacific",
                                    "Europe & Central Asia",
                                    "Latin America & Caribbean",
                                    "Middle East & North Africa",
                                    "North America",
                                    "South Asia",
                                    "Sub-Saharan Africa") %>% rev())) %>%

  # Group keywords into categories
  dplyr::mutate(category = case_when(
    keyword %in% c("boredom", 
                   "anxiety",
                   "anxiety attack",
                   "anxiety symptoms",
                   "overwhelmed", # panic
                   "hysteria",
                   "suicide",
                   "insomnia",
                   "social isolation",
                   #"symptoms of panic attack",
                   "lonely",
                   "loneliness",
                   "panic attack") ~ "Mental Health",
    #keyword %in% c(#"online therapist",
    #"therapist near me",
    #"deep breathing",
    #"body scan meditation") ~ "Mental Health\nTreatments",
    keyword %in% c("unemployment", 
                   "unemployment insurance",
                   "abuse",
                   "divorce") ~ "Social and\nEconomic\nConsequences",
    keyword %in% c("stay at home",
                   "social distance") ~ "Social Distancing"
  )) %>%
  
  dplyr::mutate(keyword = tools::toTitleCase(keyword))

results_df$keyword <- fct_reorder(results_df$keyword, results_df$category, min) 

# Figures ----------------------------------------------------------------------
results_df <- results_df %>%
  dplyr::mutate(sig = (p025 > 0 & p975 > 0) | (p025 < 0 & p975 < 0))

# Can facet over type
p <- results_df %>%
  #dplyr::filter(category %in% "mental health") %>% 
  #dplyr::filter(region != "Global") %>%
  #dplyr::filter(!is.na(category)) %>% 
  ggplot(aes(xmin = p025,
             xmax = p975,
             x = b,
             y = region,
             color = category)) +
  geom_vline(xintercept = 0) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_linerange(position = position_dodge(width = 0.9)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  labs(y = NULL,
       x = "Coefficient (+/-95% CI)",
       color = "Category") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", color = "black")) +
  facet_wrap(~keyword,
             ncol = 4,
             scales = "free_x") 

ggsave(p, filename = file.path(paper_figures, "did_results.png"),
       height = 10, width = 14)

