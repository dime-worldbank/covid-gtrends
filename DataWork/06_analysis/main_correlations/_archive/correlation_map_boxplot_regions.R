# Exploring regional differences

# Boxplot showing: (1) loss of smell and (2) fever, and maps showing country
# correlations for each

# keywords_en_use <- c("loss of smell", 
#                      "loss of taste",
#                      "pneumonia",
#                      "fever",
#                      "ageusia",
#                      "anosmia",
#                      "i can't smell",
#                      "how to treat coronavirus")

keywords_en_use <- c("loss of smell",
                     "fever")

# Load Data --------------------------------------------------------------------
cor_1_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries",
                              "correlation_datasets",
                              "correlations_gtrends_since2020-01-01_until2021-07-31.Rds")) 

cor_1_df <- cor_1_df[!is.na(cor_1_df$wb_region),]
cor_1_df$wb_region <- cor_1_df$wb_region %>% fct_rev() # for ordering on ggplot

#cor_1_df$wb_region[gtrends_df$wb_region %in% "Europe & Central Asia"]       <- "Europe & Cent. Asia"
#cor_1_df$wb_region[gtrends_df$wb_region %in% "Latin America & Caribbean"]   <- "Lat. Am. & Caribbean"
#cor_1_df$wb_region[gtrends_df$wb_region %in% "Middle East & North Africa"] <- "Middle East & N Africa"

# Prep Data --------------------------------------------------------------------
cor_df <- cor_1_df %>%
  dplyr::filter(type %in% "Cases") %>%
  dplyr::filter(keyword_en %in% keywords_en_use) %>%
  
  # For ranking correlations show terms with highest median correlation
  group_by(keyword_en) %>%
  dplyr::mutate(cor_sort_all = quantile(cor_nolag, 0.95, na.rm=T)) %>%
  ungroup() %>%
  
  # Cleanup variables
  mutate(keyword_en = keyword_en %>% 
           tools::toTitleCase() %>% 
           str_replace_all("\\bi\\b", "I"))

# Maps =========================================================================

#### Prep Data
world_sp <- ne_countries(type = "countries", scale=50)

world_sp <- world_sp[world_sp$continent != "Antarctica",]

world_sp@data <- world_sp@data %>%
  dplyr::select(name, continent, iso_a2) %>%
  dplyr::rename(geo = iso_a2)

world_sp$geo <- world_sp$geo %>% as.character()

cor_map_df <- cor_df %>%
  dplyr::select(geo, keyword_en, cor_nolag) %>%
  dplyr::mutate(keyword_en = keyword_en %>% tolower %>% str_replace_all(" ", "_")) %>%
  tidyr::pivot_wider(names_from = keyword_en,
                     values_from = cor_nolag)

world_sp <- merge(world_sp, cor_map_df, by = "geo", all.x = T, all.y = F)

#### Tidy
world_sp$id <- row.names(world_sp)
world_sp_tidy <- tidy(world_sp)
world_sp_tidy <- merge(world_sp_tidy, world_sp@data, by = "id")

world_sp_tidy_long <- world_sp_tidy %>%
  pivot_longer(cols = c(fever, loss_of_smell),
               names_to = "search_term",
               values_to = "cor") %>%
  dplyr::mutate(search_term = case_when(
    search_term == "fever" ~ "Fever",
    search_term == "loss_of_smell" ~ "Loss of Smell",
    TRUE ~ search_term
  ) %>%
    fct_rev)

NA_COLOR <- "gray40"
p_map <- ggplot() +
  geom_polygon(data = world_sp_tidy_long[is.na(world_sp_tidy_long$cor),][1,] %>%
                 mutate(temp = "Low Search\nInterest"),
               aes(x = long, y = lat, group = group,
                   color = temp),
               fill = NA_COLOR) +
  geom_polygon(data = world_sp_tidy_long,
               aes(x = long, y = lat, group = group,
                   fill = cor)) +
  theme_void() +
  coord_quickmap() +
  labs(fill = "Correlation\nbetween\nsearch term and\nCOVID-19 cases",
       color = NULL) +
  scale_fill_gradient2(low = "firebrick", mid = "bisque", high = "forestgreen",
                       midpoint = 0,
                       limits = c(-1,1)) +
  scale_color_manual(values = c(NA_COLOR)) +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~search_term,
             nrow = 2) 
ggsave(p_map, filename = file.path(paper_figures, "cor_map.png"),
       height = 7, width = 8)

# Boxplot ======================================================================
ALPHA_VLINE <- 0.3

p_boxplot <- cor_df %>%
  ggplot(aes(x = cor_nolag,
             y = wb_region,
             fill = keyword_en)) +
  #geom_vline(xintercept = -1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = -0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = 0, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = 0.5, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_vline(xintercept = 1, size = 0.1, color = "black", alpha = ALPHA_VLINE) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitterdodge(jitter.width=0.3,
                                             dodge.width = 0.85),
             pch = 21,
             size = 0.9, # 0.7
             stroke = 0.2, # 0.1
             color = "black") +
  geom_vline(xintercept = 0, color = "firebrick1") +
  scale_fill_manual(values = c("dodgerblue2", "darkorange"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(fill = "Search terms",
       title = "B",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(p_boxplot, filename = file.path(paper_figures, "cor_by_region.png"),
       height = 4, width = 7)

# Append =======================================================================
p <- ggarrange(p_map, p_boxplot, ncol = 2,
               widths = c(0.65, 0.35))
ggsave(p, filename = file.path(paper_figures, "cor_by_region_map_boxplot.png"),
       height = 6, width = 16)





