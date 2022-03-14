# Summary Stats by Income Group

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", 
                                "gtrends_otherdata_varclean_complete_vaccine.Rds"))

geo_distinct <- gtrends_df %>%
  distinct(geo, .keep_all = T) %>%
  dplyr::filter(!is.na(gdp_pc))

#med_gdp <- geo_distinct$gdp_pc %>% median()
#geo_distinct$geo

#gtrends_df <- gtrends_df %>%
#  dplyr::mutate(gdp_group = ifelse())

replace_inf <- function(x){
  x[x %in% Inf] <- NA
  return(x)
}

gtrends_df <- gtrends_df %>%
  filter(!is.na(income)) %>%
  mutate(income = income %>% factor(levels = c("Low income",
                                               "Lower middle income",
                                               "Upper middle income",
                                               "High income")))

gtrends_df_90days_policy <- gtrends_df %>%
  dplyr::filter(!is.na(days_since_c_policy)) %>%
  dplyr::mutate(days_since_c_policy %in% 1:90) %>%
  group_by(geo, income) %>%
  dplyr::summarise(gm_retail = min(gmobility_retail_and_recreation_percent_change_from_baseline, na.rm = T),
                   gm_grocery = min(gmobility_grocery_and_pharmacy_percent_change_from_baseline, na.rm = T),
                   gm_parks = min(gmobility_parks_percent_change_from_baseline, na.rm = T),
                   gm_transit = min(gmobility_transit_stations_percent_change_from_baseline, na.rm = T),
                   gm_work = min(gmobility_workplaces_percent_change_from_baseline, na.rm = T),
                   StringencyIndex = max(StringencyIndex, na.rm = T),
                   EconomicSupportIndex = max(EconomicSupportIndex, na.rm = T)) %>%
  dplyr::mutate_all(replace_inf) %>%
  dplyr::mutate(gm_avg = (gm_retail + gm_grocery + gm_parks + gm_transit + gm_work)/5) 

gtrends_df_alltime <- gtrends_df %>%
  group_by(geo, income) %>%
  dplyr::summarise(cases_total = max(cases_total, na.rm = T),
                   total_vaccinations_per_hundred = max(total_vaccinations_per_hundred, na.rm = T)) %>%
  dplyr::mutate_all(replace_inf)

gtrends_df_vars <- bind_rows(gtrends_df_90days_policy,
                             gtrends_df_alltime) %>%
  pivot_longer(cols = -c(geo, income))

gtrends_df_vars %>%
  ggplot(aes(y = income,
             x = value,
             color = income)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width=0.3,
                                             dodge.width = 0.85),
             pch = 21,
             size = 1.5, #  0.9, # 0.7
             stroke = 0.2, # 0.1
             alpha = 0.75,
             fill = "firebrick") +
  scale_color_manual(values = rep("black", 4)) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~name,
             scales = "free_x")



gtrends_df_90days_policy

gtrends_df$days_since_c_policy

gtrends_df$gmobility_workplaces_percent_change_from_baseline

gtrends_df$cases_total[gtrends_df$geo %in% "US"]
StringencyIndex
cases_total
EconomicSupportIndex
total_vaccinations_per_hundred

gmobility_retail_and_recreation_percent_change_from_baseline 
gmobility_grocery_and_pharmacy_percent_change_from_baseline 
gmobility_parks_percent_change_from_baseline 
gmobility_transit_stations_percent_change_from_baseline 
gmobility_workplaces_percent_change_from_baseline

gtrends_df$days_since_c_policy