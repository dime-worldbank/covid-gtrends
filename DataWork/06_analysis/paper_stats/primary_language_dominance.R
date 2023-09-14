languages <- readRDS(file.path(prim_lang_dir, 
                               "FinalData",
                               "country_language.Rds"))

## Proportion of countries with one language
languages_1 <- languages %>%
  dplyr::filter(is.na(language_id_2))

languages_2more <- languages %>%
  dplyr::filter(!is.na(language_id_2))

nrow(languages_1) / nrow(languages)

## Dominance
scnd_highest <- function(x){
  rev(sort(x))[2]
}

languages_2more_prop_greater <- languages_2more %>%
  select(geo, contains("hits_mean_id_")) %>%
  pivot_longer(-geo) %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(geo) %>%
  dplyr::summarise(hits_max = max(value),
                   hits_2nd = scnd_highest(value)) %>%
  ungroup() %>%
  mutate(perc_greater = (hits_max - hits_2nd) / hits_2nd*100) %>%
  dplyr::filter(perc_greater != Inf)
  

languages_2more_prop_greater$perc_greater %>% summary()


