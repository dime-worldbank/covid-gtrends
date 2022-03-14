# Relation between economic support & contaiment restriveness

df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                        "did_pooled_data.Rds"))

df <- df %>%
  dplyr::filter(pandemic_time %in% 1) %>%
  group_by(geo) %>%
  dplyr::summarise()



df$days_since_c_policy_yearcurrent %>% min()
