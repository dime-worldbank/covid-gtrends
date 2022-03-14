# Merge Correlation Data with Other Data

#### Parameters
begin_day_i <- "2020-02-01"

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                            "global_with_refstate",
                            paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",begin_day_i,".Rds")))

# gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
#                                 "global_with_refstate",
#                                 paste0("gl_gtrends_ref","US","_adj_cases_cor_since_",begin_day_i,".Rds")))

policy_data <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

# Prep Policy Data -------------------------------------------------------------
policy_data_sum <- policy_data %>%
  group_by(CountryCode) %>%
  dplyr::summarise(H2_Testing.policy = max(H2_Testing.policy, na.rm=T)) %>%
  dplyr::rename(iso3 = CountryCode) %>%
  dplyr::mutate(geo =  countrycode(iso3, origin = "iso3c", destination = "iso2c"))

# Merge ------------------------------------------------------------------------
cor_df <- merge(cor_df, policy_data_sum, by = "geo")

# Figures ----------------------------------------------------------------------
cor_df <- cor_df %>%
  filter(type %in% "Cases") 

cor_df$test_policy <- NA
cor_df$test_policy[cor_df$H2_Testing.policy > 0] <- "0"
cor_df$test_policy[cor_df$H2_Testing.policy > 1] <- "1"
cor_df$test_policy[cor_df$H2_Testing.policy > 2] <- "2"

#cor_df <- cor_df[!(cor_df$lag %in% c(-21, 21)),]
#cor_df_sub <- cor_df[cor_df$zscore > 1.1,]
#cor_df_sub <- cor_df[cor_df$keyword_en %in% "loss of smell",]
#cor_df_sub <- cor_df_sub[cor_df_sub$lag > -21,]

cor_df_sub$lag[cor_df_sub$test_policy %in% "0"] %>% summary()
cor_df_sub$lag[cor_df_sub$test_policy %in% "1"] %>% summary()
cor_df_sub$lag[cor_df_sub$test_policy %in% "2"] %>% summary()

cor_df_sub$lag[cor_df_sub$test_policy %in% "0"] %>% length()
cor_df_sub$lag[cor_df_sub$test_policy %in% "1"] %>% length()
cor_df_sub$lag[cor_df_sub$test_policy %in% "2"] %>% length()

cor(cor_df_sub$lag, cor_df_sub$H2_Testing.policy)

cor_df_sub$cor[cor_df_sub$test_policy %in% "0"] %>% summary()
#cor_df_sub$cor[cor_df_sub$test_policy %in% "1"] %>% summary()
cor_df_sub$cor[cor_df_sub$test_policy %in% "2"] %>% summary()

cor_df_sub %>%
  mutate(test_policy = test_policy %>% as.numeric) %>%
  ggplot(aes(x = test_policy, y = cor)) +
  geom_point() +
  geom_smooth()

cor_df %>%
  filter(keyword_en %in% "loss of smell",
         zscore > 1.5,
         cor > 0.2) %>%
  ggplot() +
  geom_dotplot(aes(x = test_policy,
                   y = lag,
                   fill = "=  One Country"),
               binaxis = "y", 
               stackdir = "center",
               dotsize = 5,
               binwidth = .1,
               color = "palegreen4") 



