# Example Trends

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries",
                                "correlation_datasets",
                                "correlations_gtrends_since2020-01-01_until2021-12-31_symptoms.Rds")) # 2021-09-30

for(keyword_en_i in c("loss of smell", "loss of taste", "covid symptoms")){
  
  gtrends_losssmell_df <- gtrends_df %>%
    dplyr::filter(keyword_en %in% keyword_en_i,
                  type %in% "Cases") %>%
    mutate(income = income %>% factor(levels = c("High income",
                                                 "Low income",
                                                 "Lower middle income",
                                                 "Upper middle income")))
  
  gtrends_losssmell_df$lag <- as.numeric(gtrends_losssmell_df$lag)
  gtrends_losssmell_df$gdp_pc_ln <- log(gtrends_losssmell_df$gdp_pc)
  
  # factor(income)
  # gdp_pc_ln
  lm1_cor <- lm(cor_nolag ~ log(cases_total), data = gtrends_losssmell_df)
  lm2_cor <- lm(cor_nolag ~ per_pop_using_internet, data = gtrends_losssmell_df)
  lm3_cor <- lm(cor_nolag ~ mobile_cell_sub_per100, data = gtrends_losssmell_df)
  lm4_cor <- lm(cor_nolag ~ gdp_pc_ln, data = gtrends_losssmell_df)
  #lm4_cor <- lm(cor_nolag ~ factor(h2_testing_policy_median), data = gtrends_losssmell_df)
  lm5_cor <- lm(cor_nolag ~ log(cases_total) + per_pop_using_internet + mobile_cell_sub_per100 + gdp_pc_ln, data = gtrends_losssmell_df)
  summary(lm4_cor)
  
  lm1_lag <- lm(lag ~ log(cases_total), data = gtrends_losssmell_df)
  lm2_lag <- lm(lag ~ per_pop_using_internet, data = gtrends_losssmell_df)
  lm3_lag <- lm(lag ~ mobile_cell_sub_per100, data = gtrends_losssmell_df)
  lm4_lag <- lm(lag ~ gdp_pc_ln, data = gtrends_losssmell_df)
  #lm4_lag <- lm(lag ~ factor(h2_testing_policy_max), data = gtrends_losssmell_df)
  lm5_lag <- lm(lag ~ log(cases_total) + per_pop_using_internet + mobile_cell_sub_per100 + gdp_pc_ln, data = gtrends_losssmell_df)
  summary(lm4_lag)
  
  # Correlation between loss of smell search interest and COVID-19
  keyword_underscore <- keyword_en_i %>% 
    str_replace_all(" ", "_")
  
  stargazer(lm1_cor,
            lm2_cor,
            lm3_cor,
            lm4_cor,
            lm5_cor,
            lm1_lag,
            lm2_lag,
            lm3_lag,
            lm4_lag,
            lm5_lag,
            dep.var.labels.include = T,
            dep.var.labels = c("Correlation",
                               "Best Lag"),
            covariate.labels = c("Total COVID-19 Cases, log",
                                 "Per Pop. Using Internet",
                                 "Mobile Cell Sub. per 100",
                                 "GDP Per Cap, Log"),
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            out = file.path(paper_tables, 
                            paste0("lm_cor_",keyword_underscore,".tex")))
  
}

