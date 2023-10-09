# Example Trends

# Daily ------------------------------------------------------------------------
gtrends_daily_df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                                      "gtrends_full_timeseries",
                                      "correlation_datasets",
                                      "correlations_gtrends_since2020-01-01_until2021-12-31_symptoms.Rds")) 

for(keyword_en_i in c("loss of smell", "loss of taste", "covid symptoms")){
  
  gtrends_daily_sub_df <- gtrends_daily_df %>%
    dplyr::filter(keyword_en %in% keyword_en_i,
                  type %in% "Cases") %>%
    mutate(income = income %>% factor(levels = c("High income",
                                                 "Low income",
                                                 "Lower middle income",
                                                 "Upper middle income")))
  
  gtrends_daily_sub_df$lag            <- as.numeric(gtrends_daily_sub_df$lag)
  gtrends_daily_sub_df$gdp_pc_ln      <- log(gtrends_daily_sub_df$gdp_pc)
  gtrends_daily_sub_df$ln_cases_total <- log(gtrends_daily_sub_df$cases_total + 1)
  
  gtrends_daily_sub_df <- gtrends_daily_sub_df %>%
    dplyr::filter(!is.na(cor_nolag))
  
  #### Correlation
  m1c <- lm(cor_nolag ~ ln_cases_total, data = gtrends_daily_sub_df)
  m2c <- lm(cor_nolag ~ per_pop_using_internet, data = gtrends_daily_sub_df)
  m3c <- lm(cor_nolag ~ mobile_cell_sub_per100, data = gtrends_daily_sub_df)
  m4c <- lm(cor_nolag ~ gdp_pc_ln, data = gtrends_daily_sub_df)
  m5c <- lm(cor_nolag ~ factor(income), data = gtrends_daily_sub_df)
  m6c <- lm(cor_nolag ~ factor(wb_region), data = gtrends_daily_sub_df)
  m7c <- lm(cor_nolag ~ ln_cases_total + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_daily_sub_df)
  m8c <- lm(cor_nolag ~ ln_cases_total + 
              per_pop_using_internet +
              mobile_cell_sub_per100 + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_daily_sub_df)
  
  #### Lag
  m1l <- lm(lag ~ ln_cases_total, data = gtrends_daily_sub_df)
  m2l <- lm(lag ~ per_pop_using_internet, data = gtrends_daily_sub_df)
  m3l <- lm(lag ~ mobile_cell_sub_per100, data = gtrends_daily_sub_df)
  m4l <- lm(lag ~ gdp_pc_ln, data = gtrends_daily_sub_df)
  m5l <- lm(lag ~ factor(income), data = gtrends_daily_sub_df)
  m6l <- lm(lag ~ factor(wb_region), data = gtrends_daily_sub_df)
  m7l <- lm(lag ~ ln_cases_total + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_daily_sub_df)
  m8l <- lm(lag ~ ln_cases_total + 
              per_pop_using_internet +
              mobile_cell_sub_per100 + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_daily_sub_df)
  
  # Correlation between loss of smell search interest and COVID-19
  keyword_underscore <- keyword_en_i %>% 
    str_replace_all(" ", "_")
  
  stargazer(m1c,
            m2c,
            m3c,
            m4c,
            m5c,
            m6c,
            m7c,
            m8c,
            dep.var.labels.include = T,
            dep.var.labels = c("Correlation"),
            covariate.labels = c("Total COVID-19 Cases, log",
                                 "Per Pop. Using Internet",
                                 "Mobile Cell Sub. per 100",
                                 "GDP Per Cap, Log",
                                 "Low Income",
                                 "Lower Middle Income",
                                 "Upper Middle Income",
                                 "Europe and Central Asia",
                                 "Latin America and Caribbean",
                                 "Middle East and North Africa",
                                 "North America",
                                 "South Asia",
                                 "Sub-Saharan Africa"),
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            out = file.path(paper_tables, 
                            paste0("lm_cor_daily_",keyword_underscore,".tex")))
  
  stargazer(m1l,
            m2l,
            m3l,
            m4l,
            m5l,
            m6l,
            m7l,
            m8l,
            dep.var.labels.include = T,
            dep.var.labels = c("Best Lag"),
            covariate.labels = c("Total COVID-19 Cases, log",
                                 "Per Pop. Using Internet",
                                 "Mobile Cell Sub. per 100",
                                 "GDP Per Cap, Log",
                                 "Low Income",
                                 "Lower Middle Income",
                                 "Upper Middle Income",
                                 "Europe and Central Asia",
                                 "Latin America and Caribbean",
                                 "Middle East and North Africa",
                                 "North America",
                                 "South Asia",
                                 "Sub-Saharan Africa"),
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            out = file.path(paper_tables, 
                            paste0("lm_lag_daily_",keyword_underscore,".tex")))
  
}

# Monthly ----------------------------------------------------------------------
gtrends_monthly_df <- readRDS(file.path(data_dir, "google_trends", "FinalData",
                                        "gtrends_full_timeseries", "correlation_datasets",
                                        "gtrends_monthly_correlations.Rds"))

gtrends_monthly_df <- gtrends_monthly_df %>%
  dplyr::filter(begin_date == ymd("2020-01-01"),
                end_date == ymd("2021-12-31"))

for(keyword_en_i in c("loss of smell", "loss of taste", "covid symptoms")){
  
  gtrends_monthly_sub_df <- gtrends_monthly_df %>%
    dplyr::filter(keyword_en %in% keyword_en_i) %>%
    mutate(income = income %>% factor(levels = c("High income",
                                                 "Low income",
                                                 "Lower middle income",
                                                 "Upper middle income")))
  
  gtrends_monthly_sub_df$gdp_pc_ln      <- log(gtrends_monthly_sub_df$gdp_pc)
  gtrends_monthly_sub_df$ln_cases_total <- log(gtrends_monthly_sub_df$cases_total + 1)
  
  #### Correlation
  m1c <- lm(cor_cases ~ ln_cases_total, data = gtrends_monthly_sub_df)
  m2c <- lm(cor_cases ~ per_pop_using_internet, data = gtrends_monthly_sub_df)
  m3c <- lm(cor_cases ~ mobile_cell_sub_per100, data = gtrends_monthly_sub_df)
  m4c <- lm(cor_cases ~ gdp_pc_ln, data = gtrends_monthly_sub_df)
  m5c <- lm(cor_cases ~ factor(income), data = gtrends_monthly_sub_df)
  m6c <- lm(cor_cases ~ factor(wb_region), data = gtrends_monthly_sub_df)
  m7c <- lm(cor_cases ~ ln_cases_total + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_monthly_sub_df)
  m8c <- lm(cor_cases ~ ln_cases_total + 
              per_pop_using_internet +
              mobile_cell_sub_per100 + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_monthly_sub_df)
  
  #### Lag
  m1e <- lm(cor_excess ~ ln_cases_total, data = gtrends_monthly_sub_df)
  m2e <- lm(cor_excess ~ per_pop_using_internet, data = gtrends_monthly_sub_df)
  m3e <- lm(cor_excess ~ mobile_cell_sub_per100, data = gtrends_monthly_sub_df)
  m4e <- lm(cor_excess ~ gdp_pc_ln, data = gtrends_monthly_sub_df)
  m5e <- lm(cor_excess ~ factor(income), data = gtrends_monthly_sub_df)
  m6e <- lm(cor_excess ~ factor(wb_region), data = gtrends_monthly_sub_df)
  m7e <- lm(cor_excess ~ ln_cases_total + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_monthly_sub_df)
  m8e <- lm(cor_excess ~ ln_cases_total + 
              per_pop_using_internet +
              mobile_cell_sub_per100 + 
              gdp_pc_ln +
              factor(income) +
              factor(wb_region), data = gtrends_monthly_sub_df)
  
  # Correlation between loss of smell search interest and COVID-19
  keyword_underscore <- keyword_en_i %>% 
    str_replace_all(" ", "_")
  
  stargazer(m1c,
            m2c,
            m3c,
            m4c,
            m5c,
            m6c,
            m7c,
            m8c,
            dep.var.labels.include = T,
            dep.var.labels = c("Correlation"),
            covariate.labels = c("Total COVID-19 Cases, log",
                                 "Per Pop. Using Internet",
                                 "Mobile Cell Sub. per 100",
                                 "GDP Per Cap, Log",
                                 "Low Income",
                                 "Lower Middle Income",
                                 "Upper Middle Income",
                                 "Europe and Central Asia",
                                 "Latin America and Caribbean",
                                 "Middle East and North Africa",
                                 "North America",
                                 "South Asia",
                                 "Sub-Saharan Africa"),
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            out = file.path(paper_tables, 
                            paste0("lm_cor_cases_monthly_",keyword_underscore,".tex")))
  
  stargazer(m1e,
            m2e,
            m3e,
            m4e,
            m5e,
            m6e,
            m7e,
            m8e,
            dep.var.labels.include = T,
            dep.var.labels = c("Correlation"),
            covariate.labels = c("Total COVID-19 Cases, log",
                                 "Per Pop. Using Internet",
                                 "Mobile Cell Sub. per 100",
                                 "GDP Per Cap, Log",
                                 "Low Income",
                                 "Lower Middle Income",
                                 "Upper Middle Income",
                                 "Europe and Central Asia",
                                 "Latin America and Caribbean",
                                 "Middle East and North Africa",
                                 "North America",
                                 "South Asia",
                                 "Sub-Saharan Africa"),
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            out = file.path(paper_tables, 
                            paste0("lm_cor_excess_monthly_",keyword_underscore,".tex")))
  
}


