# Days since events

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "gtrends_full_timeseries", "gtrends_otherdata_varclean_complete_vaccine.Rds"))

# Add events -------------------------------------------------------------------
gtrends_df$days_since_jj_paused <- difftime(gtrends_df$date, ymd("2021-04-13"), 
                                            units = "days") %>%
  as.numeric()

gtrends_df$days_since_astraz_suspended <- difftime(gtrends_df$date, ymd("2021-03-15"), 
                                                   units = "days") %>%
  as.numeric()

gtrends_df$days_since_jj_spoiled <- difftime(gtrends_df$date, ymd("2021-03-31"), 
                                             units = "days") %>%
  as.numeric()

gtrends_df$days_since_astraz_clots <- difftime(gtrends_df$date, ymd("2021-04-07"), 
                                               units = "days") %>%
  as.numeric()

gtrends_df$days_since_fda_approves <- difftime(gtrends_df$date, ymd("2021-08-23"), 
                                               units = "days") %>%
  as.numeric()

gtrends_df$days_since_delta <- difftime(gtrends_df$date, ymd("2021-06-30"), 
                                        units = "days") %>%
  as.numeric()

gtrends_df$days_since_vacc_mandate <- difftime(gtrends_df$date, ymd("2021-09-09"), 
                                               units = "days") %>%
  as.numeric()

THRESH <- 90

gtrends_df <- gtrends_df %>%
  group_by(keyword_en, country) %>%
  dplyr::mutate(hits_ma7_sd = sd(hits_ma7, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(hits_ma7_sd > 0)

for(var in c("days_since_jj_paused",
             "days_since_astraz_suspended",
             "days_since_jj_spoiled",
             "days_since_astraz_clots",
             "days_since_fda_approves",
             "days_since_delta",
             "days_since_vacc_mandate")){
  gtrends_df$days_since_var <- gtrends_df[[var]]
  
  for(word_i in unique(gtrends_df$keyword_en)){
    p <- gtrends_df %>%
      dplyr::filter(!is.na(days_since_var)) %>%
      dplyr::filter(keyword_en %in% word_i) %>%
      dplyr::filter(abs(days_since_var) <= THRESH) %>%
      ggplot(aes(x = days_since_var,
                 y = hits_ma7)) +
      geom_line() +
      geom_vline(xintercept = 0,
                 color = "red") +
      facet_wrap(~country,
                 scales = "free_y")
    
    dir.create(file.path("~/Desktop", var))
    ggsave(p, filename = file.path("~/Desktop", 
                                   var,
                                   paste0(word_i, ".png")),
           height = 20,
           width = 20)
    
  }
}



