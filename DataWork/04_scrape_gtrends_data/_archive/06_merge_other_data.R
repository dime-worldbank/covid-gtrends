# Merge Other Data

gtrends_otherdata_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                          "gtrends_full_timeseries", "gtrends_otherdata_varclean.Rds"))

otherdata_df <- gtrends_otherdata_df %>%
  dplyr::select(geo, population, gdp_pc, per_pop_using_internet, mobile_cell_sub_per100, working_age_pop,
                urban_pop, business_ease_index, cases_total, death_total) %>%
  distinct()


# Load Data --------------------------------------------------------------------
begin_day <- c("2020-02-01",
               "2020-03-01",
               "2020-04-01",
               "2020-05-01",
               "2020-06-01",
               "2020-07-01",
               "2020-08-01",
               "2020-09-01",
               "2020-10-01",
               "2020-11-01",
               "2020-12-01")

for(begin_day_i in begin_day){
  
  gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                  "gtrends_full_timeseries", "correlation_datasets",
                                  paste0("gtrends_since",begin_day_i,".Rds")))

  cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                              "gtrends_full_timeseries", "correlation_datasets",
                              paste0("correlations_gtrends_since",begin_day_i,".Rds")))
  
  #gtrends_df <- merge(gtrends_df, otherdata_df, by = "geo", all.x=T, all.y=F)
  cor_df     <- merge(cor_df,     otherdata_df, by = "geo", all.x=T, all.y=F)
  
  saveRDS(gtrends_df,
          file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries", "correlation_datasets",
                    paste0("gtrends_otherdata_varclean_since",begin_day_i,".Rds")))
  
  saveRDS(cor_df,
          file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                    "gtrends_full_timeseries", "correlation_datasets",
                    paste0("correlations_gtrends_otherdata_varclean_since",begin_day_i,".Rds")))
}




