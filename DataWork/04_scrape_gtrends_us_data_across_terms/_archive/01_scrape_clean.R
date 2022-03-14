# Compare search interest across search terms in US

dir.create(file.path(gtrends_dir, "RawData", "search_interest_across_terms_us"))

for(time_span in c("2020-12-01_2021-05-31",
                   "2021-03-01_2021-05-31",
                   "2020-12-01_2021-09-30",
                   "2021-03-01_2021-09-30",
                   "2020-12-01_2021-12-31")){
  print(time_span)
  
  out <- gtrends(keyword = c("covid microchip",
                             "covid vaccine cause infertility",
                             "covid vaccine change dna",
                             "is the covid vaccine the mark of the beast",
                             "ivermectin"),
                 geo = c("US"),
                 time = time_span %>% str_replace_all("_", " "),
                 onlyInterest = F)
  
  saveRDS(out, file.path(gtrends_dir, "RawData", "search_interest_across_terms_us", 
                         paste0("gtrends_missinfo_", time_span, ".Rds")))
}




