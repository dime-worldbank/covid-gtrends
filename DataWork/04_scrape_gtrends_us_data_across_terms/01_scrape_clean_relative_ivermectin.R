# Compare search interest across search terms in US

dir.create(file.path(gtrends_dir, "RawData", "search_interest_refivermectin_across_terms_us"))

#"2021-03-01_2021-05-31",
#"2021-03-01_2021-09-30"
#"2020-12-01_2021-05-31"

VACCINE_KEYWORDS_notivermectin <- VACCINE_KEYWORDS[VACCINE_KEYWORDS != "ivermectin"]

for(time_span in c("2020-12-01_2021-09-30",
                   "2020-12-01_2021-12-31")){
  print(time_span)
  
  for(keyword_i in c(VACCINE_KEYWORDS_notivermectin)){
    
    PATH_OUT <- file.path(gtrends_dir, "RawData", "search_interest_refivermectin_across_terms_us", 
                          paste0("gtrends_missinfo_",keyword_i,"_", time_span, ".Rds"))
    
    if(!file.exists(PATH_OUT)){
      
      print(keyword_i)
      
      out <- gtrends(keyword = c(keyword_i,
                                 "ivermectin"),
                     geo = c("US"),
                     time = time_span %>% str_replace_all("_", " "),
                     onlyInterest = F)
      
      print(nrow(out$interest_over_time))
      
      saveRDS(out, PATH_OUT)
    }
  }
}





