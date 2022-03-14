

SCRAPED_FILES <- file.path("~/Desktop", "PORT") %>%
  list.files(full.names = T,
             pattern = "*.Rds",
             recursive = T)

SCAPED_FILE_i = SCRAPED_FILES[1]
for(SCAPED_FILE_i in SCRAPED_FILES){
  
  name_i <- SCAPED_FILE_i %>% str_replace_all(".*/", "")
  
  file.rename(from = SCAPED_FILE_i,
              to = file.path(gtrends_dir, "RawData", "timeseries", name_i))
}

file.path(gtrends_dir, "RawData", "timeseries") %>%
  list.files(pattern = "*.Rds") %>%
  length()




