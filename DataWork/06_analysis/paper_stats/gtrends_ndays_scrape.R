# Number of days can scrape in Google Trends for a given query
# 270

# Range 1 ----------------------------------------------------------------------
## Too long of time range for daily
out1 <- gtrends(keyword = "fever", 
               geo = "US",
               time = "2020-01-01 2020-09-27",
               onlyInterest = T,
               low_search_volume=T)
out1$interest_over_time %>% head()

out2 <- gtrends(keyword = "fever", 
               geo = "US",
               time = "2020-01-01 2020-09-26",
               onlyInterest = T,
               low_search_volume=T)
out2$interest_over_time %>% head()
nrow(out2$interest_over_time)

# Range 2 ----------------------------------------------------------------------
## Check other ranges
## Too long
out3 <- gtrends(keyword = "fever", 
                geo = "US",
                time = "2021-01-01 2021-09-28",
                onlyInterest = T,
                low_search_volume=T)
out3$interest_over_time %>% head()

out4 <- gtrends(keyword = "fever", 
                geo = "US",
                time = "2021-01-01 2021-09-27",
                onlyInterest = T,
                low_search_volume=T)
out4$interest_over_time %>% head()
nrow(out4$interest_over_time)

