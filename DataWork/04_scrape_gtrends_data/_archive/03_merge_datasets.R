# GOAL
## This file merges the Google trends data with adjusted hits (i.e. relative to another state) 
## with population data and data on covid-19 cases in Brazil.

#user's file path
if(Sys.info()[["user"]] == "wb537287") dropbox_file_path <- "/Users/wb537287/Dropbox/COVID Social Media Analysis/"

#load libraries
library(tidyverse)
library(gridExtra)
library(lubridate)
library(readstata13)
library(tidylog)

#import datasets

#population data
state_pop_data <- read.csv(file.path(dropbox_file_path, "Data/city_population/FinalData/brazil_state_pop.csv"))

#admin data on covid-19 cases
#covid_data <- read.csv(file.path(dropbox_file_path, "Data/brazil_admin_data/brazil_covid19_200705.csv"), encoding = "UTF-8")
covid_data <- read.csv(file.path(dropbox_file_path, "Data/brazil_admin_data/brazil_covid19_201008.csv"), encoding = "UTF-8")

#data of google searches at the national level
gtrends_data <- 
  readRDS(
    file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_with_refstate/br_gtrends_refBR-SP_adj.Rds")
  )

#To merge the different files, we need them to have the same state name. gtrends has acronyms ("BR-SP"), 
#population data has full name with accents (e.g. São Paulo), and covid data has reduced acronyms ("SP")

#We create a dataset with the different name types for each state
isocodes_all <- ISO_3166_2

isocodes_br <- 
  isocodes_all %>% 
  janitor::clean_names() %>% 
  filter(
    str_detect(code, "BR-"),
    code != "BR-FN"
  ) %>% 
  dplyr::rename(sub_code = code) %>% 
  dplyr::select(sub_code, name)

isocodes_br <- 
  isocodes_br %>% 
  mutate(sub_code_red = str_replace(sub_code, "BR-", "")) 
  
# we now add the 3 types to the google trends dataset
gtrends_data <- 
  gtrends_data %>% 
  left_join(isocodes_br, by = c("geo" = "sub_code"))

#Convert covid data's date to date var, and state names to character
covid_data <- 
  covid_data %>% 
  mutate(
    date = as.Date(date), 
    state = as.character(state)
  )

state_pop_data <- 
  state_pop_data %>% 
  mutate(State = as.character(State))

#We now merge the datasets on gtrends and covid
gtrends_data <- 
  gtrends_data %>% 
  full_join(covid_data, by = c("date", "sub_code_red" = "state"))

#we add the population data
gtrends_data <- 
  gtrends_data %>% 
  left_join(state_pop_data, by = c("name" = "State"))


# Export -----------------------------------------------------------------------
saveRDS(gtrends_data, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_with_refstate_covid_pop.Rds"))
write.csv(gtrends_data, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_with_refstate_covid_pop.csv"), row.names = F)
