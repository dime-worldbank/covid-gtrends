# GOAL
# this file creates new variables such as categories, case rate and death rate
# this file applies to the google trends with ref state, after merging it with pop data and covid cases

#user's file path
if(Sys.info()[["user"]] == "wb537287") dropbox_file_path <- "/Users/wb537287/Dropbox/COVID Social Media Analysis/"

#import data
gtrends_data <- 
  readRDS(file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_with_refstate_covid_pop.Rds"))

#ungroup data 
gtrends_data <- 
  gtrends_data %>% 
  ungroup() 

#create categories for keywords

gtrends_data <- 
  gtrends_data %>% 
  mutate(
    categories = case_when(
      keyword %in% 
        c("abuso", "abuso sexual", "desemprego",  "dívida", "Educação online",
          "psicologia", "terapia" ) ~ "consequences",
      keyword %in% 
        c("cama de hospital", "desinfetantes", "hidroxicloroquina", "cloroquina",
          "máscara facial", "termômetros", "ventiladores", "medicos", 
          "profissionais de saúde") ~ "resources",
      keyword %in% 
        c( "dificuldade ao respirar", "dor nos olhos", "falta de cheiro", 
           "febre", "febre alta valor","perda de olfato", "tosse",
           "sintomas do coronavirus", "teste de coronavírus", "anosmia", "cansaco") ~ "symptoms",
      keyword %in% 
        c("distância social","fique em casa", "lavar as mãos", "Isolamento social", 
          "isolamento vertical") ~ "prevention", 
      keyword %in% c("coronavirus", "covid", "covid19") ~ "virus", 
      keyword %in% 
        c("como tratar o coronavírus", "como tratar o coronavirus", "estou com falta de ar",
          "estou com febre", "eu fico em casa", "eu tenho coronavírus","eu tenho coronavirus",
          "perdi o olfato", "quais são os sintomas do coronavírus", "ajuda do coronavirus", 
          "quais sao os sintomas do coronavirus"
        ) ~ "in_1st_person"
    )
  ) 


# Create new variables: death rate, cases & deaths per 100,000

gtrends_data <- 
  gtrends_data %>% 
  mutate(
    case_rate = cases*100000/estimate_2018_state, 
    death_rate = deaths*100000/estimate_2018_state, 
    fatalities_per_case = deaths/cases
  )

gtrends_data <- 
  gtrends_data %>% 
  mutate(fatalities_per_case = if_else(is.na(fatalities_per_case), 0, fatalities_per_case))

# Create new variables for growth rate of cases and deaths 

gtrends_data <- 
  gtrends_data %>%
  group_by(geo) %>% 
  mutate(
    diff_date = as.numeric(date - lag(date)), 
    diff_growth_cases = cases - lag(cases), 
    diff_growth_deaths = deaths - lag(deaths), 
    growth_rate_cases = (diff_growth_cases/diff_date)*100/lag(cases),
    growth_rate_deaths = (diff_growth_deaths/diff_date)*100/lag(deaths),
    growth_rate_cases = if_else(is.na(growth_rate_cases), 0, growth_rate_cases), 
    growth_rate_deaths = if_else(is.na(growth_rate_deaths), 0, growth_rate_deaths), 
    growth_rate_cases = if_else(is.infinite(growth_rate_cases), 100, growth_rate_cases), 
    growth_rate_deaths = if_else(is.infinite(growth_rate_deaths), 100, growth_rate_deaths), 
  ) %>% 
  ungroup()

# Add date variables
gtrends_data <- 
  gtrends_data %>% 
  mutate(
    week_number = week(date), 
    day = day(date), 
    month = month(date), 
    wday = wday(date, label = TRUE)
  )


# Export -----------------------------------------------------------------------
saveRDS(gtrends_data, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_with_refstate_analysis.Rds"))
write.csv(gtrends_data, file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_with_refstate_analysis.csv"), row.names = F)
