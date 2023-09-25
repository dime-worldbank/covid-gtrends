# Download WDI data

download_wdi_indicator <- function(indicator){
  df <- WDI(country = "all",
            indicator = indicator,
            start = 2019,
            end = 2019,
            extra = TRUE)
  
  #df$iso2c <- NULL
  df$capital <- NULL
  df$longitude <- NULL 
  df$latitude <- NULL
  
  return(df)
}

if(!file.exists(file.path(wdi_dir, "FinalData", "wdi_data.Rds"))){
  
  indicator_list <- c("SP.POP.TOTL", "NY.GDP.PCAP.KD", "IT.NET.USER.ZS", "IT.CEL.SETS.P2", 
                      "SP.POP.1564.TO", "SP.URB.TOTL.IN.ZS",
                      "IC.BUS.EASE.XQ")
  
  wdi_data <- lapply(indicator_list, download_wdi_indicator) %>%
    purrr::reduce(left_join, by = c("iso3c", "iso2c", "country", "year", "region", "income", "lending")) 
  
  wdi_clean_data <- wdi_data %>%
    dplyr::select(iso3c, country, year, region, income, lending, everything()) %>%
    filter(lending != "Aggregates") %>%
    dplyr::rename(geo                    = iso2c,
                  population             = SP.POP.TOTL,
                  gdp_pc                 = NY.GDP.PCAP.KD,
                  per_pop_using_internet = IT.NET.USER.ZS,
                  working_age_pop        = SP.POP.1564.TO,
                  urban_pop              = SP.URB.TOTL.IN.ZS,
                  mobile_cell_sub_per100 = IT.CEL.SETS.P2,
                  business_ease_index    = IC.BUS.EASE.XQ) %>%
    dplyr::mutate(geo = geo %>% toupper) %>%
    dplyr::select(-c(country, year, region, iso3c))
  
  saveRDS(wdi_clean_data, file.path(wdi_dir, "FinalData", "wdi_data.Rds"))
  
}


