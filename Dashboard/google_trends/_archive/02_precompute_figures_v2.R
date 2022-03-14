# Precompute Figures

# Settings
LINE_COR_FIG <- T
HIST_COR <- T
HIST_TIME_LAG <- T
MAP_COR <- F
CHANGE_TABLE <- F
CHANGE_MAP <- F
MAX_COR_HIST <- T

# Filepaths --------------------------------------------------------------------
DASHBOARD_PATH <- file.path(dropbox_file_path, "Data", "google_trends", 
                            "DashboardData", "data")

FIGURES_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData",
                          "precomputed_figures")

keyword <- "Loss of Smell" # "Loss of Smell"
cases_deaths <- "Cases"
continent <- "All" # "Asia" # 
sort_by <- "Name"

# Keyword List -----------------------------------------------------------------
gtrends_df <- readRDS(file.path(DASHBOARD_PATH, "gtrends.Rds"))
keyword_list <- unique(gtrends_df$keyword_en) %>% sort()
saveRDS(keyword_list, file.path(FIGURES_PATH, paste0("keyword_list",
                                                     ".Rds")))

for(keyword in c("Loss of Smell", keyword_list)){
  for(cases_deaths in c("Cases")){ #                         "Deaths"
    for(continent in c("All",
                       "Asia",
                       "Africa",
                       "Europe",
                       "South America",
                       "Oceania",
                       "North America")){
      for(sort_by in c("Name",
                       "Cases",
                       "Deaths",
                       "Correlation")){
        
        print(paste(keyword, "-",
                    cases_deaths, "-",
                    continent, "-",
                    sort_by))
        
        # Load Data ------------------------------------------------------------
        gtrends_df <- readRDS(file.path(DASHBOARD_PATH, "gtrends.Rds"))
        cor_df     <- readRDS(file.path(DASHBOARD_PATH, "correlations.Rds"))
        world      <- readRDS(file.path(DASHBOARD_PATH, "world.Rds"))
        
        # Subset ---------------------------------------------------------------
        if(continent != "All"){
          world <- world[world$continent %in% continent,]
          gtrends_df <- gtrends_df[gtrends_df$geo %in% world_sf$geo,]
          cor_df     <- cor_df[cor_df$geo %in% world_sf$geo,]
        }
        
        if(cases_deaths %in% c("Cases")){
          gtrends_df$covid_new <- gtrends_df$cases_new
          gtrends_df$covid_total <- gtrends_df$cases_total
          gtrends_df$cor_covidMA7_hitsMA7_max <- gtrends_df$cor_casesMA7_hitsMA7_max
          gtrends_df$cor_covidMA7_hitsMA7_lag <- gtrends_df$cor_casesMA7_hitsMA7_lag
          
          cor_df <- cor_df %>%
            dplyr::filter(type %in% "cases") 
        }
        
        if(cases_deaths %in% c("Deaths")){
          
          gtrends_df$covid_new <- gtrends_df$death_new
          gtrends_df$covid_total <- gtrends_df$cases_total
          gtrends_df$cor_covidMA7_hitsMA7_max <- gtrends_df$cor_deathMA7_hitsMA7_max
          gtrends_df$cor_covidMA7_hitsMA7_lag <- gtrends_df$cor_deathMA7_hitsMA7_lag
          
          cor_df <- cor_df %>%
            dplyr::filter(type %in% "death") 
        }
        
        #### Keyword
        cor_all_df <- cor_df
        
        gtrends_df <- gtrends_df[gtrends_df$keyword_en %in% keyword,]
        cor_df     <- cor_df[cor_df$keyword_en %in% keyword,]
        
        # 0. Best Time Lag -----------------------------------------------------
        #### Stat
        time_lag_best <- cor_df$lag %>% mean() %>% round()
        
        saveRDS(time_lag_best, file.path(FIGURES_PATH, paste0("stat_time_lag_best",
                                                              "_keyword", keyword,
                                                              "_cases_deaths", cases_deaths,
                                                              "_continent", continent,
                                                              ".Rds")))
        
        # 1. Line and Cor Figure -----------------------------------------------
        if(LINE_COR_FIG){
          
          #### Rescale for left/right y axes
          gtrends_sub_df <- gtrends_df %>%
            mutate(hits = hits_ma7) %>%
            group_by(geo) %>%
            mutate(hits = hits / max(hits, na.rm = T)) %>% # ensure max is 1 (for eg, for moving avg)
            mutate(hits = hits * max(covid_new)) %>%
            ungroup() %>%
            unique()
          
          if(nrow(gtrends_sub_df) %in% 0) next
          
          #### Title
          gtrends_sub_df$past_future <- ifelse(gtrends_sub_df$cor_covidMA7_hitsMA7_lag < 0, 
                                               "past", "future")

          gtrends_sub_df$title <- paste0("<b>", gtrends_sub_df$name, "</b><br>",
                                         "<b>", abs(gtrends_sub_df$cor_covidMA7_hitsMA7_lag),
                                         "</b> days into the ",
                                         gtrends_sub_df$past_future,
                                         "<br>Correlation: <b>",
                                         round(gtrends_sub_df$cor_covidMA7_hitsMA7_max, 2),
                                         "</b>")
          
          #### Sort
          if(sort_by %in% c("Cases", "Deaths")){
            gtrends_sub_df$title <- gtrends_sub_df$title %>% 
              as.factor() %>% 
              reorder(-gtrends_sub_df$covid_total)
          }
          
          if(sort_by %in% "Correlation"){
            gtrends_sub_df$title <- gtrends_sub_df$title %>% 
              as.character() %>%
              as.factor() %>% 
              reorder(-gtrends_sub_df$cor_covidMA7_hitsMA7_max)
          }
          
          
          #####
          gtrends_sub_df_lim <- gtrends_sub_df %>%
            arrange(date) %>%
            dplyr::select(title, Country, date, hits, covid_new)
          
          saveRDS(gtrends_sub_df_lim, file.path(FIGURES_PATH, paste0("fig_line_data",
                                                                     "_keyword", keyword,
                                                                     "_cases_deaths", cases_deaths,
                                                                     "_continent", continent,
                                                                     "_sort_by", sort_by,
                                                                     ".Rds")))
          
          
          gtrends_sub_df$week <- gtrends_sub_df$date %>% lubridate::week()
          
          p_line <- ggplot(gtrends_sub_df[gtrends_sub_df$geo %in% c("US", "BR"),], 
                           aes(x = date)) +
            geom_col(aes(y = covid_new, fill = paste("COVID-19", cases_deaths))) +
            geom_line(aes(y = hits, color = paste0("Search Popularity of ", keyword_en))) +
            facet_wrap(~title,
                       scales = "free_y",
                       ncol = 5) +
            scale_fill_manual(values = "orange1") +
            scale_color_manual(values = "green4") +
            labs(x = "", y = paste("COVID-19", cases_deaths),
                 fill = "", color = "",
                 title = "") +
            theme_minimal() + 
            theme(legend.position="top",
                  legend.text = element_text(size=14),
                  strip.text = element_markdown(size = 14, hjust = 0))
          
          saveRDS(p_line, file.path(FIGURES_PATH, paste0("fig_line",
                                                         "_keyword", keyword,
                                                         "_cases_deaths", cases_deaths,
                                                         "_continent", continent,
                                                         "_sort_by", sort_by,
                                                         ".Rds")))
        
        }
        
        # 2. Histogram of Correlation ------------------------------------------
        if(HIST_COR){
          df <- cor_df %>%
            dplyr::mutate(bins = round(cor*100, digits=-1) / 100) %>%
            distinct(geo, bins) %>%
            dplyr::group_by(bins) %>%
            dplyr::summarise(N = n()) %>%
            ungroup() 
          
          df_m <- seq(from = min(df$bins),
                      to = max(df$bins),
                      by = .1) %>%
            as.data.frame() %>%
            dplyr::rename(bins = ".") %>%
            mutate(bins = bins %>% round(1))
          
          df <- merge(df, df_m, by = "bins", all=T) %>%
            mutate(N = replace_na(N, 0)) %>%
            mutate(text = paste0("Correlation: ", bins, "\nN countries: ", N)) 
          
          
          p <- ggplot(df) +
            geom_col(aes(x = bins %>% as.factor(), 
                         y = N, 
                         fill = bins,
                         text = text), color = "black") +
            labs(x = "Correlation",
                 y = "Number of Countries") +
            scale_fill_gradient(low = "white",
                                high = muted("red")) +
            theme_minimal() +
            theme(legend.position = "none")
          
          saveRDS(p, file.path(FIGURES_PATH, paste0("fig_cor_hist",
                                                    "_keyword", keyword,
                                                    "_cases_deaths", cases_deaths,
                                                    "_continent", continent,
                                                    ".Rds")))
        }
        # 3. Time Lag Hist -----------------------------------------------------
        if(HIST_TIME_LAG){
          p <- cor_df %>%
            mutate(text = "FILLER") %>%
            ggplot() +
            geom_histogram(aes(x = lag),
                           fill = "palegreen3",
                           color = "black",
                           text = text,
                           binwidth = 3) +
            geom_vline(aes(xintercept = time_lag_best), color = "red") +
            labs(x = "Time lag (days) of strongest correlation",
                 y = "Number of Countries") +
            theme_minimal()
          
          saveRDS(p, file.path(FIGURES_PATH, paste0("fig_time_lag_hist",
                                                    "_keyword", keyword,
                                                    "_cases_deaths", cases_deaths,
                                                    "_continent", continent,
                                                    ".Rds")))
        }
        
        # 4. Correlation Map ---------------------------------------------------
        if(MAP_COR){
          cor_sub_df <- cor_df %>%
            dplyr::select(geo, cor)
          
          world_data <- merge(world, cor_sub_df, by = "geo", all.x = T, all.y = F)
          world_data$text <- paste0(world_data$name, "\n", world_data$cor %>% round(2))
          
          world_data <- world_data %>% st_as_sf()
          
          p <- ggplot() +
            geom_sf(data = world_data,
                    aes(fill = cor,
                        text = text),
                    color = NA) +
            scale_fill_gradient(low = "white",
                                high = muted("red")) +
            theme_void() +
            theme(legend.position = "none") +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_blank()) #+
          #coord_map(
          #  projection = "mercator")
          
          saveRDS(p, file.path(FIGURES_PATH, paste0("fig_cor_map",
                                                    "_keyword", keyword,
                                                    "_cases_deaths", cases_deaths,
                                                    "_continent", continent,
                                                    ".Rds")))
        }
        
        # 5. Hits Change Table -------------------------------------------------
        if(CHANGE_TABLE | CHANGE_MAP){
          #### Define Colors
          customGreen = "#71CA97"
          customGreen0 = "#DeF7E9"
          customRed = "#ff7f7f"
          customRed0 = "#FA614B66"
          customGreen0 = "#DeF7E9"
          customYellow = "goldenrod2"
          
          data_for_table <- gtrends_df %>%
            filter(!is.na(week)) %>%
            group_by(Country, hits_change, cases_total) %>%
            summarize(
              TrendSparkline = spk_chr(
                hits, 
                type ="line",
                lineColor = 'black', 
                fillColor = "orange", 
                height=40,
                width=90
              ),
              week_2_hits = week_2_hits[1]
            ) %>%
            dplyr::select(Country, cases_total, hits_change, week_2_hits, TrendSparkline) %>%
            arrange(-hits_change) %>%
            ungroup() %>%
            mutate(hits_change = round(hits_change, 2),
                   week_2_hits = round(week_2_hits, 2))
          
          #### Make Table
          f_list <- list(
            `var1` = formatter("span", style = ~ style(color = "black")),
            `var2` = formatter("span", style = ~ style(color = "black")),
            `var3` = formatter("span", style = ~ style(color = "black")),
            `var4` = formatter("span", style = ~ style(color = "black"))
          )
          
          names_vec <- c("Country", "Total Cases", "Change", "Average Hits List Week","Trend")
          names(f_list)         <- names_vec[1:4]
          names(data_for_table) <- names_vec
          
          table_max <- 10
          
          # l <- formattable(
          #   data_for_table %>% as.data.table(), # [1:table_max,]
          #   align = c("l", "l", "l", "l"),
          #   f_list
          # ) %>% format_table(align = c("l", "l", "l", "l")) %>%
          #   htmltools::HTML() %>%
          #   div() %>%
          #   # use new sparkline helper for adding dependency
          #   spk_add_deps() %>%
          #   # use column for bootstrap sizing control
          #   # but could also just wrap in any tag or tagList
          #   {column(width=12, .)}
          
          saveRDS(data_for_table, file.path(FIGURES_PATH, paste0("data_hits_change_table",
                                                                 "_keyword", keyword,
                                                                 "_cases_deaths", cases_deaths,
                                                                 "_continent", continent,
                                                                 ".Rds")))
        }
        
        
        # 6. Hits Change Map ---------------------------------------------------
        if(CHANGE_MAP){
          change_df <- gtrends_df %>%
            distinct(geo, Country, hits_change)
          
          #spark_df <- data_for_table %>%
          #  dplyr::select(Country, Trend)
          
          world_data_sf <- merge(world_sf, change_df, by = "geo", all.x = T, all.y = F)
          #world_data_sf <- merge(world_data_sf, spark_df, by = "Country", all.x = T, all.y = F)
          
          
          world_data_sf$text <- paste0(world_data_sf$name, "\n", 
                                       world_data_sf$hits_change %>% round(2))
          
          p <- ggplot() +
            geom_sf(data = world_data_sf,
                    aes(fill = hits_change,
                        text = text),
                    color = NA) +
            
            scale_fill_gradient2(low =  "#1A9850",
                                 mid = "#FFFFBF",
                                 high = "#D73027",
                                 midpoint = 0) +
            theme_void() +
            theme(legend.position = "none") +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_blank()) #+
          #coord_map(
          #  projection = "mercator")
          
          saveRDS(p, file.path(FIGURES_PATH, paste0("fig_hits_change_map",
                                                    "_keyword", keyword,
                                                    "_cases_deaths", cases_deaths,
                                                    "_continent", continent,
                                                    ".Rds")))
        }
        
        # 7. Max Correlation Distribution --------------------------------------
        if(MAX_COR_HIST){
          
          cor_all_sum_df <- cor_all_df %>%
            group_by(keyword_en) %>%
            dplyr::summarise(cor = mean(cor)) %>%
            ungroup()
          
          p <- ggplot() +
            geom_violin(aes(x = reorder(keyword_en,
                                        cor),
                            y = cor),
                        data = cor_all_df,
                        fill = "palegreen3") +
            geom_point(aes(x = reorder(keyword_en,
                                       cor),
                           y = cor),
                       data = cor_all_sum_df,
                       fill = "palegreen3") +
            geom_hline(yintercept = 0) +
            coord_flip() +
            theme_minimal() +
            labs(x = "",
                 y = "Correlation") +
            theme(axis.text.y = element_text(face = "bold", size = 14)) +
            theme(axis.text.x = element_text(size = 14))
          
          saveRDS(p, file.path(FIGURES_PATH, paste0("fig_max_cor_hist",
                                                    "_cases_deaths", cases_deaths,
                                                    "_continent", continent,
                                                    ".Rds")))
          
        }
        
        
        
      }
    }
  }
}


