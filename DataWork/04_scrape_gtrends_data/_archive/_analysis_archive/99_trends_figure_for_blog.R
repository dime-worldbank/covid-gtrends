# Merge Correlation Data with Other Data

# Load Data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor_since_","2020-02-01",".Rds")))

gtrends_df <- gtrends_df %>%
  group_by(geo, keyword_en) %>%
  mutate(hits_ma7 = hits_ma7 / max(hits_ma7, na.rm=T),
         cases_new = cases_new / max(cases_new, na.rm=T))

gtrends_df <- gtrends_df %>%
  filter(keyword_en %in% "loss of smell")

gtrends_df$Country[gtrends_df$Country %in% "occupied Palestinian territory, including east Jerusalem"] <- "Palestine"
gtrends_df$Country[gtrends_df$Country %in% "Iran (Islamic Republic of)"] <- "Iran"
gtrends_df$Country[gtrends_df$Country %in% "Bolivia (Plurinational State of)"] <- "Bolivia"
gtrends_df$Country[gtrends_df$Country %in% "United Republic of Tanzania"] <- "Tanzania"
gtrends_df$Country[gtrends_df$Country %in% "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
gtrends_df$Country[gtrends_df$Country %in% "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
gtrends_df$Country[gtrends_df$Country %in% "Viet Nam"] <- "Vietnam"

# gtrends_df <- gtrends_df %>%
#   filter(Country %in% c("United States of America", "Australia", "Brazil"))

library(ggtext)

#gtrends_df$Country <- fct_reorder(gtrends_df$Country, gtrends_df$cor_casesMA7_hitsMA7_mean, .desc=T)

p <- gtrends_df %>%
  filter(cases_total > 0) %>%
  ggplot() +
  geom_col(aes(x = date, y = cases_new),
           fill = "#ffc266", # orange3
           color = "#ffc266") +
  geom_line(aes(x = date, y = hits_ma7),
            color = "#3AA959", # green4
            size=0.8) + # .75 olivedrab3 deepskyblue
  labs(x = "",
       y = "",
       # #ffffff -- white
       title ="<span style='font-size:18pt'><span style='color:#000000;'>Trends in Google Search Interest in</span> 
               <span style='color:#3AA959;'>'Loss of Smell'</span> 
               <span style='color:#000000;'>and</span>
    <span style='color:#ff9900;'>COVID-19 Cases</span>
    <br>
    </span>") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold", color = "black", size=10),
        #panel.spacing = unit(0.2, "lines"),
        #panel.spacing.x=unit(0, "lines"),
        #panel.spacing.y=unit(0, "lines"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        plot.title = element_markdown(lineheight = 1.1, hjust = 0.5, face = "bold",size=5)) +
  facet_wrap(~Country, 
             ncol = 8,
             scales = "free") 

ggsave(p, filename = file.path(dropbox_file_path,
                               "Blog Figures",
                               "blog1_figure1.png"),
       heigh = 16, width=14.2)

ggsave(p, filename = file.path(dropbox_file_path,
                               "Data",
                               "google_trends",
                               "Outputs",
                               "figures",
                               "trend_i_cant_smell.png"),
       heigh = 12, width=14.1)



# #f9ba2d
# #3aa757

#gtrends_df$Country[nchar(gtrends_df$Country) > 10] %>% unique()
