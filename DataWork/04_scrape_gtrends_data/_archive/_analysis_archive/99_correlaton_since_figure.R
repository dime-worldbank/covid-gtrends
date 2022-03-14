# Merge Correlation Data with Other Data

# Correlation Since ============================================================
cor_02_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "global_with_refstate",
                               paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",
                                      "2020-02-01",
                                      ".Rds")))

cor_04_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "global_with_refstate",
                               paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",
                                      "2020-04-01",
                                      ".Rds")))

cor_06_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                               "global_with_refstate",
                               paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",
                                      "2020-06-01",
                                      ".Rds")))

cor_df <- bind_rows(
  cor_02_df %>%
    filter(keyword_en %in% "loss of smell") %>%
    mutate(begin_date = "2020-02-01"),
  cor_04_df %>%
    filter(keyword_en %in% "loss of smell") %>%
    mutate(begin_date = "2020-04-01"),
  cor_06_df %>%
    filter(keyword_en %in% "loss of smell") %>%
    mutate(begin_date = "2020-06-01")
) %>%
  filter(type %in% "Cases")

cor_df$begin_date[cor_df$begin_date %in% "2020-02-01"] <- "February\n"
cor_df$begin_date[cor_df$begin_date %in% "2020-04-01"] <- "April\n"
cor_df$begin_date[cor_df$begin_date %in% "2020-06-01"] <- "June\n"

cor_df$begin_date <- cor_df$begin_date %>%
  as.character() %>%
  factor(levels = c("February\n", "April\n", "June\n"))

p_cor_since <- ggplot() +
  geom_dotplot(data = cor_df,
               aes(x = begin_date,
                   y = cor,
                   fill = "=  One Country"),
               binaxis = "y", 
               stackdir = "center",
               dotsize = 1,
               binwidth = .03,
               color = "palegreen4") +
  scale_fill_manual(values = c("palegreen3")) +
  labs(fill = NULL) +
  #geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = "Using data since...\n",
       y = "Correlation\nBetween\n'Loss of Smell'\nand\nCOVID-19 Cases",
       title = NULL) +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.position = "top",
        plot.background=element_rect(fill=NA,
                                      color="black")) +
  scale_x_discrete(position = "top") 


# Cases Per Capita =============================================================

#### Parameters
begin_day_i <- "2020-02-01"

# Load Data --------------------------------------------------------------------
cor_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                            "global_with_refstate",
                            paste0("gl_gtrends_ref","US","_adj_cases_correlations_since_",begin_day_i,".Rds")))

gtrends_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData",
                                "global_with_refstate",
                                paste0("gl_gtrends_ref","US","_adj_cases_cor_since_",begin_day_i,".Rds")))

wdi_data <- readRDS(file.path(dropbox_file_path, "Data", "wdi", "RawData", "wdi_data.Rds"))

# Merge ------------------------------------------------------------------------
cases_df <- gtrends_df %>%
  distinct(geo, cases_total) 

cor_df <- merge(cor_df, wdi_data, by.x = "geo", by.y = "iso2c", all=F)
cor_df <- merge(cor_df, cases_df, by = "geo", all=F)

# Figures ----------------------------------------------------------------------
cor_df <- cor_df %>%
  filter(type %in% "Cases")

cor_df$cases_pc <- cor_df$cases_total / cor_df$SP.POP.TOTL

cor_df$cases_pc_group <- NA
cor_df$cases_pc_group[cor_df$cases_pc > 0.01] <- "Cases >1%\nPopulation"
cor_df$cases_pc_group[cor_df$cases_pc < 0.01] <- "Cases <1%\nPopulation"

cor_df <- cor_df[cor_df$keyword_en %in% "loss of smell",]



cor_df$cor[cor_df$cases_pc > 0.01] %>% mean()
cor_df$cor[cor_df$cases_pc < 0.01] %>% mean()
cor_df$country[cor_df$cases_pc > 0.01 & cor_df$cor < 0.462]

cor_df$lag %>% mean()
cor_df$lag %>% summary()


p_cor_cases_pc <- cor_df %>%
  filter(keyword_en %in% "loss of smell") %>%
  ggplot() +
  geom_dotplot(aes(x=cases_pc_group, y = cor,
                   fill = "=  One Country"),
               binaxis = "y", 
               stackdir = "center",
               dotsize = 1,
               binwidth = .03,
               color = "palegreen4") +
  scale_fill_manual(values = c("palegreen3")) +
  labs(fill = NULL) +
  #geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(x = " \n ",
       y = "  ",
       title = NULL) +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.position = "top",
        plot.background=element_rect(fill=NA,
                                      color="black")) +
  scale_x_discrete(position = "top")

# Arrange ======================================================================
ggarrange(p_cor_since, 
          p_cor_cases_pc,
          common.legend = T,
          widths = c(0.6, 0.4)) %>%
  ggsave(filename = file.path(dropbox_file_path,
                              "Data",
                              "google_trends",
                              "Outputs",
                              "figures",
                              "cor_over_time.png"),
         height = 6, width=12)


