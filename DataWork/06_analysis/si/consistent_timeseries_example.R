# Figure illustrating creating a consistent time series

# Load data --------------------------------------------------------------------
gtrends_df <- readRDS(file.path(gtrends_dir, "FinalData",
                                "gtrends_full_timeseries",
                                "gtrends_complete_symptoms.Rds"))

gtrends_sub_df <- gtrends_df %>%
  dplyr::filter(geo %in% "US",
                keyword_en %in% "fever")

# Figure -----------------------------------------------------------------------
p1 <- gtrends_sub_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = hits_tm3),
            color = "orange2",
            size = 0.4) +
  geom_line(aes(y = hits_tm2),
            color = "dodgerblue3",
            size = 0.4) +
  geom_line(aes(y = hits_tm1),
            color = "forestgreen",
            size = 0.4) + 
  geom_line(aes(y = hits_t0),
            color = "blue",
            size = 0.4) + 
  geom_line(aes(y = hits_tp1),
            color = "purple",
            size = 0.4) + 
  geom_line(aes(y = hits_tp2),
            color = "firebrick3",
            size = 0.4) + 
  geom_line(aes(y = hits_tp3),
            color = "orange2",
            size = 0.4) + 
  #scale_color_manual(values = c("orange2", "dodgerblue3", "forestgreen")) +
  labs(x = NULL,
       y = "Search Interest",
       title = "A. Search interest across different time-series queries") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p2 <- gtrends_sub_df %>%
  ggplot() +
  geom_line(aes(x = date, y = hits)) +
  labs(x = NULL,
       y = "Search Interest",
       title = "B. Search interest: consistent time-series") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p <- ggarrange(p1, p2, ncol = 2)
ggsave(p, filename = file.path(paper_figures, "const_timeseries_ex.png"),
       height = 4, width = 12)

