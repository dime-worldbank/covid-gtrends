# Lockdown Difference-in-Difference Analysis

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", "FinalData", "results", 
                                "did_results_country.Rds"))

# Prep Data --------------------------------------------------------------------
results_df$sig <- ifelse(results_df$pvalue <= 0.05, "Yes", "No") %>% fct_rev()
results_df <- results_df[!is.na(results_df$b),]
results_df <- results_df[results_df$variable %in% "days_since_c_policy_yearcurrent_post_X_year2020",]

results_df <- results_df %>%
  group_by(keyword) %>%
  dplyr::mutate(b_avg = mean(b, na.rm = T)) 

results_df$keyword <- results_df$keyword %>% tools::toTitleCase()
results_df$keyword <- fct_reorder(results_df$keyword, results_df$b_avg)

p <- results_df %>%
  ggplot(aes(x = b,
             y = keyword,
             fill = sig)) +
  geom_vline(aes(xintercept = 0)) +
  geom_boxplot(alpha = 0.3,
               outlier.size = 0) +
  geom_point(position = position_jitterdodge(jitter.width=0.5,
                                             dodge.width = 0.85),
             pch = 21,
             size = 0.9, # 0.7
             stroke = 0.2, # 0.1
             color = "black") +
  scale_fill_manual(values = c("gray", "firebrick2")) +
  labs(x = "Difference-in-Difference Coefficient",
       y = "Search Term",
       fill = "Significant (p < 0.05)") +
  theme_minimal()

p <- results_df %>%
  ggplot(aes(x = b,
             y = keyword)) +
  geom_vline(aes(xintercept = 0)) +
  geom_boxplot(alpha = 0.3,
               outlier.size = 0,
               outlier.alpha = 0) +
  geom_point(aes(fill = sig),
             position = position_jitterdodge(jitter.width=0.5,
                                             dodge.width = 0),
             pch = 21,
             size = 1.1, #0.9, # 0.7
             stroke = 0.2, # 0.1
             color = "black") +
  scale_fill_manual(values = c("firebrick2", "gray")) +
  labs(x = "Difference-in-Difference Coefficient",
       y = "Search Term",
       fill = "Significant (p < 0.05)") +
  theme_minimal() +
  theme(axis.text = element_text(face = "bold"))

ggsave(p, filename = file.path(paper_figures, "did_country_dot_box.png"),
       height = 8, width = 6)
# height = 6, width = 6
