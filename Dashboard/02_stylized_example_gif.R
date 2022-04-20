# Stylized Gif

#### Parameters
color_cases <- "orange2"
color_hits <- "forestgreen"

#### Make Data Frame
dates <- seq(from = as.Date("2020-02-01"),
             to = as.Date("2020-04-01"),
             by = 1)

z_scores <- seq(-7, 15, length = length(dates))
values <- dchisq(z_scores, df=5)

df <- data.frame(date = dates,
                 values = values) %>%
  mutate(cases = round(values*10000)) %>%
  mutate(hits = cases) %>%
  mutate(hits = lead(hits, 10) %>% replace_na(0)) 

df_cor <- data.frame(NULL)

name_i <- 10 # start at 2 digit number for ordering
# if 999, then end
for(shift_i in c(0:-18, 1:18)){
  print(shift_i)
  
  if(shift_i < 0){
    df_shift <- df %>%
      mutate(cases_shift = lead(cases, abs(shift_i)))
  } else{
    df_shift <- df %>%
      mutate(cases_shift = lag(cases, abs(shift_i)))
  }
  
  df_shift_sub <- df_shift[!is.na(df_shift$cases_shift),]
  df_cor_i <- data.frame(cor = cor(df_shift_sub$cases_shift, df_shift_sub$hits),
                         shift = shift_i)
  df_cor <- bind_rows(df_cor,
                      df_cor_i)
  
  fig_line <- ggplot(df_shift) +
    geom_line(aes(x = date, y = hits, 
                  color = "Hits",
                  linetype = "Hits"),
              size = 1) +
    geom_line(aes(x = date, y = cases, 
                  color = "Cases",
                  linetype = "Cases"),
              size = 1) +
    geom_line(aes(x = date, y = cases_shift, 
                  color = "Cases (Shifted)",
                  linetype = "Cases (Shifted)"),
              size = 1) +
    scale_color_manual(name = "", values = c(color_cases, color_cases, color_hits)) +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "solid")) +
    labs(x = "") +
    theme_minimal() +
    theme(legend.position = "left") +
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Search\nInterest",
                                           breaks = c(0, 1500),
                                           labels = c("Low", "High"))) +
    labs(y = "COVID-19\nCases") +
    theme(axis.title.y.left = element_text(angle = 0, 
                                           vjust = 0.5, 
                                           color=color_cases,
                                           face = "bold",
                                           size=13),
          axis.title.y.right = element_text(angle = 0, 
                                            vjust = 0.5, 
                                            color=color_hits,
                                            face = "bold",
                                            size=13),
          axis.text.y.left = element_text(color = color_cases,
                                          size=13),
          axis.text.y.right = element_text(color = color_hits,
                                           size=13),
          axis.text = element_text(face = "bold", size=10),
          legend.text = element_text(face = "bold", size=10)) +
    theme(legend.position = "none")
  
  
  fig_col <- ggplot() +
    geom_col(data = df_cor,
             aes(x = shift, y = cor), fill = "dodgerblue4") +
    geom_col(data = df_cor_i,
             aes(x = shift, y = cor), fill = "red") +
    scale_y_continuous(limits = c(-1,1)) +
    scale_x_continuous(limits = c(-19,19)) + 
    labs(x = "Days Shift", 
         y = "Correlation") +
    theme_minimal() +
    theme(axis.text = element_text(face = "bold", size = 10),
          axis.title = element_text(face = "bold", size = 13))
  
  best_cor <- max(df_cor$cor) %>% round(3)
  zscore <- round((max(df_cor$cor) - mean(df_cor$cor)) / sd(df_cor$cor), 3) %>%
    replace_na(0)
  best_lag <- df_cor$shift[which.max(df_cor$cor)]
  
  #title <- paste0("The best correlation is ",
  #                best_cor,
  #                " with a ",best_lag, " day shift")
  # title <- paste0("The maximum correlation is ", best_cor,
  #                 " with a ",best_lag, " day shift.\n",
  #                 "The maximum correlation is ",
  #                 zscore,
  #                 " standard deviations above the average correlation (z-score).")
  # 
  title <- paste0(abs(shift_i), " Days into the ",
                  ifelse(shift_i <= 0, "Past","Future"),
                  "\nCorrelation: ", round(df_cor_i$cor, 2))
  
  fig_all <- ggarrange(fig_line, fig_col, widths = c(0.6, 0.4)) %>%
    annotate_figure(top = text_grob(title, color = "black", face = "bold", size = 14))
  
  ggsave(fig_all, filename = file.path(dropbox_file_path, "Data", "google_trends", "Outputs", 
                                       "cor_gif", "images", 
                                       paste0("image_", name_i, ".png")),
         height = 4, width = 12)
  
  name_i <- name_i + 1
  
}

## Best correlation
shift_i <- -10

print(shift_i)

if(shift_i < 0){
  df_shift <- df %>%
    mutate(cases_shift = lead(cases, abs(shift_i)))
} else{
  df_shift <- df %>%
    mutate(cases_shift = lag(cases, abs(shift_i)))
}

df_cor_i <- df_cor[df_cor$shift %in% shift_i,]

fig_line <- ggplot(df_shift) +
  geom_line(aes(x = date, y = hits, 
                color = "Hits",
                linetype = "Hits"),
            size = 1) +
  geom_line(aes(x = date, y = cases, 
                color = "Cases",
                linetype = "Cases"),
            size = 1) +
  geom_line(aes(x = date, y = cases_shift, 
                color = "Cases (Shifted)",
                linetype = "Cases (Shifted)"),
            size = 1) +
  scale_color_manual(name = "", values = c(color_cases, color_cases, color_hits)) +
  scale_linetype_manual(name = "", values = c("solid", "dashed", "solid")) +
  labs(x = "") +
  theme_minimal() +
  theme(legend.position = "left") +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Search\nInterest",
                                         breaks = c(0, 1500),
                                         labels = c("Low", "High"))) +
  labs(y = "COVID-19\nCases") +
  theme(axis.title.y.left = element_text(angle = 0, 
                                         vjust = 0.5, 
                                         color=color_cases,
                                         face = "bold",
                                         size=13),
        axis.title.y.right = element_text(angle = 0, 
                                          vjust = 0.5, 
                                          color=color_hits,
                                          face = "bold",
                                          size=13),
        axis.text.y.left = element_text(color = color_cases,
                                        size=13),
        axis.text.y.right = element_text(color = color_hits,
                                         size=13),
        axis.text = element_text(face = "bold", size=10),
        legend.text = element_text(face = "bold", size=10)) +
  theme(legend.position = "none")


fig_col <- ggplot() +
  geom_col(data = df_cor,
           aes(x = shift, y = cor), fill = "dodgerblue4") +
  geom_col(data = df_cor_i,
           aes(x = shift, y = cor), fill = "green", color = "green3") +
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(-19,19)) + 
  labs(x = "Days Shift", 
       y = "Correlation") +
  theme_minimal() +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 13))

best_cor <- max(df_cor$cor) %>% round(3)
zscore <- round((max(df_cor$cor) - mean(df_cor$cor)) / sd(df_cor$cor), 3) %>%
  replace_na(0)
best_lag <- df_cor$shift[which.max(df_cor$cor)]

#title <- paste0("The best correlation is ",
#                best_cor,
#                " with a ",best_lag, " day shift")
title <- paste0("The maximum correlation is ", best_cor,
                " when COVID-19 cases is shifted 10 days into the past\n")
# title <- paste0("The maximum correlation is ", best_cor,
#                 " when COVID-19 cases is shifted 10 days into the past.\n",
#                 "This correlation is ", 
#                 zscore, 
#                 " standard deviations above the average correlation (z-score).")
# 
# title <- paste0("Search Interest and COVID-19 cases ",
#                 abs(shift_i), " Days into the ",
#                 ifelse(shift_i <= 0, "Past","Future"),
#                 "\nCorrelation: ", round(df_cor_i$cor, 2))

fig_all <- ggarrange(fig_line, fig_col, widths = c(0.6, 0.4)) %>%
  annotate_figure(top = text_grob(title, color = "black", face = "bold", size = 14))

for(shift_i in rep(-10, 20)){
  ggsave(fig_all, filename = file.path(dropbox_file_path, "Data", "google_trends", "Outputs", 
                                       "cor_gif", "images", 
                                       paste0("image_", name_i, ".png")),
         height = 4, width = 12)
  
  name_i <- name_i + 1
  
}


if(T){
  file.path(dropbox_file_path, "Data", "google_trends", "Outputs", 
            "cor_gif", "images") %>% 
    list.files(pattern = '*.png', full.names = TRUE) %>%
    sort() %>%
    image_read() %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps=2) %>% # animates, can opt for number of loops
    image_write(file.path(dropbox_file_path, "Data", "google_trends", "Outputs", 
                          "cor_gif", "cor.gif")) 
  
}
