
#' Exploratory analysis plots. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
library(ggsci)
library(cowplot)
library(ggpubr)
library(ggtext)
library(grid)
library(runstats)
options(digits.secs = 3)
options(scipen = 999)
source(file.path(here(), "R", "config_figures.R"))
source(file.path(here(), "R", "config.R"))
source(file.path(here(), "R", "utils.R"))


# ------------------------------------------------------------------------------
# read data 
# ------------------------------------------------------------------------------

# read sample
dat_sample_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds")
dat_sample_beiwe_id_0 <- readRDS(dat_sample_beiwe_id_path) 
dat_sample_beiwe_id <- 
  dat_sample_beiwe_id_0 %>% 
  select(beiwe_id, device_os, adol_adult, date_min_meta, approx_days_after_dc, beiwe_id_fct)

# read subset of beiwe ID and utc dates of power_state data to be used in statistical analyses 
dat_sample_power_state_t24h_path <- file.path(here(), "results_objects", "analysis_sample_power_state_t24h.rds")
dat_sample_power_state_t24h <- readRDS(dat_sample_power_state_t24h_path)

# read actual data 
dat_t24h_path <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_measures_t24h.rds")
dat_t24h <- 
  readRDS(dat_t24h_path) %>%
  inner_join(dat_sample_power_state_t24h, by = c("beiwe_id", "utc_time_date")) 
# check
dim(dat_t24h)
length(unique(dat_t24h$beiwe_id))


# ------------------------------------------------------------------------------
# create a plot df 
# ------------------------------------------------------------------------------

dat_t24h2 <- 
  dat_t24h %>% 
  left_join(dat_sample_beiwe_id %>% select(beiwe_id, date_min_meta)) %>%
  mutate(relative_day = as.numeric(utc_time_date - date_min_meta)) 

# expand to long
dat_t24h3 <- 
  expand.grid(beiwe_id = unique(dat_t24h2$beiwe_id), relative_day = 0 : max(dat_t24h2$relative_day)) %>%
  left_join(dat_t24h2) %>%
  inner_join(dat_sample_beiwe_id %>% select(beiwe_id, adol_adult, beiwe_id_fct)) %>%
  mutate(
    adol_adult_fct = factor(adol_adult, levels = c("adol", "adult"), labels = c("Adolescent", "Adult"))
  ) %>%
  mutate(
    total_onscreen_dur_min = total_onscreen_dur / 60,
    avg_onscreen_bout_dur_min = avg_onscreen_bout_dur / 60,
    avg_offscreen_bout_dur_min = avg_offscreen_bout_dur / 60
  )

plt_color = "skyblue"
plt_color2 = "navyblue"


# ------------------------------------------------------------------------------
# create a parameters plot
# ------------------------------------------------------------------------------

col_vec <- rev(pal_jco()(2))
# show_col(pal_jco()(2))

param_df <- data.frame(
  y_name = rep(Y_NAME_MINUTE_VEC, each = 2),
  y_label = rep(Y_LABEL_MINUTE_VEC, each = 2),
  adol_adult_fct = rep(c("Adolescent", "Adult"), 3),
  adol_adult_fct_label = rep(c("Adolescents", "Adults"), 3),
  plt_color = rep(col_vec, 3),
  plt_color2 = rep(col_vec, 3)
)
param_df

y_limits_list <- list(
  c(0, 600),
  c(0, 600),
  c(0, 14),
  c(0, 14),
  c(0, 80),
  c(0, 80)
)

plot_title_vec <- list(
  "a",
  "b",
  "c",
  "d",
  "e",
  "f"
)

probs_vec <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
probs_label_vec <- paste0("qt_", gsub("\\.", "", sprintf("%.2f", probs_vec)))


# ------------------------------------------------------------------------------
# make 3x2 plots with quantiles
# ------------------------------------------------------------------------------

plt_list <- list()

for (i in 1 : nrow(param_df)){ # i <- 1
  
  y_name_i <- param_df$y_name[i]
  y_label_i <- param_df$y_label[i]
  adol_adult_fct_i <- param_df$adol_adult_fct[i]
  adol_adult_fct_label_i <- param_df$adol_adult_fct_label[i]
  y_limits_i <- y_limits_list[[i]]
  plot_title_i <- plot_title_vec[i]
  
  plt_df <- 
    dat_t24h3  %>%
    filter(adol_adult_fct == adol_adult_fct_i) %>%
    rename_with(~ c("y"), all_of(y_name_i)) %>%
    group_by(relative_day) %>%
    summarise(y_agg = quantile(y, probs_vec, na.rm = TRUE),
              q_prob_label = probs_label_vec) %>%
    ungroup() %>%
    group_by(q_prob_label) %>%
    arrange(q_prob_label, relative_day) %>%
    mutate(y_agg_rm = runstats::RunningMean(y_agg, W = 7, circular = FALSE)) %>%
    ungroup() %>%
    select(-y_agg) %>%
    filter(!is.na(y_agg_rm)) %>%
    pivot_wider(values_from = "y_agg_rm", names_from = q_prob_label) 

  text_x <- 3
  text_y <- y_limits_i[1] + 0.95 * diff(y_limits_i)
  text_value <- adol_adult_fct_label_i
  plt_color =  param_df$plt_color[i]
  plt_color2 = param_df$plt_color2[i]
  txt_color = "black"
  txt_size = 10
  line_size = 0.7
  x_breaks = seq(0, 200, by = 30)

  plt <- 
    ggplot(plt_df, aes(x = relative_day, y = qt_050)) + 
    # geom_ribbon(aes(ymin = qt_005, ymax = qt_095), alpha = 0.3, color = NA) + 
    geom_ribbon(aes(ymin = qt_010, ymax = qt_090), alpha = 0.4, color = NA, fill = plt_color) +
    geom_ribbon(aes(ymin = qt_025, ymax = qt_075), alpha = 0.4, color = NA, fill = plt_color) + 
    geom_line(aes(y = qt_010), alpha = 0.93, size = line_size, color = plt_color) + 
    geom_line(aes(y = qt_090), alpha = 0.93, size = line_size, color = plt_color) +
    geom_line(aes(y = qt_025), alpha = 0.93, size = line_size, color = plt_color2) + 
    geom_line(aes(y = qt_075), alpha = 0.93, size = line_size, color = plt_color2) + 
    geom_line(aes(y = qt_050), alpha = 0.93, size = line_size, color = "grey30") + 
    scale_y_continuous(limits = y_limits_i) + 
    scale_x_continuous(breaks = x_breaks, expand = rep(0.01, 2)) +
    theme(
      # panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey50", linewidth = 0.5, fill = NA),
      axis.text.x = element_text(color = txt_color, size = txt_size),
      axis.text.y = element_text(color = txt_color, size = txt_size),  
      axis.title.x = element_text(color = txt_color, size = txt_size),
      axis.title.y = element_text(color = txt_color, size = txt_size),
      plot.title = ggtext::element_markdown(color = txt_color, size = txt_size),
      text = element_text(color = txt_color, size = txt_size) 
    ) +
    # t, r, b, l
    theme(plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm")) + 
    # annotate("text", x = text_x, y = text_y, label = text_value, hjust = 0, fontface = 2, color = "grey30") + 
    annotate("text", x = text_x, y = text_y, label = text_value, hjust = 0, color = "grey20", size = txt_size/.pt) + 
    labs(y = y_label_i, x = "", title = paste0("**", plot_title_i, "**"))
  # if (i %in% 1 : 4){
  #   plt <- plt + labs(x = "")
  # }
  plt_list[[length(plt_list) + 1]] <- plt
}


# ------------------------------------------------------------------------------
# make plots showing number of participants 
# ------------------------------------------------------------------------------

plot_title_vec <- list(
  "g",
  "h"
)

i <- 1
y_limits_i <- c(0, 80)
text_x <- 3
text_y <- y_limits_i[1] + 0.95 * diff(y_limits_i)

for (i in 1 : 2){
  adol_adult_fct_i <- param_df$adol_adult_fct[i]
  adol_adult_fct_label_i <- param_df$adol_adult_fct_label[i]
  plot_title_i <- plot_title_vec[i]
  
  plt_df <- 
    dat_t24h3  %>%
    filter(adol_adult_fct == adol_adult_fct_i) %>%
    rename_with(~ c("y"), all_of("total_onscreen_dur")) %>%
    filter(!is.na(y)) %>%
    group_by(relative_day) %>%
    summarise(cnt = n()) %>%
    ungroup()
  
  plt <- 
    ggplot(plt_df, aes(x = relative_day, y = cnt)) + 
    geom_bar(stat = "identity", color = "grey30", fill = "grey30", alpha = 0.3, size = 0.2) + 
    scale_y_continuous(limits = y_limits_i) +
    scale_x_continuous(breaks = x_breaks, expand = rep(0.01, 2)) +
    theme(
      # panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey50", linewidth = 0.5, fill = NA),
      axis.text.x = element_text(color = txt_color, size = txt_size),
      axis.text.y = element_text(color = txt_color, size = txt_size),  
      axis.title.x = element_text(color = txt_color, size = txt_size),
      axis.title.y = element_text(color = txt_color, size = txt_size),
      plot.title = ggtext::element_markdown(color = txt_color, size = txt_size),
      text = element_text(color = txt_color, size = txt_size) 
    ) +
    theme(plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), "cm")) + 
    annotate("text", x = text_x, y = text_y, label = adol_adult_fct_label_i, hjust = 0, color = "grey20", size = txt_size/.pt) + 
    labs(y = "# participants", x = "Relative day", title = paste0("**", plot_title_i, "**"))
  plt_list[[length(plt_list) + 1]] <- plt
}


# generate final plot
plt <- plot_grid(plotlist = plt_list, ncol = 2, align = "vh", byrow = TRUE)
plt_fpath <- file.path(here(), "results_figures", "screen_time_measures_t24hr.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10, dpi = 300)


