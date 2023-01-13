
#' Plot minute-level on-screen time 

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
options(digits.secs = 3)
options(scipen = 999)
source(file.path(here(), "R", "config.R"))
source(file.path(here(), "R", "config_figures.R"))
source(file.path(here(), "R", "utils.R"))


# ------------------------------------------------------------------------------
# read data 
# ------------------------------------------------------------------------------

# read sample
sample_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds")
sample_beiwe_id <- readRDS(sample_beiwe_id_path) 

# read subset of beiwe ID and utc dates of power_state data to be used in statistical analyses 
dat_sample_power_state_t24h_path <- file.path(here(), "results_objects", "analysis_sample_power_state_t24h.rds")
dat_sample_power_state_t24h <- readRDS(dat_sample_power_state_t24h_path)

# read actual data (minute-level)
t1 <- Sys.time()
dat_t1min_path <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_t1min.rds")
dat_t1min <- readRDS(dat_t1min_path) 
t2 <- Sys.time()
t2 - t1
# Time difference of 52.48328 secs

# read pre-calculated data of average daily measures stats 
dat_t24h_agg_path <- file.path(here(), 'data_beiwe_processed', 'power_state', "screen_time_measures_t24h.rds")
dat_t24h_agg <- 
  readRDS(dat_t24h_agg_path) %>%
  inner_join(dat_sample_power_state_t24h, by = c("beiwe_id", "utc_time_date")) 
dim(dat_t24h_agg)
length(unique(dat_t24h_agg$beiwe_id))


# ------------------------------------------------------------------------------
# select the sample we are using
# ------------------------------------------------------------------------------

dat_t24h_aggall <- 
  dat_t24h_agg %>% 
  select(-c(utc_time_date, valid_day_utc)) %>%
  group_by(beiwe_id, op_sys) %>%
  mutate(cnt = n()) %>%
  group_by(beiwe_id, op_sys, cnt) %>% 
  summarise_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  # filter(op_sys == "ios", cnt > 140) %>%
  filter(op_sys == "ios") %>%
  mutate(
    total_onscreen_dur_min = total_onscreen_dur / 60,
    avg_onscreen_bout_dur_min = avg_onscreen_bout_dur / 60,
    avg_offscreen_bout_dur_min = avg_offscreen_bout_dur / 60
  ) %>%
  select(beiwe_id, total_onscreen_dur_min, avg_onscreen_bout_dur_min, avg_offscreen_bout_dur_min, cnt) %>%
  arrange(total_onscreen_dur_min) %>%
  as.data.frame()
dat_t24h_aggall


# ------------------------------------------------------------------------------
# prepare the data 
# ------------------------------------------------------------------------------

# join to have only valid days 
dat_t1min_2 <- 
  dat_t1min %>% 
  inner_join(dat_sample_power_state_t24h) %>%
  select(beiwe_id, utc_time, screen_time_sec) 
  
# expand to long form 
utc_time_grid_list <- list()
for (i in 1 : nrow(sample_beiwe_id)){ # i <- 1
  print(i)
  beiwe_id_i <- sample_beiwe_id$beiwe_id[i] 
  time_start_i <- ymd_hms(paste0(sample_beiwe_id$date_min_meta[i], " 00:00:00"))
  time_end_i <- ymd_hms(paste0(sample_beiwe_id$date_max_meta[i], " 23:59:59"))
  utc_time_i <- seq(time_start_i, time_end_i, by = '1 min')
  out_df_i <- data.frame(utc_time = utc_time_i)
  out_df_i$beiwe_id = beiwe_id_i
  utc_time_grid_list[[i]] <- out_df_i
}
utc_time_grid_df <- rbindlist(utc_time_grid_list) %>% as.data.frame()
# note we keep some screen_time_sec purposefully NA here 
dat_t1min_3 <- 
  utc_time_grid_df %>%
  left_join(dat_t1min_2) %>%
  mutate(utc_time_date = as.Date(utc_time)) 

# add relative_day, minute of a day
dat_t1min_4 <- 
  dat_t1min_3 %>%
  group_by(beiwe_id) %>%
  mutate(relative_day = as.numeric(utc_time_date - min(utc_time_date))) %>%
  mutate(day_minute_idx = (minute(utc_time) + hour(utc_time) * 60)) %>%
  ungroup()

dim(dat_t1min_2)
dim(dat_t1min_3)
dim(dat_t1min_4)
summary(dat_t1min_4$day_minute_idx)

# rename final data frame
dat_t1min_F <- dat_t1min_4


# ------------------------------------------------------------------------------
# plot: screen time volume, per minute
# ------------------------------------------------------------------------------

# Note: uses MINUTELEVEL_SCREEN_TIME_BEIWE_ID_VEC -- predefined vector of subject IDS
# that is defined in config.R file (not publically available)

txt_size <- 10
txt_color <- "black"
subplot_letter_vec <- c("a", "b", "c", "d")

plt_list <- list()
plt_legend <- NULL
for (i in 1 : length(MINUTELEVEL_SCREEN_TIME_BEIWE_ID_VEC)){ # i <- 4
  print(i)
  beiwe_id_tmp <- MINUTELEVEL_SCREEN_TIME_BEIWE_ID_VEC[i] # "d6tqz1ex"
  # plot title 
  beiwe_id_fct_tmp   <- sample_beiwe_id %>% filter(beiwe_id == beiwe_id_tmp) %>% pull(beiwe_id_fct)
  device_os_tmp      <- sample_beiwe_id %>% filter(beiwe_id == beiwe_id_tmp) %>% pull(device_os)
  adol_adult0_tmp    <- sample_beiwe_id %>% filter(beiwe_id == beiwe_id_tmp) %>% pull(adol_adult)
  adol_adult_tmp     <- ifelse(adol_adult0_tmp == "adol", "adolescent", "adult")
  # day-level measures params
  
  total_onscreen_dur_min_tmp  <- dat_t24h_aggall %>% filter(beiwe_id == beiwe_id_tmp) %>% pull(total_onscreen_dur_min)
  avg_onscreen_bout_dur_min_tmp <- dat_t24h_aggall %>% filter(beiwe_id == beiwe_id_tmp) %>% pull(avg_onscreen_bout_dur_min)
  avg_offscreen_bout_dur_min_tmp <- dat_t24h_aggall %>% filter(beiwe_id == beiwe_id_tmp) %>% pull(avg_offscreen_bout_dur_min)
  subplot_letter_tmp <- subplot_letter_vec[i]
  plt_title <- paste0(
    "**", subplot_letter_tmp, "** <br>",
    # " \n&nbsp;",
    beiwe_id_fct_tmp, " (", device_os_tmp, ", ", adol_adult_tmp, ")",
    ", ", "Total on-screen time = ", sprintf("%.1f", total_onscreen_dur_min_tmp), 
    " minutes, avg. on-screen bout = ", sprintf("%.1f", avg_onscreen_bout_dur_min_tmp), 
    " minutes, avg. off-screen bout = ", sprintf("%.1f", avg_offscreen_bout_dur_min_tmp), " minutes"
  )
  # plot data frame 
  plt_df <- dat_t1min_F %>% filter(beiwe_id == beiwe_id_tmp) 
  # plot 
  y_limits <- c(0, 1440)
  y_breaks <- seq(0, 24 * 60, by = 4 * 60)
  plt <- 
    ggplot(plt_df, aes(x = relative_day, y = day_minute_idx, fill = screen_time_sec)) + 
    geom_tile() + 
    labs(
      x = "",
      y = "",
      title = plt_title
    ) + 
    # scale_fill_gradient2(low =  "yellow", high = "blue") + 
    scale_fill_material("purple", na.value = "grey90") + 
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 170, by = 30)) +
    scale_y_continuous(expand = c(0, 0), breaks = y_breaks, limits = y_limits) + 
    theme(
      panel.grid.major = element_line(size = 0.3, color = "grey50"), 
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
    theme(plot.margin = unit(c(0.1, 0.2, 0, 0.2), "cm"))
  if (i == 4){
    plt <- plt + labs(
        x = "Relative day",
        y = "Minute of a day [UTC time]",
        fill = "Screen time per minute [secs]", 
        title = plt_title
      ) + 
      theme(legend.position = "bottom")
    # save the legend
    plt_legend <- get_legend(plt)
  }
  plt <- plt + guides(fill = "none")
  plt_list[[length(plt_list) + 1]] <- plt
}

# add last plot consisting of a legend only 
plt_with_legend <- 
  ggplot(plt_df, aes(x = relative_day, y = day_minute_idx, fill = screen_time_sec)) + 
  scale_fill_material("purple", na.value = "grey91") 
plt_legend_add <- as_ggplot(plt_legend) + theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))
plt_list[[5]] <- plt_legend_add
  
# generate final plot
plt <- plot_grid(plotlist = plt_list, ncol = 1, align = "v", byrow = TRUE, 
                 rel_heights = c(rep(1, 4), 0.25))
plt_fpath <- file.path(here(), "results_figures", "minutelevel_screen_time.jpeg")
save_plot(filename = plt_fpath, plot = plt, base_width = 10, base_height = 10, dpi = 300)


# ------------------------------------------------------------------------------
# pull hourly values for the paper text

df_tmp <- 
  dat_t1min %>%
  filter(beiwe_id == MINUTELEVEL_SCREEN_TIME_BEIWE_ID_VEC[2]) %>%
  mutate(relative_day = as.numeric(utc_time_date - min(utc_time_date))) 

df_tmp2 <- df_tmp %>% filter(relative_day == 24)
head(df_tmp2)
# Eastern Daylight Time (EDT; Mar-Nov) is 4 hours behind Coordinated Universal Time
df_tmp2$screen_time_sec[600: 800]
min_idx <- 630
hour_UTC <- min_idx / 60
hour_EDT <- hour_UTC - 4
hour_EDT

df_tmp2 <- df_tmp %>% filter(relative_day == 92)
head(df_tmp2)
# Eastern Standard Time (EST; Nov-Mar) is 5 hours behind Coordinated Universal Time
df_tmp2$screen_time_sec[600: 800]
min_idx <- 690
hour_UTC <- min_idx / 60
hour_EST <- hour_UTC - 5
hour_EST

# Eastern Standard Time (EST; Nov-Mar) is 5 hours behind Coordinated Universal Time
# Eastern Daylight Time (EDT; Mar-Nov) is 4 hours behind Coordinated Universal Time




