
#' @description 
#' Get t24hr (day-level) valid day label. 
#' 
#' Used ariables' values (from source config file, not publically available)
#' VALID_MINUTES_THRESH_IOS = 60 * 18
#' ANY_SCREEN_TIME_HOURS_THRESH_ANDROID = 8

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 3)
options(scipen=999)

# source config file (not publically available) with hard-coded values 
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# iOS operating system
# ------------------------------------------------------------------------------

path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "valid_minute_t1min_ios.rds")
dat <- readRDS(path_tmp)

dat_t24hr_ios <-
  dat %>% 
  mutate(utc_time_date = as.Date(utc_time)) %>%
  group_by(
    beiwe_id,
    utc_time_date
  ) %>%
  summarise(
    minutes_cnt = n(),
    valid_minutes = sum(valid_minute)
  ) %>%
  ungroup()

dat_t24hr_ios <- 
  dat_t24hr_ios %>%
  mutate(valid_day_utc = ifelse(valid_minutes >= VALID_MINUTES_THRESH_IOS, 1, 0))


# ------------------------------------------------------------------------------
# Android operating system
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# read t1min screen time data to infer for android 
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_t1min.rds")
dat <- readRDS(path_tmp)
dat_android <- dat %>% filter(op_sys == "android")


# ------------------------------------------------------------------------------
# prepare expansion into long form

utc_time_df <- 
  dat_android %>% 
  group_by(beiwe_id, op_sys) %>%
  summarise(
    utc_time_date_min = min(utc_time_date),
    utc_time_date_max = max(utc_time_date)) %>%
  as.data.frame() 
nrow(utc_time_df)

utc_time_grid_list <- list()
for (i in 1 : nrow(utc_time_df)){ # i <- 1
  print(i)
  beiwe_id_i <- utc_time_df$beiwe_id[i] 
  op_sys_i <- utc_time_df$op_sys[i] 
  utc_time_date_i <- seq(utc_time_df$utc_time_date_min[i], utc_time_df$utc_time_date_max[i], by = '1 day')
  out_df_i <- data.frame(utc_time_date = utc_time_date_i)
  out_df_i$beiwe_id = beiwe_id_i
  out_df_i$op_sys = op_sys_i
  utc_time_grid_list[[i]] <- out_df_i
}
utc_time_grid_df <- rbindlist(utc_time_grid_list) %>% as.data.frame()


# ------------------------------------------------------------------------------
# aggregate dat_android 

# calculate number of distinct hours with any screen time
dat_android_t24h_0 <- 
  dat_android %>%
  filter(screen_time_sec > 0) %>%
  mutate(utc_time_hour = hour(utc_time)) %>%
  group_by(
    beiwe_id,
    utc_time_date
  ) %>%
  summarise(
    hours_any_screen_time = n_distinct(utc_time_hour)
  ) %>%
  ungroup()

# join date and time grid 
dat_android_t24h <- 
  utc_time_grid_df %>%
  left_join(dat_android_t24h_0) %>%
  mutate(hours_any_screen_time = ifelse(is.na(hours_any_screen_time), 0, hours_any_screen_time))
nrow(dat_android_t24h)
# [1] 7443    # 2022-11-07

dat_android_t24h <- 
  dat_android_t24h %>%
  mutate(valid_day_utc = ifelse(hours_any_screen_time >= ANY_SCREEN_TIME_HOURS_THRESH_ANDROID, 1, 0))


# ------------------------------------------------------------------------------
# combine ios + android, save 
# ------------------------------------------------------------------------------

dat_t24hr_comb <- 
  (dat_t24hr_ios %>% select(beiwe_id, utc_time_date, valid_day_utc)) %>%
  rbind(dat_android_t24h %>% select(beiwe_id, utc_time_date, valid_day_utc)) %>%
  arrange(beiwe_id, utc_time_date)

dim(dat_t24hr_comb)
head(dat_t24hr_comb)

# save to file 
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "power_state_t24hr_valid_day.rds")
saveRDS(dat_t24hr_comb, path_tmp)

