
#' @description 
#' Get t1min valid minute label (for iOS operating system only).
#' 
#' The approach assumes that in most cases, the battery level is in one of the 
#' following states: 
#' (a) decreasing, 
#' (b) increasing (when a phone is charging), 
#' (c) constant at 0% (when the phone battery is drained), 
#' (d), constant at 100% (when the phone battery is charged to maximum and the 
#' phone is having the charger plugged in). 
#' 
#' Specifically, we labelled a minute of phone state logs data as valid if: 
#' (a) it was within a period of time that battery changes are recorded with 
#' an assumed minimal frequency of 1% change per 1 hour, or 
#' (b) if it was within an at most 12 hours-long period of no battery change 
#' logs that can be plausibly attributed to the phone battery being charged to 
#' 100% and the phone having a charger plugged in; 
#' otherwise, we labelled a minute of phone state logs data as invalid. 
#' 
#' For Android OS, mimicking a similar approach is not possible since the phone 
#' battery level changes are not recorded.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 3)
options(scipen=999)


# ------------------------------------------------------------------------------
# read data 
# ------------------------------------------------------------------------------

# save to file 
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "power_state_processed.rds")
dat <- readRDS(path_tmp)


# ------------------------------------------------------------------------------
# process data 
# ------------------------------------------------------------------------------

# get ios subset
dat_ios <- 
  dat %>% 
  filter(op_sys == "ios") %>%
  mutate(utc_time = ymd_hms(utc_time)) %>%
  mutate(utc_time_floort1min = floor_date(utc_time, unit = "minute"))


# ------------------------------------------------------------------------------
# aggregate on minute-level
dat_ios_agg <- 
  dat_ios %>%
  group_by(beiwe_id, utc_time_floort1min) %>%
  summarise(level = max(level)) %>%
  ungroup() %>%
  rename(utc_time = utc_time_floort1min)


# ------------------------------------------------------------------------------
# expand to a long form

utc_time_df <- 
  dat_ios_agg %>% 
  mutate(utc_time_date = as.Date(utc_time)) %>%
  group_by(beiwe_id) %>%
  summarise(
    utc_time_date_min = min(utc_time_date),
    utc_time_date_max = max(utc_time_date)) %>%
  as.data.frame() 
nrow(utc_time_df)

utc_time_grid_list <- list()
for (i in 1 : nrow(utc_time_df)){ # i <- 1
  print(i)
  beiwe_id_i <- utc_time_df$beiwe_id[i] 
  time_start_i <- ymd_hms(paste0(utc_time_df$utc_time_date_min[i], " 00:00:00"))
  time_end_i <- ymd_hms(paste0(utc_time_df$utc_time_date_max[i], " 23:59:59"))
  utc_time_i <- seq(time_start_i, time_end_i, by = '1 min')
  out_df_i <- data.frame(utc_time = utc_time_i)
  out_df_i$beiwe_id = beiwe_id_i
  out_df_i$beiwe_id = beiwe_id_i
  utc_time_grid_list[[i]] <- out_df_i
}
utc_time_grid_df <- rbindlist(utc_time_grid_list) %>% as.data.frame()
head(utc_time_grid_df)
dim(utc_time_grid_df)

# join date and time grid 
dat_ios_agg2 <- 
  utc_time_grid_df %>%
  left_join(dat_ios_agg, by = c("utc_time", "beiwe_id")) 
head(dat_ios_agg2)
dim(dat_ios_agg2)


# ------------------------------------------------------------------------------
# determine whether or not observed according to the assumed heuristic

dat_ios_agg3 <- 
  dat_ios_agg2 %>%
  mutate(
    level_nafilled_down = level,
    level_nafilled_up = level
    ) %>%
  tidyr::fill(level_nafilled_down, .direction = 'down') %>%
  tidyr::fill(level_nafilled_up, .direction = 'up') 

dat_ios_agg4 <- 
  dat_ios_agg3 %>%
  mutate(
    utc_time_chr = as.character(utc_time),
    utc_time_nafilled_down = ifelse(is.na(level), NA, utc_time_chr),
    utc_time_nafilled_up = ifelse(is.na(level), NA, utc_time_chr)
  )  %>%
  tidyr::fill(utc_time_nafilled_down, .direction = 'down')%>%
  tidyr::fill(utc_time_nafilled_up, .direction = 'up')

dat_ios_agg5 <- 
  dat_ios_agg4 %>%
  mutate(
    utc_time_nafilled_down = ymd_hms(utc_time_nafilled_down),
    utc_time_nafilled_up = ymd_hms(utc_time_nafilled_up),
    time_diff_down = as.numeric(difftime(utc_time_nafilled_down, utc_time, units = 'mins')),
    time_diff_up = as.numeric(difftime(utc_time_nafilled_up, utc_time, units = 'mins'))
  )

dat_ios_agg6 <- 
  dat_ios_agg5 %>% 
  filter(!is.na(time_diff_down), !is.na(time_diff_up))
dim(dat_ios_agg6)
dim(dat_ios_agg5)

rm(dat_ios_agg, dat_ios_agg2, dat_ios_agg3, dat_ios_agg4)

# define valid minutes
dat_ios_agg7 <- 
  dat_ios_agg6 %>% 
  mutate(
    level_nafilled_up = as.numeric(level_nafilled_up),
    level_nafilled_down = as.numeric(level_nafilled_down),
    valid_minute = 0,
    valid_minute = ifelse(abs(time_diff_down) + abs(time_diff_up) <= 60 & abs(level_nafilled_up - level_nafilled_down) <= 0.02, 1, valid_minute) ,
    valid_minute = ifelse(abs(time_diff_down) + abs(time_diff_up) <= (60 * 12) & level_nafilled_down >= 0.98 & level_nafilled_up >= 0.98, 1, valid_minute),
    valid_minute_case = "invalid",
    valid_minute_case = ifelse(abs(time_diff_down) + abs(time_diff_up) <= 60 & abs(level_nafilled_up - level_nafilled_down) <= 0.02, "valid_not_fully_charged", valid_minute_case) ,
    valid_minute_case = ifelse(abs(time_diff_down) + abs(time_diff_up) <= (60 * 12) & level_nafilled_down >= 0.98 & level_nafilled_up >= 0.98, "valid_fully_charged", valid_minute_case)
  )

dat_ios_agg8 <- 
  dat_ios_agg7 %>%
  select(beiwe_id, utc_time, valid_minute, valid_minute_case)


# ------------------------------------------------------------------------------
# save to file 

dat_F <- dat_ios_agg8

# save to file 
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "valid_minute_t1min_ios.rds")
saveRDS(dat_F, path_tmp)


