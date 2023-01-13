
#' @description 
#' Calculate minute-level screen time information (duration of on-screen bout 
#' across each participant-minute). 

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

path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "estimated_screen_time.rds")
dat <- readRDS(path_tmp)


# ------------------------------------------------------------------------------
# transform bouts into minute-level data 
# ------------------------------------------------------------------------------

t1 <- Sys.time()
dat_nrow <- nrow(dat)
bouts_list <- vector(mode = "list", length = dat_nrow)
for (i in 1 : dat_nrow){ # i <- 1
  print(paste0(i, " / ", dat_nrow, " -- ", round(100 * i / dat_nrow, 2), "%"))
  utc_time_start <- dat$utc_time[i]
  utc_time_end <- dat$utc_time[i] + lubridate::seconds(dat$screen_time_sec[i])
  # vector with sequence of dates 
  utc_time_start_f <- floor_date(utc_time_start, unit = "minutes")
  utc_time_end_f <- floor_date(utc_time_end, unit = "minutes")
  utc_time_vec <- seq(utc_time_start_f, utc_time_end_f, by = '1 min')
  vals_n <- length(utc_time_vec)
  if (vals_n > 1){
    # vector with info how any minutes were used 
    screen_time_sec_vec <- rep(60, vals_n)
    screen_time_sec_vec[1] <- 60 - abs(as.numeric(difftime(utc_time_start_f, utc_time_start, units = "secs")))
    screen_time_sec_vec[vals_n] <- abs(as.numeric(difftime(utc_time_end_f, utc_time_end, units = "secs")))
    # vector with info whether this is a start
    bout_start_vec <- rep(0, vals_n)
    bout_start_vec[1] <- 1
  } else {
    # vector with info how any minutes were used 
    screen_time_sec_vec <- abs(as.numeric(difftime(utc_time_end, utc_time_start, units = "secs")))
    # vector with info whether this is a start
    bout_start_vec <- 1
  }
  # data frame 
  df_i <- data.frame(
    utc_time = utc_time_vec,
    bout_start = bout_start_vec,
    screen_time_sec = screen_time_sec_vec
  )
  df_i$beiwe_id = dat$beiwe_id[i]  
  df_i$op_sys = dat$op_sys[i]  
  df_i$bout_idx = i
  # append to data frame 
  bouts_list[[i]] <- df_i  
}
t2 <- Sys.time()
t2 - t1

dat_t1min_frombouts <- data.table::rbindlist(bouts_list) %>% as.data.frame(); rm(bouts_list)


# ------------------------------------------------------------------------------
# aggregate into unique minutes
# ------------------------------------------------------------------------------

dat_t1min <- 
  dat_t1min_frombouts %>%
  group_by(beiwe_id, op_sys, utc_time) %>%
  summarise(
    bout_start = sum(bout_start),
    screen_time_sec = sum(screen_time_sec),
    bout_idx_ndist = n_distinct(bout_idx)
  )
dat_t1min <- as.data.frame(dat_t1min)

# save to file a temporary state of the data frame 
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "dat_t1min_pre.rds")
saveRDS(dat_t1min, path_tmp)


# ------------------------------------------------------------------------------
# expand to a long form (cover each participant-minutes)
# ------------------------------------------------------------------------------

# read from file a temporary state of the data frame 
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "dat_t1min_pre.rds")
dat_t1min <- readRDS(path_tmp)

utc_time_df <- 
  dat_t1min %>% 
  mutate(utc_time_date = as.Date(utc_time)) %>%
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
  time_start_i <- ymd_hms(paste0(utc_time_df$utc_time_date_min[i], " 00:00:00"))
  time_end_i <- ymd_hms(paste0(utc_time_df$utc_time_date_max[i], " 23:59:59"))
  utc_time_i <- seq(time_start_i, time_end_i, by = '1 min')
  out_df_i <- data.frame(utc_time = utc_time_i)
  out_df_i$beiwe_id = beiwe_id_i
  out_df_i$op_sys = op_sys_i
  utc_time_grid_list[[i]] <- out_df_i
}
utc_time_grid_df <- rbindlist(utc_time_grid_list) %>% as.data.frame()

# join date and time grid 
dat_t1min2 <- 
  utc_time_grid_df %>%
  left_join(dat_t1min %>% select(-op_sys), by = c("utc_time", "beiwe_id")) %>%
  mutate(utc_time_date = as.Date(utc_time))
nrow(dat_t1min2)

dat_t1min3 <- 
  dat_t1min2 %>%
  rename(bout_start_cnt = bout_start) %>%
  mutate_at(
    vars(c(bout_start_cnt, screen_time_sec, bout_idx_ndist)),
    .funs = ~ replace_na(., 0)
  ) %>%
  select(
    beiwe_id, 
    op_sys,
    utc_time,
    everything()
  ) %>%
  arrange(
    beiwe_id,
    utc_time
  )



# ------------------------------------------------------------------------------
# add ET time zone
# ------------------------------------------------------------------------------

# rm(dat_t1min)
# rm(dat_t1min2)
et_time_vec <- format(dat_t1min3$utc_time, tz = "America/New_York")
dat_t1min3$et_time <- et_time_vec
dat_t1min3$et_time_date <- as.Date(dat_t1min3$et_time)

dat_t1min4 <- 
  dat_t1min3 %>%
  select(
    beiwe_id,
    op_sys,
    utc_time,
    utc_time_date,
    et_time,
    et_time_date,
    everything()
  )

head(dat_t1min4, 24)


# ------------------------------------------------------------------------------
# save to file 
# ------------------------------------------------------------------------------

dat_F <- dat_t1min4

# save to file 
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_t1min.rds")
saveRDS(dat_F, path_tmp)

