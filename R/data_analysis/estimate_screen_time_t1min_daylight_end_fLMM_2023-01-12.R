
rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 3)
options(scipen = 999)
source(file.path(here(), "R", "data_analysis", "lfos3s.R"))
source(file.path(here(), "R", "config.R"))
source(file.path(here(), "R", "utils.R"))


# ------------------------------------------------------------------------------
# params 
# ------------------------------------------------------------------------------

DAYS_DIFF_VALUE_MAX_VEC <- c(7, 14, 28)


# ------------------------------------------------------------------------------
# read data 
# ------------------------------------------------------------------------------

# read actual data (minute-level)
t1 <- Sys.time()
dat_t1min_path <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_t1min.rds")
dat_t1min <- readRDS(dat_t1min_path) 
t2 <- Sys.time()
t2 - t1

# read subset of beiwe ID and utc dates of power_state data to be used in statistical analyses 
dat_sample_power_state_t24h_path <- file.path(here(), "results_objects", "analysis_sample_power_state_t24h.rds")
dat_sample_power_state_t24h <- readRDS(dat_sample_power_state_t24h_path)

# join to have only valid days 
dat_t1min_F <- 
  dat_t1min %>% 
  inner_join(dat_sample_power_state_t24h) %>%
  select(beiwe_id, utc_time, screen_time_sec) %>%
  mutate(utc_time_date = as.Date(utc_time))


# ------------------------------------------------------------------------------
# match daylight time data frame with screen time data 

dates_vec <- c(
  "2020-03-08",
  "2021-03-14",
  "2022-03-13",
  "2019-11-03", 
  "2020-11-01", 
  "2021-11-06"
)
dates_vec <- as.Date(dates_vec)
dates_type_vec <- c(rep("daylight_start", 3), rep("daylight_end", 3))
daylight_df <- data.frame(daylight_time_date = dates_vec, daylight_type = dates_type_vec)
daylight_df$daylight_time_date_idx <- 1 : nrow(daylight_df)
daylight_df

for (DAYS_DIFF_VALUE_MAX in DAYS_DIFF_VALUE_MAX_VEC){ # DAYS_DIFF_VALUE_MAX <- 7

  dat_comb <- data.frame()
  daylight_df_nrow <- nrow(daylight_df)
  for (i in 1 : nrow(daylight_df)){ # i <- 1
    dat_t1min_F$daylight_time_date <- daylight_df$daylight_time_date[i]
    dat_t1min_sub <- 
      dat_t1min_F %>% 
      mutate(days_diff = as.numeric(utc_time_date - daylight_time_date)) %>%
      filter(days_diff >= (-DAYS_DIFF_VALUE_MAX), days_diff <= DAYS_DIFF_VALUE_MAX) %>%
      mutate(
        utc_time_date_wday = weekdays(utc_time_date),
        daylight_time_date_wday = weekdays(daylight_time_date)
      )
    dat_t1min_sub$daylight_type <- daylight_df$daylight_type[i]
    dat_t1min_sub$daylight_time_date_idx <- daylight_df$daylight_time_date_idx[i]
    dat_comb <- rbind(dat_comb, dat_t1min_sub)
  }
  dim(dat_comb)
  
  # ------------------------------------------------------------------------------
  # prepare data modeling subsets
  
  weekdays_vec <- c("Monday", "Tuesday", "Wednesday", "Friday", "Thursday" )
  unique(dat_comb$utc_time_date_wday)
  
  mod_df0 <- 
    dat_comb %>%
    filter(utc_time_date_wday %in% weekdays_vec) %>%
    mutate(after_change = ifelse(days_diff > 0, 1, 0)) %>%
    group_by(daylight_time_date, beiwe_id) %>%
    filter(n_distinct(after_change) == 2) %>%
    ungroup() 
  
  mod_df <- 
    mod_df0 %>%
    group_by(beiwe_id, utc_time_date) %>%
    filter(n() == 1440) %>%
    arrange(beiwe_id, utc_time_date, utc_time) %>%
    mutate(index_within = row_number()) %>%
    mutate(index_within = paste0("V", index_within)) %>%
    ungroup()
  
  
  # ------------------------------------------------------------------------------
  # RUN MODEL 1
  # ------------------------------------------------------------------------------
  
  mod_df_w <- 
    mod_df %>% 
    filter(daylight_type == "daylight_end") %>%
    mutate(y = as.integer(ifelse(screen_time_sec > 0, 1, 0))) %>%
    select(beiwe_id, utc_time_date, y, index_within, after_change) %>%
    pivot_wider(names_from = index_within, values_from = y)
  dim(mod_df_w)
  length(unique(mod_df_w$beiwe_id))

  # make data frame with data specific to this loop iteration
  dat_df_tmp    <- as.data.frame(mod_df_w)
  fit_Y_tmp     <- dat_df_tmp %>% select(starts_with("V")) %>% as.matrix()
  fit_dat_tmp   <- dat_df_tmp %>% select(-starts_with("V"))
  fit_dat_tmp$Y <- fit_Y_tmp
  dim(fit_Y_tmp)
  
  B_boot = 100
  argvals_manual = seq(1, 1440, by = 5)
  knots_manual = 20
  
  # fit model
  t1 <- Sys.time()
  fit_tmp <- lfosr3s(
    formula = as.formula(Y ~ after_change + (1 | beiwe_id)),
    data = fit_dat_tmp,
    family = "binomial",
    argvals_manual = argvals_manual, 
    var = TRUE, 
    parallel = FALSE, 
    silent = FALSE, 
    B_boot = B_boot, 
    knots_manual = knots_manual, 
    msg_prefix = ""
  )
  t2 <- Sys.time()
  print(t2-t1)
  
  # save fit object
  fit_fname <- paste0("fit_result_lfosr3s_daylight_end_", DAYS_DIFF_VALUE_MAX, ".rds")
  fit_fpath <- file.path(here(), "results_objects", fit_fname)
  saveRDS(fit_tmp, fit_fpath)
}






