
#' @description 
#' Define statistical analysis sample 
#' 
#' Used variables' values (from source config file, not publicly available)
#' OBSERVATION_PERIOD_DATE_CAP = as.Date("2022-11-18")

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
library(runstats)
options(digits.secs = 3)
options(scipen = 999)

# source config file (not publicly available) with hard-coded values 
source(file.path(here(), "R", "config.R"))

W <- 28
W_FRAC <- 14/28


# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

# meta data 
dat_meta_path <- file.path(here(), "results", "beiwe_id_list_with_dates_2022-11-17.csv")
dat_meta <- 
  fread(dat_meta_path) %>% 
  as.data.frame() %>%
  mutate(
    study_start = as.Date(study_start),
    study_end = as.Date(study_end),
    study_obs_duration = as.numeric(study_end - study_start) + 1,
    # consider date min, date max as one day after / before the study IRB start / end date 
    date_min_meta = study_start,
    # temporarily keep as char
    date_max_meta = as.character(study_end),
    date_max_meta = ifelse(as.Date(date_max_meta) > OBSERVATION_PERIOD_DATE_CAP, as.character(OBSERVATION_PERIOD_DATE_CAP), date_max_meta),
    date_max_meta = as.Date(date_max_meta),
    meta_obs_duration = as.numeric(date_max_meta - date_min_meta) + 1,
    approx_days_after_dc = ifelse(is.na(approx_days_after_dc), 0, approx_days_after_dc)
  )

# valid days data 
valid_day_t24hr_path <- file.path(here(), "data_beiwe_processed", "power_state", "power_state_t24hr_valid_day.rds")
valid_day_t24hr <- readRDS(valid_day_t24hr_path)

# demog data 
demog_df_path <- file.path(here(), 'data_nock_lab', 'data_participants_other_processed', "demog_clean.csv")
demog_df <- 
  fread(demog_df_path) %>% 
  as.data.frame()

# identifiers 
identifiers_df_path <- file.path(here(), 'data_beiwe_processed', 'identifiers', "identifiers_processed.csv")
identifiers_df <- 
  fread(identifiers_df_path) %>% 
  as.data.frame() %>%
  select(beiwe_id, device_os)
head(identifiers_df)
dim(identifiers_df)


# ------------------------------------------------------------------------------
# prepare data
# ------------------------------------------------------------------------------

# expand valid days data  into the long form 
utc_time_date_grid_list <- list()
for (i in 1 : nrow(dat_meta)){ # i <- 204
  print(i)
  beiwe_id_i <- dat_meta$beiwe_id[i] 
  date_min_meta_i <- dat_meta$date_min_meta[i]
  date_max_meta_i <- dat_meta$date_max_meta[i]
  utc_time_date_i <- seq(date_min_meta_i, date_max_meta_i, by = '1 day')
  out_df_i <- data.frame(utc_time_date = utc_time_date_i)
  out_df_i$beiwe_id = beiwe_id_i
  utc_time_date_grid_list[[i]] <- out_df_i
}
utc_time_date_grid_df <- rbindlist(utc_time_date_grid_list) %>% as.data.frame()

# join date and time grid 
valid_day_t24hr_2 <- 
  utc_time_date_grid_df %>%
  left_join(valid_day_t24hr, by = c("utc_time_date", "beiwe_id")) %>%
  mutate(valid_day_utc = ifelse(is.na(valid_day_utc), 0, valid_day_utc))

# check 
!any(is.na(valid_day_t24hr_2))
length(unique(valid_day_t24hr_2$beiwe_id))
# check: should all be 1 except a few NAs
valid_day_t24hr_2 %>%
  group_by(beiwe_id) %>%
  mutate(days_diff = as.numeric(utc_time_date - lag(utc_time_date))) %>%
  pull(days_diff) %>%
  summary()


# ------------------------------------------------------------------------------
# filter participants
# ------------------------------------------------------------------------------

# define running mean calculated out of 0/1-valued flag whether a day is valid 
valid_day_t24hr_3 <- 
  valid_day_t24hr_2 %>%
  group_by(beiwe_id) %>%
  # to calculate running mean
  filter(n() >= W) %>%
  arrange(beiwe_id, utc_time_date) %>%
  mutate(valid_day_utc_rm = runstats::RunningMean(valid_day_utc, W = W, circular = FALSE))
# check
summary(valid_day_t24hr_3$valid_day_utc_rm)

# define a 0/1-valued flag whether a day is within any W days long window such 
# that at least W_FRAC days are within the participant-specific valid day bounds
valid_day_t24hr_4 <- data.frame()
beiwe_id_vec <- unique(valid_day_t24hr_3$beiwe_id)
for (i in 1 : length(beiwe_id_vec)){
  print(paste0("i = ", i))
  beiwe_id_i <- beiwe_id_vec[i]
  df_i <- valid_day_t24hr_3 %>% filter(beiwe_id == beiwe_id_i)
  day_is_included_i <- rep(0, nrow(df_i))
  j_cnt <- (nrow(df_i) - W + 1)
  for (j in 1 : j_cnt){
    W_win_ok <- (df_i$valid_day_utc_rm[j] >= W_FRAC)
    if (W_win_ok){
      idx_ok <- j + seq(0, by = 1, length.out = W)
      day_is_included_i[idx_ok] <- 1
    }
  }
  df_i$day_is_included <- day_is_included_i
  valid_day_t24hr_4 <- rbind(valid_day_t24hr_4, df_i)
}

# final filtering
valid_day_t24hr_5 <- 
  valid_day_t24hr_4 %>%
  filter(valid_day_utc == 1, day_is_included == 1) %>%
  group_by(beiwe_id) %>%
  filter(n() >= W) %>%
  as.data.frame()

# rename  final data frame
valid_day_t24hr_F <- valid_day_t24hr_5
table(valid_day_t24hr_F$valid_day_utc)


# ------------------------------------------------------------------------------
# define final subset of beiwe ID 
# ------------------------------------------------------------------------------

beiwe_id_FINAL <- intersect(
  unique(valid_day_t24hr_F$beiwe_id),
  demog_df$beiwe_id
)


# ------------------------------------------------------------------------------
# save: analysis sample beiwe id with observation period range dates
# ------------------------------------------------------------------------------

sample_beiwe_id_df_F <- 
  dat_meta %>%
  filter(beiwe_id %in% beiwe_id_FINAL) %>%
  left_join(identifiers_df) %>%
  mutate(meta_obs_duration = (as.numeric(difftime(date_max_meta, date_min_meta)) + 1)) %>%
  arrange(beiwe_id) %>%
  mutate(beiwe_id_idx = row_number()) %>%
  mutate(beiwe_id_fct = paste0("ID ", beiwe_id_idx)) %>%
  mutate(beiwe_id_fct = as.factor(beiwe_id_fct))

saveRDS(sample_beiwe_id_df_F, file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds"))


# ------------------------------------------------------------------------------
# save: subset of beiwe ID and utc dates of power_state data to be used in statistical analyses 
# ------------------------------------------------------------------------------

sample_power_state_df_F <- 
  valid_day_t24hr_F %>%
  filter(beiwe_id %in% beiwe_id_FINAL) %>%
  select(beiwe_id, utc_time_date, valid_day_utc) 

saveRDS(sample_power_state_df_F, file.path(here(), "results_objects", "analysis_sample_power_state_t24h.rds"))

