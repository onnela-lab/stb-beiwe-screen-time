
#' @description
#' Estimate on-screen time events. The resukted data frame has one row per one 
#' on-screen bout, keeping its start time and duration.
#' 
#' For iOS OS, the the timing and duration of on-screen bouts was estimated from 
#' the time intervals between subsequent "unlocked" and "locked" logs timestamps. 
#' During logs preprocessing, we perform:
#' -  missing logs imputation,
#' -  windsorizing on-screen bouts longer than 30 minutes. 
#' 
#' For Android OS, the estimation was based on the time intervals between 
#' subsequent "screen turned on" and "screen turned off" logs timestamps. 
#' During logs preprocessing, we performem: 
#' -  missing logs imputation,
#' -  windsorizing on-screen bouts longer than 30 minutes,
#' - removing bouts attributed to a notification arrival. 
#' 
#' Our preprocessing steps were based on and are very similar to those presented 
#' in Kristensen et al. (2022) (https://www.sciencedirect.com/science/article/pii/S2451958821001123). 


rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 3)
options(scipen=999)
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# read data 

dat_path <- file.path(here(), "data_beiwe_processed", "power_state", "power_state_processed.rds")
dat0 <- readRDS(dat_path) 

# make separate data frames with android / ios devices only
dat <- dat0 %>% filter(event %in% c("screen_turned_on", "locked", "screen_turned_off", "unlocked"))
dat_adr <- dat %>% filter(op_sys == "android")
dat_ios <- dat %>% filter(op_sys == "ios")


# ------------------------------------------------------------------------------
# PROCESS IOS
# ------------------------------------------------------------------------------

beiwe_id_vec <- sort(unique(dat_ios$beiwe_id))
dat_ios_2 <- 
  dat_ios %>%
  mutate(utc_time = ymd_hms(utc_time)) %>%
  arrange(beiwe_id, utc_time) %>%
  group_by(beiwe_id) %>%
  mutate(event_next = lead(event)) %>%
  mutate(utc_time_next = lead(utc_time)) %>%
  as.data.frame()


# ------------------------------------------------------------------------------
# correct ("locked","locked"), ("unlocked","unlocked") cases

dat_ios_3 <- dat_ios_2

# "unlocked" event is missing
corr_seq_df <- dat_ios_2 %>% filter(event == "locked", event_next == "locked")
corr_seq_df_upd <- data.frame()
for (i in 1 : nrow(corr_seq_df)){ # i <- 1
  print(paste0(i, "/", nrow(corr_seq_df)))
  corr_seq_i_upd <- corr_seq_df[i, ]
  corr_seq_i_upd$event[1] <- "unlocked"
  utc_time_diff <- as.numeric(difftime(corr_seq_df$utc_time_next[i], corr_seq_df$utc_time[i], unit = "secs"))
  if (utc_time_diff > SCREEN_TIME_IMPUTE_SEC){
    utc_time_i <- corr_seq_df$utc_time_next[i] - seconds(SCREEN_TIME_IMPUTE_SEC)
  } else {
    utc_time_i <- corr_seq_df$utc_time_next[i] - seconds(utc_time_diff/2)
  }
  corr_seq_i_upd$utc_time[1] <- utc_time_i
  corr_seq_df_upd <- rbind(corr_seq_df_upd, corr_seq_i_upd)
}
dat_ios_3 <- rbind(dat_ios_3, corr_seq_df_upd)
nrow(dat_ios_3) - nrow(dat_ios_2)
nrow(dat_ios_3) / nrow(dat_ios_2)

# stats values to paper methods section
val1 <- dat_ios_2 %>% filter(event == "locked", event_next == "locked")
val2 <- dat_ios_2 %>% filter(event == "locked", event_next == "unlocked")
round(nrow(val1) / (nrow(val1) + nrow(val2)) * 100, 2)

# "locked" event is missing
corr_seq_df <- dat_ios_2 %>% filter(event == "unlocked", event_next == "unlocked")
corr_seq_df_upd <- data.frame()
for (i in 1 : nrow(corr_seq_df)){ # i <- 32
  print(paste0(i, "/", nrow(corr_seq_df)))
  corr_seq_i_upd <- corr_seq_df[i, ]
  corr_seq_i_upd$event[1] <- "locked"
  utc_time_diff <- as.numeric(difftime(corr_seq_df$utc_time_next[i], corr_seq_df$utc_time[i], unit = "secs"))
  if (utc_time_diff > SCREEN_TIME_IMPUTE_SEC){
    utc_time_i <- corr_seq_df$utc_time[i] + seconds(SCREEN_TIME_IMPUTE_SEC)
  } else {
    utc_time_i <- corr_seq_df$utc_time[i] + seconds(utc_time_diff/2)
  }
  corr_seq_i_upd$utc_time[1] <- utc_time_i
  corr_seq_df_upd <- rbind(corr_seq_df_upd, corr_seq_i_upd)
}
dat_ios_3 <- rbind(dat_ios_3, corr_seq_df_upd)
nrow(dat_ios_3) - nrow(dat_ios_2)
nrow(dat_ios_3) / nrow(dat_ios_2)

# stats values to paper methods section
val1 <- dat_ios_2 %>% filter(event == "unlocked", event_next == "unlocked")
val2 <- dat_ios_2 %>% filter(event == "unlocked", event_next == "locked")
round(nrow(val1) / (nrow(val1) + nrow(val2)) * 100, 2)


# ------------------------------------------------------------------------------
# cap on-screen bout duration at 30 minutes

dat_ios_4 <-
  dat_ios_3 %>%
  arrange(beiwe_id, utc_time) %>%
  group_by(beiwe_id) %>%
  mutate(event_next = lead(event)) %>%
  mutate(utc_time_next = lead(utc_time)) %>%
  filter(event == "unlocked", !is.na(event_next)) %>%
  mutate(screen_time_sec = as.numeric(difftime(utc_time_next, utc_time, unit = "secs")))

dat_ios_5 <- 
  dat_ios_4 %>%
  mutate(screen_time_sec = ifelse(screen_time_sec > SCREEN_TIME_CAP_SEC, SCREEN_TIME_CAP_SEC, screen_time_sec))
  
# stats values to paper methods section
round(mean(dat_ios_4$screen_time_sec > SCREEN_TIME_CAP_SEC) * 100, 2)


# ------------------------------------------------------------------------------
# PROCESS ANDROID
# ------------------------------------------------------------------------------

dat_adr_2 <- 
  dat_adr %>%
  mutate(utc_time = ymd_hms(utc_time)) %>%
  arrange(beiwe_id, utc_time) %>%
  group_by(beiwe_id) %>%
  mutate(event_next = lead(event)) %>%
  mutate(utc_time_next = lead(utc_time)) %>%
  as.data.frame()


# ------------------------------------------------------------------------------
# correct ("screen_turned_off","screen_turned_off"), ("screen_turned_on","screen_turned_on") cases

dat_adr_3 <- dat_adr_2

# "screen_turned_on" event is missing
corr_seq_df <- dat_adr_2 %>% filter(event == "screen_turned_off", event_next == "screen_turned_off")
corr_seq_df_upd <- data.frame()
for (i in 1 : nrow(corr_seq_df)){
  if (nrow(corr_seq_df) == 0) break
  print(paste0(i, "/", nrow(corr_seq_df)))
  corr_seq_i_upd <- corr_seq_df[i, ]
  corr_seq_i_upd$event <- "screen_turned_on"
  utc_time_diff <- as.numeric(difftime(corr_seq_df$utc_time_next[i], corr_seq_df$utc_time[i], unit = "secs"))
  if (utc_time_diff > SCREEN_TIME_IMPUTE_SEC){
    utc_time_i <- corr_seq_df$utc_time_next[i] - seconds(SCREEN_TIME_IMPUTE_SEC)
  } else {
    utc_time_i <- corr_seq_df$utc_time_next[i] - seconds(utc_time_diff/2)
  }
  corr_seq_i_upd$utc_time[1] <- utc_time_i
  corr_seq_df_upd <- rbind(corr_seq_df_upd, corr_seq_i_upd)
}
dat_adr_3 <- rbind(dat_adr_3, corr_seq_df_upd)
nrow(dat_adr_3) - nrow(dat_adr_2)
nrow(dat_adr_3) / nrow(dat_adr_2)

# stats values to paper methods section
val1 <- dat_adr_2 %>% filter(event == "screen_turned_off", event_next == "screen_turned_off")
val2 <- dat_adr_2 %>% filter(event == "screen_turned_off", event_next == "screen_turned_on")
round(nrow(val1) / (nrow(val1) + nrow(val2)) * 100, 2)

# "screen_turned_off" event is missing
corr_seq_df <- dat_adr_2 %>% filter(event == "screen_turned_on", event_next == "screen_turned_on")
corr_seq_df_upd <- data.frame()
for (i in 1 : nrow(corr_seq_df)){ # i <- 1
  print(paste0(i, "/", nrow(corr_seq_df)))
  corr_seq_i_upd <- corr_seq_df[i, ]
  corr_seq_i_upd$event <- "screen_turned_off"
  utc_time_diff <- as.numeric(difftime(corr_seq_df$utc_time_next[i], corr_seq_df$utc_time[i], unit = "secs"))
  if (utc_time_diff > SCREEN_TIME_IMPUTE_SEC){
    utc_time_i <- corr_seq_df$utc_time[i] + seconds(SCREEN_TIME_IMPUTE_SEC)
  } else {
    utc_time_i <- corr_seq_df$utc_time[i] + seconds(utc_time_diff/2)
  }
  corr_seq_i_upd$utc_time[1] <- utc_time_i
  corr_seq_df_upd <- rbind(corr_seq_df_upd, corr_seq_i_upd)
}
dat_adr_3 <- rbind(dat_adr_3, corr_seq_df_upd)
nrow(dat_adr_3) - nrow(dat_adr_2)
nrow(dat_adr_3) / nrow(dat_adr_2)

# stats values to paper methods section
val1 <- dat_adr_2 %>% filter(event == "screen_turned_on", event_next == "screen_turned_on")
val2 <- dat_adr_2 %>% filter(event == "screen_turned_on", event_next == "screen_turned_off")
round(nrow(val1) / (nrow(val1) + nrow(val2)) * 100, 2)

dat_adr_4 <-
  dat_adr_3 %>%
  arrange(beiwe_id, utc_time) %>%
  group_by(beiwe_id) %>%
  mutate(event_next = lead(event)) %>%
  mutate(utc_time_next = lead(utc_time)) %>%
  as.data.frame()


# ------------------------------------------------------------------------------
# difference in times between the events; cap the time at half an hour 

dat_adr_5a <- 
  dat_adr_4 %>%
  filter(event == "screen_turned_on", !is.na(event_next)) %>%
  mutate(screen_time_sec = as.numeric(difftime(utc_time_next, utc_time, unit = "secs"))) 

# stats values to paper methods section
dat_adr_5 <- 
  dat_adr_5a %>%
  mutate(screen_time_sec = ifelse(screen_time_sec > SCREEN_TIME_CAP_SEC, SCREEN_TIME_CAP_SEC, screen_time_sec))


# ------------------------------------------------------------------------------
# remove repetitive notifications of certain time length (bouts attributed to a notification arrival)

dat_adr_6 <- 
  dat_adr_5 %>%
  group_by(beiwe_id) %>%
  mutate(
    utc_time_date = as.Date(utc_time),
    day_relative = as.numeric(difftime(utc_time_date, min(utc_time_date), units = c("days"))),
    week_relative = as.numeric(difftime(utc_time_date, min(utc_time_date), units = c("weeks"))),
    utc_time_date_floor1wk = floor(week_relative),
    screen_time_sec_1dec = round(screen_time_sec, 1)
  ) %>%
  ungroup() %>%
  select(-c(utc_time_date, day_relative, week_relative))

dat_adr_6_notif <- 
  dat_adr_6 %>%
  mutate(
    screen_time_sec_1dec = round(screen_time_sec, 1)
  ) %>%
  group_by(
    beiwe_id, 
    utc_time_date_floor1wk,
    screen_time_sec_1dec
  ) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  group_by(beiwe_id, utc_time_date_floor1wk) %>%
  mutate(
    cnt_all = sum(cnt),
    frac_all = round(cnt / cnt_all, 3)
  ) %>%
  ungroup() %>% 
  filter(
    screen_time_sec_1dec <= 40,
    cnt >= 3,
    frac_all > 0.01
  ) %>%
  as.data.frame()
dim(dat_adr_6_notif)
# [1] 4732    6   # 2022-09-12
# [1] 5008    6   # 2022-09-12

dat_adr_6_notif_rm <- 
  dat_adr_6_notif %>%
  select(beiwe_id, utc_time_date_floor1wk, screen_time_sec_1dec) %>%
  mutate(to_rm = 1)

dat_adr_7 <- 
  dat_adr_6 %>%
  left_join(dat_adr_6_notif_rm, by = c("beiwe_id", "utc_time_date_floor1wk", "screen_time_sec_1dec")) %>%
  mutate(to_rm = ifelse(is.na(to_rm), 0, to_rm))

# stats values to paper methods section
mean(dat_adr_7$to_rm)
mean(dat_adr_7$to_rm) * 100

dat_adr_8 <- 
  dat_adr_7 %>%
  filter(to_rm == 0)

# stats values to paper methods section
mean(dat_adr_8$screen_time_sec == SCREEN_TIME_CAP_SEC) * 100


# ------------------------------------------------------------------------------
# save to file
# ------------------------------------------------------------------------------

dat_ios_F <- 
  dat_ios_5 %>% 
  select(
    beiwe_id, 
    utc_time,
    op_sys,
    event,
    screen_time_sec
  )

dat_adr_F <- 
  dat_adr_8 %>% 
  select(
    beiwe_id, 
    utc_time,
    op_sys,
    event,
    screen_time_sec
  )

dat_comb <- 
  rbind(dat_ios_F, dat_adr_F) %>%
  mutate(utc_time_date = as.Date(utc_time)) %>%
  select(beiwe_id, utc_time, utc_time_date, everything()) %>%
  arrange(beiwe_id, utc_time)

# save to file 
dat_comb_F <- dat_comb
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "estimated_screen_time.rds")
saveRDS(dat_comb_F, path_tmp)


