
#' @description
#' Combine and clean Beiwe-derived power state data. 
#' 
#' Apple iOS: Beiwe reports when the participant’s phone screen is locked/unlocked 
#' and if the phone is charging, unplugged, the percentage level of available battery, 
#' when the battery is full or if there is an unknown charging state.
#' 
#' Android: Beiwe reports screen on/off. Researchers can use Power State data to 
#' infer phone usage and if missing data is related to a depleted battery.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(here)
library(janitor)
options(digits.secs = 3)

BEIWE_RAW_DATA_DIR   = file.path(here(), "data_beiwe_raw")


# ------------------------------------------------------------------------------
# read file with Beiwe data start and end 
# ------------------------------------------------------------------------------

beiwe_id_list_path   = file.path(here(), "results",  "beiwe_id_list_with_dates_2022-11-17.csv")
beiwe_id_list_with_dates <-
  fread(beiwe_id_list_path) %>%
  as.data.frame() %>%
  mutate(
    beiwe_download_start_date = as.Date(study_start),
    beiwe_download_end_date = as.Date(study_end)
  )
head(beiwe_id_list_with_dates)


# ------------------------------------------------------------------------------
# PULL POWER STATE DATA 
# ------------------------------------------------------------------------------

# create empty list to store survey_answers responses
beiwe_ids <- list.dirs(BEIWE_RAW_DATA_DIR, recursive = FALSE, full.names = FALSE)
user_dirs <- file.path(BEIWE_RAW_DATA_DIR, beiwe_ids)

dat_list <- list()
dat_col_names <-  c('timestamp','utc_time','event', 'level')
for (i in 1 : length(beiwe_ids)){ 
  beiwe_id <- beiwe_ids[i]
  user_dir <- user_dirs[i]
  print(paste0("i = ", i, ", beiwe_id = ", beiwe_id))
  power_state_path <- file.path(user_dir, "power_state")
  fnames <- list.files(power_state_path, pattern = ".csv", full.names = TRUE)
  if (length(fnames) == 0) next
  message(paste0("length(fnames) = ", length(fnames)))
  dat_tmp_l <- lapply(fnames, fread, colClasses = 'character')
  
  dat_tmp <- rbindlist(dat_tmp_l) %>% as.data.frame()
  if (ncol(dat_tmp) == 3){
    dat_tmp$level = NA
  }
  dat_tmp$beiwe_id = beiwe_id  
  message(paste0("ncol(dat_tmp) = ", ncol(dat_tmp)))
  dat_list[[length(dat_list) + 1]] <- dat_tmp
}
dat_df <- do.call("rbind", dat_list) %>% janitor::clean_names()


# ------------------------------------------------------------------------------
# data cleaning and formatting
# ------------------------------------------------------------------------------

dat_df2 <- 
  dat_df %>%
  mutate(
    utc_time = ymd_hms(utc_time),
    utc_time_date = as.Date(utc_time)
  ) 

dat_df3 <- 
  dat_df2 %>% 
  left_join(beiwe_id_list_with_dates) %>%
  filter(
    utc_time_date >= beiwe_download_start_date,
    utc_time_date <= beiwe_download_end_date
  ) %>%
  select(-c(beiwe_download_start_date, beiwe_download_end_date))

# format event, infer operating system (OS)
ios_events <- c("unplugged", "charging", "full", "unlocked", "locked")
dat_df4 <- 
  dat_df3 %>% 
  mutate(
    event = str_trim(str_replace_all(event, regex("\\W+"), " ")),
    event = gsub(" ", "_", tolower(event)),
    op_sys = ifelse(event %in% ios_events, "ios", "android")
  )

dat_df4_dist <- 
  dat_df4 %>%
  select(beiwe_id, op_sys) %>%
  distinct()
dim(dat_df4_dist)


# ------------------------------------------------------------------------------
# save to file 
# ------------------------------------------------------------------------------

dat_df_F <- dat_df4
path_tmp <- file.path(here(), "data_beiwe_processed", "power_state", "power_state_processed.rds")
saveRDS(dat_df_F, path_tmp)

dat_df_F <- dat_df4_dist
path_tmp <- file.path(here(), "data_beiwe_processed", "power_state", "beiwe_id_op_sys.rds")
saveRDS(dat_df_F, path_tmp)

