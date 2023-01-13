
#' @description 
#' Get t24hr (day-level) screen time daily measures (UTC time zone).

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
library(ActFrag)
library(entropy)
options(digits.secs = 3)
options(scipen=999)


# ------------------------------------------------------------------------------
# read data 
# ------------------------------------------------------------------------------
# read from file
path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_t1min.rds")
dat <- readRDS(path_tmp)


# ------------------------------------------------------------------------------
# define util functions
# ------------------------------------------------------------------------------

agg_vals_func <- function(vals){
  # helper objects
  vals_df <- data.frame(
    vals = vals,
    vals_time = seq(1 : 1440) ,
    vals_hour = floor((seq(1 : 1440) - 1) / 60)
  )
  any_non0_vals_df <- 
    vals_df %>%
    group_by(vals_hour) %>%
    summarise(any_non0_vals = as.numeric(sum(vals) > 0)) %>%
    as.data.frame()
  # number of hours between first and last hour with any screen activity
  any_non0_vals_hours_span <- 
    any_non0_vals_df %>%
    filter(any_non0_vals > 0) %>%
    summarise(result = max(vals_hour) - min(vals_hour)) %>%
    pull(result)
  if (any_non0_vals_hours_span == -Inf) any_non0_vals_hours_span <- NA
  # number of hours with any screen activity
  any_non0_vals_hours_cnt <- 
    any_non0_vals_df %>%
    pull(any_non0_vals) %>%
    sum()
  # fragmentation metrics
  vals_02 <- as.integer(ifelse(vals > 0, 2, 0))
  out_list_tmp <- fragmentation(x = vals_02, w = rep(1, 1440), thresh = 1, metrics = "all")
  freqs <- table(vals_02)/length(vals_02)
  out_list_tmp[["entropy"]] <- entropy.empirical(freqs, unit = "log2")
  out_list_tmp[["screen_time_sec"]] <- sum(vals)
  out_list_tmp[["any_non0_vals_hours_span"]] <- any_non0_vals_hours_span
  out_list_tmp[["any_non0_vals_hours_cnt"]] <- any_non0_vals_hours_cnt
  # final object
  out_df_tmp <- do.call("cbind", out_list_tmp) %>% as.data.frame()
  out_df_tmp <- round(out_df_tmp, 5)
  return(out_df_tmp)
}


# ------------------------------------------------------------------------------
# run preprocessing
# ------------------------------------------------------------------------------

dat_unique <- dat %>% select(beiwe_id, utc_time_date) %>% distinct()
dat_unique_nrow <- nrow(dat_unique)


## allocate memory 
t1 <- Sys.time()
out_list <- vector(mode = "list", length = dat_unique_nrow)
for (i in 1 : dat_unique_nrow){ # i <- 1
# for (i in 28329 : dat_unique_nrow){ # i <- 28329
  print(paste0(i, " / ", dat_unique_nrow, " -- ", round(100 * i / dat_unique_nrow, 2), "%"))
  idx_vec <- (1440 * (i - 1) + 1) : (1440 * i)
  try({
    dat_tmp <- dat[idx_vec, ]
    cond = (hour(dat_tmp$utc_time[1]) == 0) & (minute(dat_tmp$utc_time[1]) == 0)
    if (!cond) stop("!cond")
    agg_vals_tmp <- agg_vals_func(dat_tmp$screen_time_sec)
    # append other info
    agg_vals_tmp$beiwe_id <- dat_tmp$beiwe_id[1]
    agg_vals_tmp$utc_time_date <- dat_tmp$utc_time_date[1]
    agg_vals_tmp$op_sys <- dat_tmp$op_sys[1]
    out_list[[i]] <- agg_vals_tmp
  })
}
t2 <- Sys.time()
t2 - t1
# Time difference of 15.62671 mins

out_df <- data.table::rbindlist(out_list) %>% as.data.frame()
nrow(out_df)

out_df_F <- 
  out_df %>%
  select(
    beiwe_id, 
    op_sys, 
    utc_time_date, 
    any_non0_vals_hours_cnt, 
    any_non0_vals_hours_span,
    screen_time_sec,
    everything()
    )
dim(out_df_F)


# ------------------------------------------------------------------------------
# save to file 
# ------------------------------------------------------------------------------

path_tmp <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_t24h_utc.rds")
saveRDS(out_df_F, path_tmp)


