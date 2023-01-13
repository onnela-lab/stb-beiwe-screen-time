#' @description 
#' Generate table 1. 

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 0)


# ------------------------------------------------------------------------------
# read and prepare data
# ------------------------------------------------------------------------------

# read sample subset
sample_beiwe_id_df_path <- file.path(here(), 'results_objects', 'analysis_sample_beiwe_id.rds')
sample_beiwe_id_df <- 
  readRDS(sample_beiwe_id_df_path) %>%
  select(beiwe_id, meta_obs_duration, approx_days_after_dc, device_os)
dim(sample_beiwe_id_df)

# read demographics 
demog_df_path <- file.path(here(), 'data_nock_lab', 'data_participants_other_processed', "demog_clean.csv")
demog_df <- 
  fread(demog_df_path) %>% 
  as.data.frame() %>%
  filter(beiwe_id %in% sample_beiwe_id_df$beiwe_id) %>%
  select(beiwe_id, sex, race, age, age_cat)
dim(demog_df)

# read valid power_state days 
sample_power_state_t24h_path <- file.path(here(), "results_objects", "analysis_sample_power_state_t24h.rds")
sample_power_state_t24h <- 
  readRDS(sample_power_state_t24h_path) %>%
  filter(beiwe_id %in% sample_beiwe_id_df$beiwe_id) %>%
  group_by(beiwe_id) %>%
  summarise(power_state_valid_days = n()) %>%
  ungroup()
head(sample_power_state_t24h)
dim(sample_power_state_t24h)


# ------------------------------------------------------------------------------
# combine into one df

comb_df <- 
  sample_beiwe_id_df %>% 
  inner_join(demog_df) %>%
  inner_join(sample_power_state_t24h)
head(comb_df)
dim(comb_df)

# make separate table for adults and adolescents
comb_df_adol <- comb_df %>% filter(age_cat == "adol") 
comb_df_adult <- comb_df %>% filter(age_cat == "adult") 

# check
comb_df %>% arrange(age_cat, age) %>% select(age_cat, age)


# ------------------------------------------------------------------------------
# function to summarize characteristics 
# ------------------------------------------------------------------------------

get_summary_table <- function(dat_tmp){ # dat_tmp <- comb_df
  
  # ------------------------------------------------------------------------------
  # count 
  
  tbl_cnt <- 
    dat_tmp %>%
    summarise(value_f = n()) %>%
    mutate(var_name = "Count all (n)", .before = everything())
  
  
  # ------------------------------------------------------------------------------
  # age 
  
  tbl_age <- 
    dat_tmp %>%
    rename(value = age) %>%
    summarise(
      value_mean = mean(value),
      value_sd = sd(value),
      value_median = median(value),
      value_min = min(value),
      value_max = max(value)
    ) %>%
    mutate(
      value_mean_f = sprintf("%.1f", value_mean),
      value_sd_f = sprintf("%.1f", value_sd),
      value_median_f = sprintf("%.0f", value_median),
      value_min_f = sprintf("%.0f", value_min),
      value_max_f =sprintf("%.0f", value_max),
      value_f = paste0(value_median_f, " [", value_min_f, ", ", value_max_f, "]")
      # value_f = paste0(value_mean_f, " (", value_sd_f, "), ", value_median_f, " [", value_min_f, ", ", value_max_f, "]")
    ) %>%
    select(value_f) %>%
    mutate(var_name = "Age (median [min, max])", .before = everything())
  # tbl_age
  
  
  # ------------------------------------------------------------------------------
  # sex 
  
  # sex_levels <- names(sort(table(dat_tmp$sex), decreasing = TRUE))
  sex_levels <- c("F", "MTF", "F (no she/her pron.)", "M",  "FTM", "M (no he/him pron.)")
  sex_labels <- c(
    "Female (nontransgender, she/her pron. or unspecified)", 
    "Female (transgender)", 
    "Female (nontransgender, other than she/her pron.)", 
    "Male (nontransgender, he/him pron. or unspecified)", 
    "Male (transgender)", 
    "Male (nontransgender, other than he/him pron.)"
    )
  
  tbl_sex <- 
    dat_tmp %>%
    mutate(cnt_all = n()) %>%
    group_by(sex, cnt_all) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    mutate(
      cnt_pct = cnt / cnt_all * 100,
      value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
    ) %>%
    select(value_f, sex) %>%
    rename(var_name = sex) %>%
    mutate(var_name = factor(var_name, levels = sex_levels, labels = sex_labels)) %>%
    right_join(data.frame(var_name = factor(sex_labels, levels = sex_labels)))  %>%
    arrange(var_name) %>%
    mutate(var_name = paste0(var_name, " (n (%))")) %>%
    mutate(value_f = ifelse(is.na(value_f), "0 (0.0%)", value_f))
  # tbl_sex
  
  
  # ------------------------------------------------------------------------------
  # race 
  
  race_levels <- names(sort(table(dat_tmp$race), decreasing = TRUE))
  race_levels <- c(
    race_levels[!(race_levels %in% c("Other", "Unavailable"))],
    c("Other", "Unavailable")
  )
  race_labels <- race_levels
  
  tbl_race <- 
    dat_tmp %>%
    mutate(cnt_all = n()) %>%
    group_by(race, cnt_all) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    mutate(
      cnt_pct = cnt / cnt_all * 100,
      value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
    ) %>%
    select(value_f, race) %>%
    rename(var_name = race) %>%
    mutate(var_name = factor(var_name, levels = race_levels, labels = race_labels)) %>%
    right_join(data.frame(var_name = factor(race_labels, levels = race_labels)))  %>%
    arrange(var_name) %>%
    mutate(var_name = paste0(var_name, " (n (%))")) %>%
    mutate(value_f = ifelse(is.na(value_f), "0 (0.0%)", value_f))
  # tbl_race
  
  
  # ------------------------------------------------------------------------------
  # device_os 
  
  device_os_levels <- names(sort(table(dat_tmp$device_os), decreasing = TRUE))
  device_os_labels <- device_os_levels
  
  tbl_device_os <- 
    dat_tmp %>%
    mutate(cnt_all = n()) %>%
    group_by(device_os, cnt_all) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    mutate(
      cnt_pct = cnt / cnt_all * 100,
      value_f = paste0(sprintf("%.0f", cnt), " (", sprintf("%.1f", cnt_pct), "%)")
    ) %>%
    select(value_f, device_os) %>%
    rename(var_name = device_os) %>%
    mutate(var_name = factor(var_name, levels = device_os_levels, labels = device_os_labels)) %>%
    right_join(data.frame(var_name = factor(device_os_labels, levels = device_os_labels)))  %>%
    arrange(var_name) %>%
    mutate(var_name = paste0(var_name, " (n (%))")) %>%
    mutate(value_f = ifelse(is.na(value_f), "0 (0.0%)", value_f))
  # tbl_device_os
  
  
  # ------------------------------------------------------------------------------
  # meta_obs_duration
  
  tbl_meta_obs_duration <- 
    dat_tmp %>%
    rename(value = meta_obs_duration) %>%
    summarise(
      value_mean = mean(value),
      value_sd = sd(value),
      value_median = median(value),
      value_min = min(value),
      value_max = max(value)
    ) %>%
    mutate(
      value_mean_f = sprintf("%.1f", value_mean),
      value_sd_f = sprintf("%.1f", value_sd),
      value_median_f = sprintf("%.0f", value_median),
      value_min_f = sprintf("%.0f", value_min),
      value_max_f =sprintf("%.0f", value_max),
      value_f = paste0(value_median_f, " [", value_min_f, ", ", value_max_f, "]")
      # value_f = paste0(value_mean_f, " (", value_sd_f, "), ", value_median_f, " [", value_min_f, ", ", value_max_f, "]")
    ) %>%
    select(value_f) %>%
    mutate(var_name = "# days in observation period (median [min, max])", .before = everything())
  # tbl_meta_obs_duration
  
  
  # ------------------------------------------------------------------------------
  # power_state_valid_days
  
  tbl_power_state_valid_days <- 
    dat_tmp %>%
    rename(value = power_state_valid_days) %>%
    summarise(
      value_mean = mean(value),
      value_sd = sd(value),
      value_median = median(value),
      value_min = min(value),
      value_max = max(value)
    ) %>%
    mutate(
      value_mean_f = sprintf("%.1f", value_mean),
      value_sd_f = sprintf("%.1f", value_sd),
      value_median_f = sprintf("%.0f", value_median),
      value_min_f = sprintf("%.0f", value_min),
      value_max_f =sprintf("%.0f", value_max),
      value_f = paste0(value_median_f, " [", value_min_f, ", ", value_max_f, "]")
      # value_f = paste0(value_mean_f, " (", value_sd_f, "), ", value_median_f, " [", value_min_f, ", ", value_max_f, "]")
    ) %>%
    select(value_f) %>%
    mutate(var_name = "# smartphone state logs -- valid data days (median [min, max])", .before = everything())
  # tbl_power_state_valid_days
  
  
  # ------------------------------------------------------------------------------
  # combine altogether 
  
  dat_tmp_out <- 
    tbl_cnt %>%
    rbind(tbl_age) %>%
    rbind(tbl_sex) %>%
    rbind(tbl_race) %>%
    rbind(tbl_device_os) %>%
    rbind(tbl_meta_obs_duration) %>%
    rbind(tbl_power_state_valid_days) 
  
  return(dat_tmp_out)
}


# ------------------------------------------------------------------------------
# generate summary tables
# ------------------------------------------------------------------------------

table_out_all <- get_summary_table(comb_df) %>% rename(value_f_all = value_f)
table_out_adol <- get_summary_table(comb_df_adol) %>% rename(value_f_adol = value_f)
table_out_adult <- get_summary_table(comb_df_adult) %>% rename(value_f_adult = value_f)

table_out_comb <- 
  table_out_adol %>% 
  full_join(table_out_adult) %>%
  full_join(table_out_all)
table_out_comb

# view(table_out_comb)

# ------------------------------------------------------------------------------
# save to file: table 1
# ------------------------------------------------------------------------------

tbl_out_path <- file.path(here(), 'results_tables', "table_1.csv")
fwrite(table_out_comb, tbl_out_path)


