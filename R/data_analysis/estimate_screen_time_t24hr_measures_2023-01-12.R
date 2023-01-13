
#' @description 
#' The script performs what follows: 
#' - Estimate population-level values
#' - Estimate population-level values, effect of OS 
#' - Estimate population-level values, effect of relative week 0 vs weeks 4+
#' - Estimate population-level values, effect of relative weeks 0-3 vs weeks 4+


rm(list = ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
library(lme4)
library(lmerTest)
library(MuMIn)
library(broom.mixed)
options(digits.secs = 3)
options(scipen = 999)
source(file.path(here(), "R", "config_figures.R"))
source(file.path(here(), "R", "config.R"))
source(file.path(here(), "R", "utils.R"))


# ------------------------------------------------------------------------------
# read data 
# ------------------------------------------------------------------------------

# read sample
dat_sample_beiwe_id_path <- file.path(here(), "results_objects", "analysis_sample_beiwe_id.rds")
dat_sample_beiwe_id_0 <- readRDS(dat_sample_beiwe_id_path) 
dat_sample_beiwe_id <- 
  dat_sample_beiwe_id_0 %>% 
  select(beiwe_id, device_os, adol_adult, date_min_meta, approx_days_after_dc, beiwe_id_fct)

# read subset of beiwe ID and utc dates of power_state data to be used in statistical analyses 
dat_sample_power_state_t24h_path <- file.path(here(), "results_objects", "analysis_sample_power_state_t24h.rds")
dat_sample_power_state_t24h <- readRDS(dat_sample_power_state_t24h_path)

# read actual data 
dat_t24h_path <- file.path(here(), "data_beiwe_processed",  'power_state', "screen_time_measures_t24h.rds")
dat_t24h <- 
  readRDS(dat_t24h_path) %>%
  inner_join(dat_sample_power_state_t24h, by = c("beiwe_id", "utc_time_date")) 
# check
dim(dat_t24h)
# [1] 12095     7

length(unique(dat_t24h$beiwe_id))


# ------------------------------------------------------------------------------
# prepare data for modeling
# ------------------------------------------------------------------------------

dat_t24h_2 <- 
  dat_t24h %>%
  left_join(dat_sample_beiwe_id, by = "beiwe_id") %>%
  mutate_at(all_of(Y_NAME_VEC), function(x) x/60) %>%
  rename_at(all_of(Y_NAME_VEC), ~Y_NAME_MINUTE_VEC) %>%
  mutate(
    relative_day = as.numeric(utc_time_date - date_min_meta),
    relative_week = floor(relative_day / 7) ,
    relative_month = floor(relative_day / 30.5),
    relative_week_0 = ifelse(relative_week == 0, 1, 0),
    relative_week_0to3 = ifelse(relative_week %in% c(0, 1, 2, 3), 1, 0) ,
    relative_week_4andmore = ifelse(relative_week >= 4, 1, 0)
  ) %>%
  mutate(
    relative_week_0_fct = factor(relative_week_0, levels = c(0,1)),
    relative_week_0to3_fct = factor(relative_week_0to3, levels = c(0,1)),
    relative_week_4andmore_fct = factor(relative_week_4andmore, levels = c(0,1)),
    op_sys_fct = factor(op_sys, levels = c("ios", "android"), labels = c("iOS", "Android"))
  )

# rename
dat_t24h_F <- dat_t24h_2
head(dat_t24h_F) %>% as.data.frame()

table(dat_t24h_F$relative_week_0_fct)
table(dat_t24h_F$relative_week_0to3_fct)
table(dat_t24h_F$relative_week_4andmore_fct)


# ------------------------------------------------------------------------------
# util function
# ------------------------------------------------------------------------------

get_mod_summary_df <- function(mod_df, mod_formula){
  # model fit
  mod_control   <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))
  mod_out       <- lmer(mod_formula, data = mod_df, control = mod_control)
  mod_out_r2    <- round(r.squaredGLMM(mod_out), 3)
  mod_out_conv  <- merMod_has_converged(mod_out)
  # other stats
  cnt_subj <- length(unique(mod_df$beiwe_id))
  cnt_data_obs <- nrow(mod_df)
  # create data frame with model estimation results, append
  out_df      <- 
    # lme4::confint.merMod
    tidy(mod_out, conf.int = TRUE, conf.level = 0.95, conf.method = "Wald") %>% 
    filter(is.na(group)) %>% 
    select(-c(effect, group))%>%
    mutate(
      R2m = mod_out_r2[1],
      R2c = mod_out_r2[2],
      conv = mod_out_conv,
      cnt_subj = cnt_subj,
      cnt_data_obs = cnt_data_obs,
      .before = everything()
    ) 
  return(out_df)
}

MOD_IDX <- 1


# ------------------------------------------------------------------------------
# model 1: mean values
# ------------------------------------------------------------------------------

# data frame with parameters grid
mod_param_df <- expand.grid(
  age_gr = c("adol", "adult"), 
  y_name = Y_NAME_MINUTE_VEC
)
mod_contrast  <- NA
mod_formula   <- as.formula(y ~ 1 + (1  | beiwe_id_fct))


# ------------------------------------------------------------------------------

mod_summary_df <- data.frame()
# iterate over params 
for (i in 1 : nrow(mod_param_df)){ # i <- 1
  print(i)
  # model data frame 
  y_name <- mod_param_df$y_name[i]
  age_gr <- mod_param_df$age_gr[i]
  mod_idx <- mod_param_df$mod_idx[i]
  mod_df <- 
    dat_t24h_F %>% 
    filter(adol_adult == age_gr) %>%
    rename_with(~ c("y"), all_of(y_name)) %>%
    filter(!is.na(y)) 
  # estimate fit, append
  mod_summary_df_tmp  <- 
    get_mod_summary_df(mod_df, mod_formula) %>%
    mutate(
      mod_idx = MOD_IDX,
      y_name = y_name, 
      age_gr = age_gr,
      mod_contrast = mod_contrast,
      .before = everything()
    ) 
  mod_summary_df <- rbind(mod_summary_df, mod_summary_df_tmp)
  MOD_IDX <- MOD_IDX + 1
}
mod_summary_df_1 <- mod_summary_df; rm(mod_summary_df)


# ------------------------------------------------------------------------------
# model 2: mean values
# ------------------------------------------------------------------------------

# data frame with parameters grid
mod_param_df <- expand.grid(
  age_gr = c("adol", "adult"), 
  y_name = Y_NAME_MINUTE_VEC
)
mod_contrast  <- "android_vs_ios"
mod_formula   <- as.formula(y ~ 1 + op_sys_fct + (1  | beiwe_id_fct))


# ------------------------------------------------------------------------------

mod_summary_df <- data.frame()
# iterate over params 
for (i in 1 : nrow(mod_param_df)){ # i <- 1
  print(i)
  # model data frame 
  y_name <- mod_param_df$y_name[i]
  age_gr <- mod_param_df$age_gr[i]
  mod_idx <- mod_param_df$mod_idx[i]
  mod_df <- 
    dat_t24h_F %>% 
    filter(adol_adult == age_gr) %>%
    rename_with(~ c("y"), all_of(y_name)) %>%
    filter(!is.na(y)) 
  # estimate fit, append
  mod_summary_df_tmp  <- 
    get_mod_summary_df(mod_df, mod_formula) %>%
    mutate(
      mod_idx = MOD_IDX,
      y_name = y_name, 
      age_gr = age_gr,
      mod_contrast = mod_contrast,
      .before = everything()
    ) 
  mod_summary_df <- rbind(mod_summary_df, mod_summary_df_tmp)
  MOD_IDX <- MOD_IDX + 1
}
mod_summary_df_2 <- mod_summary_df; rm(mod_summary_df)


# ------------------------------------------------------------------------------
# model 3: relative_week_0_vs_4andmore
# ------------------------------------------------------------------------------

mod_contrast  <- "relative_week_0_vs_4andmore"
mod_formula   <- as.formula(y ~ 1 + relative_week_0_fct + (1  + relative_week_0_fct | beiwe_id_fct))


# ------------------------------------------------------------------------------

mod_summary_df <- data.frame()
# iterate over params 
for (i in 1 : nrow(mod_param_df)){ # i <- 1
  print(i)
  # model data frame 
  y_name <- mod_param_df$y_name[i]
  age_gr <- mod_param_df$age_gr[i]
  mod_idx <- mod_param_df$mod_idx[i]
  mod_df <- 
    dat_t24h_F %>% 
    filter(relative_week_0 == 1 | relative_week_4andmore == 1) %>%
    filter(adol_adult == age_gr) %>%
    rename_with(~ c("y"), all_of(y_name)) %>%
    filter(!is.na(y)) %>%
    group_by(beiwe_id) %>%
    # at least 2 observations in each of the two groups
    filter(sum(relative_week_0) >= 2)  %>% 
    filter(sum(relative_week_4andmore) >= 2) %>%
    ungroup()
  # estimate fit, append
  mod_summary_df_tmp  <- 
    get_mod_summary_df(mod_df, mod_formula) %>%
    mutate(
      mod_idx = MOD_IDX,
      y_name = y_name, 
      age_gr = age_gr,
      mod_contrast = mod_contrast,
      .before = everything()
    ) 
  mod_summary_df <- rbind(mod_summary_df, mod_summary_df_tmp)
  MOD_IDX <- MOD_IDX + 1
}
mod_summary_df_3 <- mod_summary_df; rm(mod_summary_df)



# ------------------------------------------------------------------------------
# model 4: relative_week_0_vs_4andmore
# ------------------------------------------------------------------------------

mod_contrast  <- "relative_week_0to3_vs_4andmore"
mod_formula   <- as.formula(y ~ 1 + relative_week_0to3_fct + (1  + relative_week_0to3_fct | beiwe_id_fct))


# ------------------------------------------------------------------------------

mod_summary_df <- data.frame()
# iterate over params 
for (i in 1 : nrow(mod_param_df)){ # i <- 1
  print(i)
  # model data frame 
  y_name <- mod_param_df$y_name[i]
  age_gr <- mod_param_df$age_gr[i]
  mod_idx <- mod_param_df$mod_idx[i]
  mod_df <- 
    dat_t24h_F %>% 
    filter(relative_week_0to3 == 1 | relative_week_4andmore == 1) %>%
    filter(adol_adult == age_gr) %>%
    rename_with(~ c("y"), all_of(y_name)) %>%
    filter(!is.na(y)) %>%
    group_by(beiwe_id) %>%
    # at least 2 observations in each of the two groups
    filter(sum(relative_week_0to3) >= 2)  %>% 
    filter(sum(relative_week_4andmore) >= 2) %>%
    ungroup()
  # estimate fit, append
  mod_summary_df_tmp  <- 
    get_mod_summary_df(mod_df, mod_formula) %>%
    mutate(
      mod_idx = MOD_IDX,
      y_name = y_name, 
      age_gr = age_gr,
      mod_contrast = mod_contrast,
      .before = everything()
    ) 
  mod_summary_df <- rbind(mod_summary_df, mod_summary_df_tmp)
  MOD_IDX <- MOD_IDX + 1
}
mod_summary_df_4 <- mod_summary_df; rm(mod_summary_df)


# ------------------------------------------------------------------------------
# combine
# ------------------------------------------------------------------------------

mod_summary_df_all <- 
  mod_summary_df_1 %>%
  rbind(mod_summary_df_2) %>%
  rbind(mod_summary_df_3) %>%
  rbind(mod_summary_df_4)


# ------------------------------------------------------------------------------
# save: data frame with model fit summary
# ------------------------------------------------------------------------------

mod_tidy_df_all <- 
  janitor::clean_names(mod_summary_df_all) %>% 
  mutate_at(vars(r2m : r2c), round, 3) %>%
  mutate_at(vars(estimate : conf_high), round, 3) %>%
  as.data.frame()
head(mod_tidy_df_all) %>% as.data.frame()

path_tmp <- file.path(here(), 'results_tables', "estimated_screen_time_t24hr_measures.csv")
fwrite(mod_tidy_df_all, path_tmp)

