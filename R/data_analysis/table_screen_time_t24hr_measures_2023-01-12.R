
rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(here)
options(digits.secs = 3)
options(scipen = 999)
source(file.path(here(), "R", "config_figures.R"))
source(file.path(here(), "R", "config.R"))
source(file.path(here(), "R", "utils.R"))


# ------------------------------------------------------------------------------
# read data 
# ------------------------------------------------------------------------------

path_tmp <- file.path(here(), 'results_tables', "estimated_screen_time_t24hr_measures.csv")
dat_0 <- fread(path_tmp) %>% as.data.frame()
dat_0
table(dat_0$conv)
head(dat_0) %>% as.data.frame()

dat_1 <- 
  dat_0 %>%
  mutate(
    # replace wearable group with abbreviations
    age_gr = replace(age_gr, age_gr == "adol", "Adolescents"),
    age_gr = replace(age_gr, age_gr == "adult", "Adults"),
    term = replace(term, term == "(Intercept)", "intercept"),
    term = replace(term, term != "intercept", "slope"),
    y_label = recode(y_name, 
                     total_onscreen_dur_min = Y_LABEL_MINUTE_VEC[1], 
                     avg_onscreen_bout_dur_min = Y_LABEL_MINUTE_VEC[2], 
                     avg_offscreen_bout_dur_min = Y_LABEL_MINUTE_VEC[3]),
    x_label = recode(mod_contrast, 
                     relative_week_0_vs_4andmore = "Week 0 (ref.: weeks 4+)", 
                     relative_week_0to3_vs_4andmore = "Weeks 0-3 (ref.: weeks 4+)")
  ) %>%
  rowwise() %>%
  mutate_at(vars(estimate : conf_high), format_num_to_char) %>%
  mutate_at(vars(r2m : r2c), format_num_to_char) %>%
  mutate(
    estimate = replace(estimate, estimate == "-0.000", "0.000")
  ) %>%
  ungroup() %>%
  mutate(
    est_f = paste0(estimate, " [", conf_low, ", ", conf_high, "]"),
    est_f = ifelse(term == "slope", paste0(estimate, " [", conf_low, ", ", conf_high, "] (", p_value, ")"), est_f)
  ) %>% 
  select(
    mod_idx, 
    age_gr, 
    y_label, 
    x_label,
    cnt_subj,
    cnt_data_obs,
    term, 
    est_f,
    r2m, 
    r2c
  ) %>% 
  as.data.frame() 
dat_1

head(dat_1) %>% as.data.frame()
dat_2 <- 
  dat_1 %>%
  pivot_wider(names_from = "term", values_from = "est_f", values_fill = NA) %>%
  select(
    mod_idx,
    y_label,
    x_label,
    age_gr,
    intercept,
    slope,
    r2m,
    r2c,
    cnt_subj,
    cnt_data_obs   
  ) %>%
  arrange(
    x_label,
    mod_idx
  )

head(dat_2) %>% as.data.frame()
dat_2 %>% as.data.frame()


dat_2
names(dat_2) <- c(
  "No.", 
  "Outcome", 
  "Covariate",
  "Subset",
  "Intercept est. [95% CI]",
  "Cov. effect est. [95% CI]",
  "R2m", 
  "R2c",
  "# IDs", 
  "# obs."
)
dat_2

# view(dat_2)




