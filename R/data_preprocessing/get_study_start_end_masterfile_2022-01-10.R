
#' @description 
#' Get a masterfile with study start and end for a participant.

rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(here)
options(digits.secs = 0)
options(scipen = 999)

# source config file (not publically available) with hard-coded values to keep them 
# anonymous
source(file.path(here(), "R", "config.R"))


# ------------------------------------------------------------------------------
# util functions
# ------------------------------------------------------------------------------

# function to preprocess values from Excel date columns 
excel_numeric_to_date1 <- function(val){
  val_upd <- ifelse(is.na(val), NA, substr(val, 1, 10))
  val_upd <- as.Date(val_upd)
  val_upd <- as.character(val_upd)
  return(val_upd)
}

# function to preprocess values from Excel date columns 
excel_numeric_to_date2 <- function(val){
  val_upd <- ifelse(is.na(val), "", val)
  val_upd = ifelse(!(nchar(val_upd) == 5 & startsWith(val_upd, "4")), NA, val_upd)
  val_upd = excel_numeric_to_date(as.numeric(as.character(val_upd)), date_system = "modern")
  val_upd <- as.character(val_upd)
  return(val_upd)
}

# function to preprocess values from Excel date columns 
excel_numeric_to_date3 <- function(val){
  val_upd <- ifelse(is.na(val), "", val)
  val_upd = ifelse(!(startsWith(as.character(val_upd), "4")), NA, val_upd)
  val_upd = excel_numeric_to_date(floor(as.numeric(as.character(val_upd))), date_system = "modern")
  val_upd <- as.character(val_upd)
  return(val_upd)
}


# ------------------------------------------------------------------------------
# read and clean external data: study start and end 
# ------------------------------------------------------------------------------

ext_file_path <- file.path(here(), "data_nock_lab", "data_participants_other", "BeiweID_11_9_2022.xlsx")
ext_file0 <- 
  read_excel(ext_file_path, sheet = 1) %>%
  as.data.frame() %>%
  clean_names() 

# remove participants to be exluses 
ext_file1 <- 
  ext_file0 %>% 
  filter(is.na(exclude_p))

dim(ext_file0)
dim(ext_file1)

# format dates
ext_file2 <- 
  ext_file1 %>% 
  mutate(study_start = excel_numeric_to_date3(study_start)) 

# fix one that has been likely manually mis-inserted in the google sheet
# (use variables from the config file -- not publicly available to keep them anonymous)
ext_file2 %>% filter(is.na(study_start))
ext_file2 %>% filter(is.na(study_end))
ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_1, "study_start"] = "2020-09-25" 
ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_2, "study_start"] = "2021-11-19" 

# those are yielding >168 days, cap at 168
# (use variables from the config file -- not publicly available to keep them anonymous)
ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_3, "study_end"] = as.Date(ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_3, "study_start"]) + 168
ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_4, "study_end"] = as.Date(ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_4, "study_start"]) + 168
ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_5, "study_end"] = as.Date(ext_file2[ext_file2$beiwe_id == BEIWE_ID_FIX_5, "study_start"]) + 168

# calculate difference between the two dates
ext_file2 <- 
  ext_file2 %>%
  mutate(obs_duration = as.numeric(as.Date(study_end) - as.Date(study_start)))
summary(ext_file2$obs_duration)

# subset colums 
ext_file3 <- 
  ext_file2 %>%
  mutate(study_start = as.Date(study_start)) %>%
  mutate(study_end = as.Date(study_end))
str(ext_file3)
head(ext_file3)
dim(ext_file3)


# ------------------------------------------------------------------------------
# save file with Beiwe ID with study start and end dates  

ext_file_F <- ext_file3 

dat_F_path <- file.path(here(), "results", "beiwe_id_list_with_dates_2022-11-17.csv")
fwrite(ext_file_F, dat_F_path)


# ------------------------------------------------------------------------------
# read and clean external data: demographics
# ------------------------------------------------------------------------------

# adolescents
dat_demog_fc_path <- file.path(here(), "data_nock_lab", "data_participants_other", "fc_u01_participant_enrollment_demog.csv")
dat_demog_fc <- 
  fread(dat_demog_fc_path) %>%
  as.data.frame() %>%
  clean_names() 
head(dat_demog_fc)

# adults
dat_demog_nofc_path <- file.path(here(), "data_nock_lab", "data_participants_other", "u01_participant_enrollment_demog.csv")
dat_demog_nofc <- 
  fread(dat_demog_nofc_path) %>%
  as.data.frame() %>%
  clean_names() 
head(dat_demog_nofc)


# ------------------------------------------------------------------------------
# uniform column subset 

dat_demog_1 <-
  dat_demog_fc %>%
  select(beiwe_id, sex = sex2_new, race = race2_new, age = age_new) %>%
  mutate(age_cat = "adol") %>%
  filter(!is.na(beiwe_id), !(trimws(beiwe_id) == ""))

dat_demog_2 <-
  dat_demog_nofc %>%
  select(beiwe_id, sex = sex_new, race = race2_new, age = age_new) %>%
  mutate(age_cat = "adult") %>%
  filter(!is.na(beiwe_id), !(trimws(beiwe_id) == ""))

# combine
dat_demog <- 
  dat_demog_1 %>%
  rbind(dat_demog_2) %>%
  group_by(beiwe_id) %>%
  mutate(beiwe_id_is_duplicate = ifelse(n() > 1, 1, 0)) %>%
  ungroup()
dim(dat_demog)
length(unique(dat_demog$beiwe_id))
table(dat_demog$beiwe_id_is_duplicate)


# ------------------------------------------------------------------------------
# check for duplicate beiwe ID 

aa <- 
  dat_demog_1 %>% group_by(beiwe_id) %>% filter(n() > 1) %>% ungroup() %>%
  arrange(beiwe_id) %>% as.data.frame()
head(aa)

aa <- 
  dat_demog_2 %>% group_by(beiwe_id) %>% filter(n() > 1) %>% ungroup() %>%
  arrange(beiwe_id) %>% as.data.frame()
head(aa)


# ------------------------------------------------------------------------------
# save demog

fwrite(dat_demog, file.path(here(), "data_nock_lab", "data_participants_other_processed", "demog_clean.csv"))


# ------------------------------------------------------------------------------
# create masterfile with the above info combined
# ------------------------------------------------------------------------------

dat_demog_tojoin <- 
  dat_demog %>%
  group_by(beiwe_id) %>%
  # remove 1 beiwe_id duplicate case
  filter(row_number() == 1) %>%
  ungroup()
dim(dat_demog_tojoin)

beiwe_masterfile <- 
  (ext_file_F %>% mutate(has_start_end = 1)) %>%
  full_join((dat_demog_tojoin %>% mutate(has_demog = 1)), by = "beiwe_id") %>%
  mutate(
    has_start_end = ifelse(is.na(has_start_end), 0, has_start_end),
    has_demog = ifelse(is.na(has_demog), 0, has_demog)
  )
dim(beiwe_masterfile)
head(beiwe_masterfile)
table(beiwe_masterfile$has_start_end, beiwe_masterfile$has_demog)

table(beiwe_masterfile$sex)
table(beiwe_masterfile$race)


# ------------------------------------------------------------------------------
# save

fwrite(beiwe_masterfile, file.path(here(), "data_nock_lab", "data_participants_other_processed", "beiwe_id_masterfile.csv"))

