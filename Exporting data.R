#' ---
#' title: "Export Data"
#' author: "Ian J. Kahrilas"
#' date: "2020/4/14"
#' output: "html_document"
#' ---
#' Load packages
#+ load packages, include = FALSE
library(tidyverse)
library(eegUtils)
library(haven)
library(here)
#'
#' Load data
#+ loading data, echo = FALSE
eeg_df <- read_csv(here("Data", "created_data", "eeg_dat.csv"))
ques_data <- read_sav(here("Data", "ESCW Data_PolnaszekDissertation1.sav"))
# define sub num variable as string
eeg_df <- eeg_df %>% mutate(pid = as.character(pid))
ques_data$SubNum <- sprintf("%03d", ques_data$SubNum)
# subset last three nubmers in eeg pid variable
eeg_df <- eeg_df %>% mutate(pid = str_sub(pid, -3, -1))
# change TRIO variable to factor with properly labeled factors
ques_data <-  ques_data %>% mutate(TRIOGroup = as.factor(TRIOGroup))
levels(ques_data$TRIOGroup) <- c("Control", "Worry", "Anxious Arousal")
#'
#' Define strings for electrode selection
#+ electrode selections
N200_elec <- paste0("EEG", c(52, 53, 54, 58))
N200_elec_revised <- paste0("EEG", c(52, 53, 54))
N450_elec <- paste0("EEG", c(56, 48, 36, 55, 47, 35, 57, 49, 37, 58))
N450_elec_revised <- paste0("EEG", c(56, 48, 36, 55, 47, 35, 57, 49, 37))
SP_elec <- paste0("EEG", c(20, 34, 47, 48, 36, 49, 37))
#'
#' Create separate dataframes for each component
#+ dataframes
# baseline_N200 <- full_df %>%
#   select(all_of(N200_elec),  trial_type:prop_trials, TRIOGroup) %>%
#   filter(trial_type %in% c("pure-incongruent-CT", "pure-congruent-CT")) %>%
#   pivot_longer(., cols = all_of(N200_elec), names_to = "electrode", values_to = "mv") %>%
#   filter(ms < 0) %>%
#   group_by(pid, trial_type, electrode) %>%
#   summarize(baseline = mean(mv, na.rm = TRUE))

N200 <- eeg_df %>%
  select(all_of(N200_elec),  trial_type:prop_trials) %>%
  filter(trial_type %in% c("pure-incongruent-CT", "pure-congruent-CT"),
         between(ms, 220, 360)) %>%
  pivot_longer(., cols = all_of(N200_elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, trial_type) %>%
  summarize(N200_mean = mean(mv, na.rm = TRUE),
            N200_centroid_latency = sum(ms * (mv - min(mv, na.rm = TRUE)), na.rm = TRUE) / sum(mv - min(mv, na.rm = TRUE), na.rm = TRUE),
            n_trials = mean(n_trials),
            total_trials = mean(total_trials),
            prop_trials = mean(prop_trials))

N200_rev <- eeg_df %>%
  select(all_of(N200_elec_revised),  trial_type:prop_trials) %>%
  filter(trial_type %in% c("pure-incongruent-CT", "pure-congruent-CT"),
         between(ms, 360, 472)) %>%
  pivot_longer(., cols = all_of(N200_elec_revised), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, trial_type) %>%
  summarize(N200_revised_mean = mean(mv, na.rm = TRUE),
            N200_revised_centroid_latency = sum(ms * (mv - min(mv, na.rm = TRUE)), na.rm = TRUE) / sum(mv - min(mv, na.rm = TRUE), na.rm = TRUE))

N450 <- eeg_df %>%
  select(all_of(N450_elec),  trial_type:prop_trials) %>%
  filter(trial_type %in% c("pure-incongruent-CT", "pure-congruent-CT"),
         between(ms, 360, 472)) %>%
  pivot_longer(., cols = all_of(N450_elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, trial_type) %>%
  summarize(N450_mean = mean(mv, na.rm = TRUE),
            N450_centroid_latency = sum(ms * (mv - min(mv, na.rm = TRUE)), na.rm = TRUE) / sum(mv - min(mv, na.rm = TRUE), na.rm = TRUE))

N450_rev <- eeg_df %>%
  select(all_of(N450_elec_revised),  trial_type:prop_trials) %>%
  filter(trial_type %in% c("pure-incongruent-CT", "pure-congruent-CT"),
         between(ms, 360, 472)) %>%
  pivot_longer(., cols = all_of(N450_elec_revised), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, trial_type) %>%
  summarize(N450_revised_mean = mean(mv, na.rm = TRUE),
            N450_revised_centroid_latency = sum(ms * (mv - min(mv, na.rm = TRUE)), na.rm = TRUE) / sum(mv - min(mv, na.rm = TRUE), na.rm = TRUE))

SP <- eeg_df %>%
  select(all_of(SP_elec),  trial_type:prop_trials) %>%
  filter(trial_type %in% c("pure-incongruent-CT", "pure-congruent-CT"),
         between(ms, 600, 900)) %>%
  pivot_longer(., cols = all_of(SP_elec), names_to = "electrode", values_to = "mv") %>%
  group_by(pid, trial_type) %>%
  summarize(SP_mean = mean(mv, na.rm = TRUE))
#'
#' Merge all data
#+
cases_to_omit <- c("006", "007", "029", "037", "039", "049", "051", "059", "061", "105", "213", "227", "232", "233", "246", "262", "304", "305", "309")

full_df <- left_join(N200, N200_rev, by = c("pid", "trial_type")) %>%
  left_join(., N450, by = c("pid", "trial_type")) %>%
  left_join(., N450_rev, by = c("pid", "trial_type")) %>%
  left_join(., SP, by = c("pid", "trial_type")) %>%
  full_join(., ques_data, by = c("pid" = "SubNum")) %>%
  select(pid, Age, Race, SEX, trial_type, TRIOGroup, N200_mean,
         N200_revised_mean, N200_centroid_latency, N200_revised_centroid_latency,
         N450_mean, N450_revised_mean, N450_centroid_latency, N450_revised_centroid_latency,
         SP_mean, everything()) %>%
  filter(!(pid %in% cases_to_omit))
glimpse(full_df)
#'
#' Write file
#+ save spss file
write_sav(full_df, here("Data", "ESCW Data_PolnaszekDissertation_EEG.sav"))
# save to workspace
write_csv(full_df, here("Data", "created_data", "full_dat.csv"))

