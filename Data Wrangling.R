#' ---
#' title: "Data Wrangling"
#' author: "Ian J. Kahrilas"
#' date: "2020/3/10"
#' output: "pdf_document"
#' ---

#+ read in data
library(tidyverse)
library(here)
#'
#' Create vector with all mul file names
#+ vector of all files in working directory
mul_names <- list.files(here("Data"), pattern = "mul")
evt_names <- list.files(here("Data"), pattern = "evt")
#'
#' Trials names
#+ list of trial names
trial_names <- c("fixation",
                 "congruent-block-CT",
                 "incongruent-block-CT",
                 "neutral-block-CT",
                 "pure-congruent-CT",
                 "pure-incongruent-CT",
                 "neutral-congruent-CT",
                 "netural-incongruent-CT",
                 "congruent-block-AT",
                 "incongruent-block-AT",
                 "neutral-block-AT",
                 "pure-congruent-AT",
                 "pure-incongruent-AT",
                 "neutral-congruent-AT",
                 "neutral-incongruent-AT"
                 )
#' Read in files
#+ map over all file names
mul_files <- map2_df(mul_names, evt_names, ~ {
  mul <- read_table2(here("Data", .x), skip = 1) %>%
  mutate(trial = rep(trial_names,
                     each = (nrow(.) / length(trial_names))
                     ),
         pid = str_extract(.x, "[0-9]{6}"),
         ms = rep(seq(-200, 1400,
                      by = ((1600 + (1600 / 400))/ 400)),
                  times = 15)
         )
  evt <- read_table(here("Data", .y)) %>%
    rename("n_trials" = `Code\tTriNo\tComnt`) %>%
    mutate(n_trials = as.numeric(str_extract(n_trials, pattern = "[0-9]{2,}")),
           pid = str_extract(.y, "[0-9]{6}")
    ) %>%
    select(-Tmu)
  full_join(mul, evt, by = "pid")
  }
  )
