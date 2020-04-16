#' ---
#' title: "ERP Plots"
#' author: "Ian J. Kahrilas"
#' date: "2020/4/14"
#' output: "html_document"
#' ---
#+ Load packages, include = FALSE
library(tidyverse)
library(eegUtils)
library(here)
#'
#' read in data
#+ read in eeg data, incude = FALSE
eeg_df <- read_csv(here("Data", "created_data", "eeg_dat.csv"))
#'
#' define clusters of electrodes and time windows for each component
#+ electrode clusters and time windows
# clusters
N200_elec <- paste0("EEG", c(52, 53, 54, 58))
N450_elec <- paste0("EEG", c(56, 48, 36, 55, 47, 35, 57, 49, 37))
SP_elec <- paste0("EEG", c(20, 34, 47, 48, 36, 49, 37))
#'
#' Create plots for each component with all conditions
#+ plot creation
erp_plot_fun <- function(cluster, comp_name, time_window_low, time_window_high) {
eeg_df %>%
  select(all_of(cluster),  trial_type:prop_trials) %>%
  filter(trial_type %in% c("pure-incongruent-CT", "pure-congruent-CT")) %>%
  pivot_longer(., cols = cluster, names_to = "electrode", values_to = "mv") %>%
  mutate(ms = round(ms, digits = -0.8)) %>%
  group_by(trial_type, ms) %>%
  summarize(mv = mean(mv, na.rm = TRUE)) %>%
  ggplot(., aes(ms, mv, linetype = trial_type)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = c(time_window_low, time_window_high), linetype = "solid", size = 1.05) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude ( ",mu,"V)")),
       title = paste("Average", comp_name, "Waveforms")) +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "line"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 16))+
  scale_linetype_discrete(name = "Trial Type",
                          breaks = c("pure-incongruent-CT", "pure-congruent-CT"),
                          labels = c("Incongruent", "Congruent"))
}
#'
#' Use pmap to iterate plotting function over list of parameters.
#+ iterate and plot
plots <- pmap(list(cluster = list(N200_elec,
                N450_elec,
                SP_elec),
          comp_name = c("N200",
                "N450",
                "SP"),
          time_window_low = c(220,
                360,
                600),
          time_window_high = c(320,
                472,
                900)),
     .f = erp_plot_fun)
#'
#' save images to workspace
#+ save the images
walk2(plots, c("N200", "N450", "SP"), ~{
  ggsave(plot = .x, filename = here("Images", paste0(.y, ".png")), device = "png", width = 8, height = 5, scale = 1.5)
})
