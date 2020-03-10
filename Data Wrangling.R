#' ---
#' title: "Data Wrangling"
#' author: "Ian J. Kahrilas"
#' date: "2020/3/10"
#' output: "pdf_document"
#' ---

#+ read in data
library(tidyverse)
#'
#' Create vector with all mul file names
#+ vector of all files in working directory
mul_names <- list.files(pattern = ".mul")
# preallocate space
vector("list", length(mul_names))
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
mul_files <- map_df(mul_names, ~ {
  read_table2(.x, skip = 1) %>%
  mutate(trial = rep(trial_names,
                     each = (nrow(.) / length(trial_names))
                     )
         )
  }
  )

#' 1600 ms
# this read all .mul files into a list and generates pid, block, and ms variables
mul_files <- map(mul_names, ~ {
  read_table2(.x, skip = 1) %>%
    mutate(
      pid = as.numeric(str_extract(.x, "[0-9]{7,}")),
      block = rep(block_order, each = nrow(.) / 7),
      ms = rep(seq(from = -200, to = 3000,
                   by = ((3200 + (3200 / (nrow(.)/7))) / (nrow(.)/7))),
               times = 7)
    )
}
)

mul_tbl <- reduce(mul_files, full_join)

glimpse(evt_tbl)
glimpse(mul_tbl)

# clean up block names
evt_tbl$block <- str_remove(evt_tbl$block, "42\t200000\t")
evt_tbl$block <- map_chr(evt_tbl$block, ~ {
  str_replace_all(.x, "Positive", "Pos") %>%
    str_replace_all(., "Negative", "Neg") %>%
    str_replace_all(., "Neutral", "Neu") %>%
    str_replace_all(., "Increase", "Inc") %>%
    str_replace_all(., "Decrease", "Dec")
}
)

# clean up electrode names in mul_tbl
names(mul_tbl) <- gsub("_.*", "", names(mul_tbl))



erp <- full_join(mul_tbl, evt_tbl, by = c("pid", "block"))
# can also drop x72 and x73 columns, as they don't contain anything!
erp <- erp %>% select(-c(X74, X73, X72))
# fix incorrect pid
erp$pid[erp$pid == 201206832] <- 206201832
write_csv(erp, "erp_data.csv")
