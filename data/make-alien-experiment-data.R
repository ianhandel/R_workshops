# make alien experiment data

library(tidyverse)
library(writexl)
library(here)

N = 12
n_weeks = 4
n_reps = 3
n_silly = 5

set.seed(11)

# make some aliens
dat <- tibble(subject = 1:N,
              sex = sample(c("mn", "fn", "MN", "female nneutered",
                             "male neutered", "male entire"),
                           N, replace = TRUE),
              age = sample(c(1:12, "6 months", "9 months"),
                           N, replace = TRUE),
              treatment = rep(c("A", "B"), times = N/2),
              colour = sample(c("purple", "red", "orange", "blue"),
                              N, replace = TRUE),
              dummy= 1)
  
  
  # add some results
  dat <- dat %>%
    inner_join(expand.grid(week = 1:n_weeks, rep = 1:n_reps) %>%
               mutate(dummy = 1)) %>% 
  dplyr::select(- dummy) %>% 
  mutate(value = (week - 1) * 2.7 + grepl("n", sex) +
           (treatment == "B") * 2.3 * (week - 1) +
           rnorm(N * n_reps * n_weeks, rep , 2),
         value = round(value, 2),
         row = 1:n())
  
  
  # make some results sily
  dat <- dat %>% 
  mutate(value = case_when(row %in% c(sample(1:n(), n_silly)) ~ value * 100,
                           TRUE ~ value),
         value = case_when(value < 0 ~ 0,
                           TRUE ~ value))
  
  # tidy up and remove alien data after first rows
  dat <- dat %>%
  dplyr::select(- row) %>% 
  mutate(week = paste0("week ", week)) %>% 
  spread(week, value, ) %>% 
  group_by(subject) %>% 
  mutate(sex = case_when(rep == 1 ~ sex,
                         TRUE ~ ""),
         age = case_when(rep == 1 ~ age,
                        TRUE ~ ""),
         treatment = case_when(rep == 1 ~ treatment,
                        TRUE ~ ""),
         colour = case_when(rep == 1 ~ colour,
                        TRUE ~ "")) %>% 
  ungroup() 


write_xlsx(dat, here("data", "data_alien-experiment-untidy_20180310.xlsx"))