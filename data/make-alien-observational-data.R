# make alien observational data

library(tidyverse)
library(writexl)
library(here)

n <- 100
N <- 1000
n_years <- 100
colours <- c("red", "blue", "orange", "purple")
colour_prob <- c(1, 4, 2, 6)
sexes <- c("male", "female", "m", "F", "MALE", "Female")
sex_probs <- c(10, 10, 1, 2, 1, 1)
healths <- c("healthy", "slightly spotty", "spotty", "very spotty")


set.seed(11)

# make some aliens
dat <- tibble(
  subject = 1:n,
  colour = sample(
    colours,
    size = n,
    prob = colour_prob,
    replace = TRUE
  ),
  sex = sample(
    sexes,
    size = n,
    prob = sex_probs,
    replace = TRUE
  ),
  growth_rate = rnorm(n, 10, 2),
  assym = rnorm(n, 100, 10)
)

# give them ages and delete some, tending to be older ones
dat <- dat %>%
  crossing(tibble(age = 1:n_years)) %>%
  filter(sqrt(age) < log(runif(n(), 1, n_years * 5))) %>% 
  distinct()

# add healths, older aliens less healthy
dat <- dat %>%
  mutate(
    health = round(age / max(age) * 3 + runif(n(), 0, 3)),
    health = health / max(health) * 3 + 1,
    health = factor(healths[health], levels = healths)
  )

# add glucose data
dat <- dat %>%
  mutate(glucose = age / 10 +
    str_detect(sex, "^m|^M") * 3 +
    as.numeric(health) +
    rnorm(n(), 10, 2)) %>%
  mutate(glucose = round(glucose, 2))

# add iron data
dat <- dat %>%
  mutate(
    iron = as.numeric(colour == colours[1]) +
      sqrt(age) +
      rnorm(n(), 20, 1),
    iron = round(iron, 1)
  )

# add IQ data
dat <- dat %>%
  mutate(
    IQ = iron * 11 +
      age * 7 +
      str_detect(sex, "^f|^F") +
      rnorm(n(), 100, 5),
    IQ = round(IQ)
  )

# add weight data
dat <- dat %>% 
  mutate(weight = assym -  50 * exp(- age / growth_rate) +
           rnorm(n(), 5, 2))

# put blanks after first subject info
dat <- dat %>%
  arrange(subject, age) %>%
  group_by(subject) %>%
  mutate(
    rep = 1:length(subject),
    sex = case_when(
      rep == 1 ~ sex,
      TRUE ~ ""
    ),
    colour = case_when(
      rep == 1 ~ colour,
      TRUE ~ ""
    )
  ) %>%
  ungroup() %>%
  select(-rep)

# make some ages in months
dat <- dat %>%
  mutate(age = case_when(
    age < 2 ~ str_c(sample(
      1:23,
      n(),
      replace = TRUE
    ), " months"),
    TRUE ~ as.character(age)
  ))

write_xlsx(dat, here("data", "data_alien-observational-untidy_20180310.xlsx"))