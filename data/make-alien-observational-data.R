# make alien observational data

library(tidyverse)
library(writexl)
library(here)
library(babynames)

n <- 200
N <- 1000
n_years <- 100
colours <- c("red", "green", "orange", "purple")
colour_prob <- c(1, 4, 2, 6)
sexes <- c("male", "female", "m", "F", "MALE", "Female")
sex_probs <- c(10, 10, 1, 2, 1, 1)
healths <- c("healthy", "slightly spotty", "spotty", "very spotty")
regions <- c("Northland", "Southland", "Eastland", "Westland")


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
  name = sample(
    babynames$name,
    n,
    replace = TRUE
  ),
  sex = sample(
    sexes,
    size = n,
    prob = sex_probs,
    replace = TRUE
  ),
  region1 = factor(sample(
    regions,
    size = n,
    replace = TRUE
  )),
  growth_rate = rnorm(n, 10, 2),
  assym = rnorm(n, 100, 10)
)

# give them ages and delete some, tending to be older ones
dat <- dat %>%
  mutate(age = round(rgamma(n, 2, 0.1), 2))

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
    iron = as.numeric(colour == colours[1]) * 3 +
      sqrt(age) +
      as.numeric(region1) * 4 +
      str_detect(sex, "^m|^M") * 2 +
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
  mutate(weight = assym - 50 * exp(-age / growth_rate) +
    str_detect(sex, "^m|^M") * 15 +
    rnorm(n(), 5, 2),
    weight = round(weight, 2))

# make some ages in months
dat <- dat %>%
  mutate(age = case_when(
    age < 2 ~ str_c(sample(
      1:23,
      n(),
      replace = TRUE
    ), " months"),
    TRUE ~ as.character(round(age))
  ))

# remove temporary columns
dat <- dat %>%
  select(-assym, -growth_rate)


# make some iron data 'missing'
dat <- dat %>%
  mutate(iron = case_when(
    runif(n(), 0, 1) > 0.95 ~ "*",
    TRUE ~ as.character(iron)
  ))

# capitalise SOME colours
dat <- dat %>%
  mutate(colour = case_when(
    runif(n(), 0, 1) > 0.80 ~ str_to_title(colour),
    TRUE ~ colour
  ))

# muck up the column names
dat <- dat %>%
  rename(
    `IQ - ref to human 100 scale!` = IQ,
    `glucose - mg/L` = glucose
  )

# shuffle rows, sort on region, insert blanks
dat <- dat %>%
  sample_frac() %>%
  arrange(region1) %>%
  group_by(region1) %>%
  mutate(
    rep = 1:length(region1),
    region = case_when(
      rep == 1 ~ unique(region1),
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup() %>%
  select(-rep, -region1)

# join sex and colour
dat <- dat %>%
  unite("type", c("sex", "colour"), sep = "_" )

dat <- dat %>%
  select(region, subject, name, everything())

# write it!
write_xlsx(dat, here("data", "aliendata_20180331.xlsx"))