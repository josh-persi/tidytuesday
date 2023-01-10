# Load packages
library(pacman)

p_load(
  dplyr,
  janitor,
  ggplot2,
  lubridate,
  purrr,
  tidyr,
  tidytuesdayR
)

# Load data
tuesdata <- tt_load("2023-01-10")

counts <- tuesdata |>
  pluck("PFW_2021_public") |>
  clean_names()

sites <- tuesdata |>
  pluck("PFW_count_site_data_public_2021") |>
  clean_names()

rm(tuesdata)

# Create a date column
counts <- counts |>
  unite(date, year, month, day, sep = "-") |>
  mutate(date = as_date(date))

# Plot the number of observations per day
counts |>
  count(date) |>
  ggplot(aes(date, n)) +
  geom_point()
