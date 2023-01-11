# Load packages
library(pacman)

p_load(
  dplyr,
  janitor,
  ggplot2,
  leafgl,
  leaflet,
  lubridate,
  purrr,
  sf,
  stringr,
  tidyr,
  tidytuesdayR,
  tigris
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

# Plot the number of observations per day. Some dates had a much higher number
# of observations.
counts |>
  count(date) |>
  ggplot(aes(date, n)) +
  geom_point()

# The days with over 1000 observations were all saturdays
counts |> 
  count(date) |>
  filter(n > 1000) |>
  mutate(weekday = wday(date, label = TRUE), .before = everything()) |> 
  count(weekday)

# Observations are in the U.S. and Canada. Note the single point left of 
# Australia
leaflet() |> 
  addTiles() |> 
  addGlPoints(
    data = st_as_sf(counts, coords = c("longitude", "latitude"), crs = 4326)
    )

# Download states geometries   
states <- states()

# Select the name and geometry columns and reproject to WGS84
states <- states |> 
  select(name = NAME, geometry) |> 
  st_transform(crs = 4326)

# States like Alaska and Hawaii are included 
leaflet() |> 
  addTiles() |> 
  addPolygons(data = states, label = ~ name)

# Filter out non-continuous U.S. states
states <- filter(
  states, 
  !name %in% c(
    "Alaska", 
    "American Samoa",
    "Commonwealth of the Northern Mariana Islands",
    "Guam",
    "Hawaii",
    "Puerto Rico",
    "United States Virgin Islands"
    )
  )

# States like Alaska and Hawaii are included 
leaflet() |> 
  addTiles() |> 
  addPolygons(data = states, label = ~ name)

# Try to reformat the state codes and sum observations per state and species
counts |> 
  group_by(subnational1_code, species_code) |> 
  summarize(how_many = sum(how_many)) |> 
  filter(str_detect(subnational1_code, "US-")) |> 
  mutate(
    name = subnational1_code |> 
      str_remove("US-") |> 
      str_replace_all(state.abb, state.name)
    )

# Code the projection for the US National Atlas Equal Area projection
epsg2163 <- str_c(
  "+proj=laea",
  "+lat_0=45",
  "+lon_0=-100",
  "+x_0=0",
  "+y_0=0",
  "+a=6370997",
  "+b=6370997",
  "+units=m",
  "+no_defs", 
  sep = " "
)

# An initial plot
ggplot() +
  geom_sf(data = st_transform(states, epsg2163))
