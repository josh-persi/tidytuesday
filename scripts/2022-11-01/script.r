# Load packages
library(pacman)

p_load(
  dplyr,
  ggplot2,
  lubridate,
  scales,
  showtext,
  stringr,
  tidytuesdayR
)

# Add google fonts
font_add_google("Nosifer")
font_add_google("Atkinson Hyperlegible")
showtext_auto()

# Load data
tuesdata <- tt_load("2022-11-01")$horror_movies

# Filter to movies that are released, part of a collection, and have > 100 votes
tuesdata <- filter(
  tuesdata,
  !is.na(collection_name),
  status == "Released",
  vote_count > 100
  ) 

# Select the columns needed for graphing
tuesdata <- select(tuesdata, collection_name, title, release_date, vote_average)

# Calculate the change in viewer rating between the first and last installment
tuesdata <- tuesdata |> 
  group_by(collection_name) |> 
  arrange(collection_name, release_date) |> 
  mutate(diff = first(vote_average) - last(vote_average))

# Select collections that have >= 3 installments and a positive difference
tuesdata <- tuesdata |> 
  filter(n() >= 3 & diff > 0) |> 
  ungroup() |> 
  arrange(desc(diff))

# Select the top 5 collections 
top_5_collections <- tuesdata |>
  distinct(collection_name) |> 
  head(5) |> 
  pull(collection_name)
  
my_plot <- ggplot(
  (plot_data |> 
    filter(collection_name %in% top_5_collections) |> 
    mutate(collection_name = str_remove(collection_name, " Collection"))
  ),
  aes(x = year(release_date), y = vote_average)
  ) +
  stat_summary(geom = "line", size = 1, colour = "#790B07") + 
  
  labs(
    title = "The Death of a Franchise",
    subtitle = "Horror movie franchises with the greatest drop in viewer rating from the first to last installment ",
    x = "Year",
    y = "Average Score",
    caption = "Made by: Josh Persi\nData Source: The Movie Database (TMDB)"
  ) +
  
  theme_minimal() + 
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 32,
      colour = "#AE200A",
      family = "Nosifer"
      ),
    text = element_text(family = "Atkinson Hyperlegible", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 16, colour = "#CABEA6"),
    plot.caption = element_text(colour = "#CABEA6"),
    plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "in"),
    plot.background = element_rect(fill = "#131314"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "#CABEA6"),
    axis.title = element_text(colour = "#CABEA6"),
    axis.line = element_line(colour = "#CABEA6"),
    axis.ticks = element_line(colour = "#CABEA6"),
    strip.text = element_text(colour = "#CABEA6", face = "bold")
        ) + 
  facet_grid(~ collection_name, scales = "free_x") + 
  scale_x_continuous(breaks = breaks_pretty(n = 3))

ggsave("scripts/2022-11-01/plot.png", width = 16, height = 9, units = "cm")

