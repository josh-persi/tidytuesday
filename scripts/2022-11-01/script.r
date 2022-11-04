library(pacman)

p_load(
  dplyr,
  ggplot2,
  lubridate,
  showtext,
  tidytuesdayR
)

# tuesdata <- tt_load("2022-11-01")$horror_movies

font_add_google("Nosifer")
showtext_auto()

# Try to find the greatest drop off here
tuesdata |> 
  group_by(collection_name) |> 
  arrange(desc(release_date)) |> 
  slice_head(n = 2) |> 
  View()

ggplot(tuesdata, aes(x = year(release_date), y = vote_average)) +
  stat_summary(geom = "line", size = 2) +
  
  labs(
    title = "Have horror movies gone downhill?",
    subtitle = "Average score per year on a 10-point sclae",
    x = "Year",
    y = "Average Score"
  ) +
  lims(y = c(0, 10)) + 
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = , family = "Nosifer"),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.margin = unit(c(0.25, 0.5, 0.25, 0.5), "in")
        )

ggsave("scripts/2022-11-01/plot.png", width = 16, height = 8)
