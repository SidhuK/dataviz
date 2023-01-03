library(tidyverse)
library(showtext)
font_add_google("Overpass", "Overpass")
showtext.auto(enable = TRUE)
showtext.opts(dpi = 250)

age <- read_csv("age.csv") |> janitor::clean_names()

age_new <- age |> head(51)

theme_set(theme_minimal(base_family = "Overpass"))

age_new <-
  age_new |> mutate(median_age_2 = median_age - difference)

color_pal = "#579BB1"
  
age_new |>
  ggplot() +
  ggforce::geom_link(
    aes(
      x = median_age,
      y = reorder(name,-index),
      xend = median_age_2,
      yend = name,
      size = after_stat(index),
      alpha = after_stat(index)
    ),
    color = color_pal
  ) +
  geom_point(
    aes(x = median_age_2, y = name),
    color = color_pal,
    shape = 21,
    size = 6,
    fill = "#F3F3F3"
  ) +
  geom_vline(xintercept = 26.3, linetype = "dashed") +
  geom_vline(xintercept = 28, linetype = "dashed") +
  geom_text(
    data = data.frame(x = 26.4, y = 15,
                      label = "Nationwide Median 2006-10"),
    mapping = aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 5,
    angle = 90,
    family = "Overpass"
  ) +
  geom_text(
    data = data.frame(x = 28.1, y = 32,
                      label = "Nationwide Median 2015-19"),
    mapping = aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 5,
    angle = 90,
    family = "Overpass"
  ) +
  labs(
    title = "Age of First Marriage for Different US States",
    subtitle = "The chart contains information on the median age at first marriage for women in the United States. The information was collected from 2006 to 2010 \n and then again from 2015 to 2019. Every state saw an increase in the age at which women first got married. The National median age also increased \n from 26.3 years in 2006-10 to 28 years in 2015-19",
    caption = "Data: Andy Kriebel/Kaggle | Graphic: Github.com/SidhuK",
    x = "Median Age (Years)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 27, face = "bold"),
    plot.caption = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 9)
  )

ggsave(
  "age.png",
  plot = last_plot(),
  width = 12,
  height = 15,
  dpi = 250,
  units = "in",
  bg = "white"
)
