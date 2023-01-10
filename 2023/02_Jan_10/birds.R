# https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-01-10
feederwatch <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv'
  )

svg_url <- "https://www.svgrepo.com/show/15359/bird-house.svg"
svg_txt <- paste(readLines(svg_url), collapse = "\n")

library(ggsvg)
library(tidyverse)
library(showtext)
showtext_auto(enable = TRUE)
font_add_google("Abel", "Abel")
font_add_google("Ruda", "Ruda")
showtext_opts(dpi = 200)

feederwatch <- feederwatch |>
  mutate(month_year = paste0(Month, "-", Year)) |>
  mutate(month_year = lubridate::my(month_year))

birds <-
  feederwatch |> select(c(subnational1_code, month_year, how_many, Year))

birbs <- birds |> group_by(subnational1_code, month_year) |>
  summarise(total_birds = sum(how_many))

birbs_us <-
  birbs |> mutate(country = str_extract(subnational1_code, "^\\w\\w")) |>
  filter(country == "US") |>
  mutate(state_short = str_extract(subnational1_code, "\\w\\w$"))

states_inbuilt <-
  data.frame(name = state.name,
             ab = state.abb,
             index = seq.int(1:50))

birbs_usa <-
  left_join(birbs_us, states_inbuilt, by = c("state_short" = "ab")) |>
  na.omit()

birbs_usa |>
  ggplot() +
  geom_point_svg(aes(month_year, fct_reorder(name, -index), size = total_birds), svg = svg_txt) +
  scale_svg_default() +
  scale_size_area(max_size = 10,
                  breaks = c(100, 250, 500, 1000, 2500, 5000)) +
  theme_minimal() +
  labs(
    size = 'Birds Observed',
    title = "Project FeederWatch: Birds of USA ",
    caption = "Data: Project FeederWatch | Graphic : Github.com/SidhuK",
    subtitle = "FeederWatch data (from Nov 2020 to Apr 2021) show which bird species visit feeders at thousands
    of locations across the continent every winter. The data also indicate how many individuals of each
    species are seen. This information can be used to measure changes in the winter ranges and abundances
    of bird species over time."
  ) +
  theme(
    plot.subtitle = element_text(
      family = "Ruda",
      size = 12,
      color = "#0081a7",
      hjust = 0.5,
      face = "bold"
    ),
    plot.title = element_text(
      family = "Ruda",
      size = 30,
      face = "bold",
      color = "#0f4c5c",
      hjust = 0.5
    ),
    plot.caption = element_text(
      family = "Abel",
      size = 12,
      color = "#0f4c5c",
      hjust = 0.5
    ),
    axis.text = element_text(
      family = "Abel",
      size = 12,
      color = "#5f0f40"
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      color = "#5f0f40"
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#0f4c5c", linewidth = .02),
    legend.position = "bottom",
    legend.title = element_text(
      color = "#5f0f40",
      size = 10,
      family = "Abel"
    ),
    legend.text = element_text(
      color = "#5f0f40",
      size = 10,
      family = "Abel"
    ),
    legend.box.margin = margin(t = 30),
    legend.background = element_rect(
      color = "#fb8b24",
      size = 0.1,
      fill = "#fb8b24"
    ),
    legend.key.height = unit(1.25, "lines"),
    legend.key.width = unit(3.5, "lines"),
    plot.margin = margin(rep(20, 4))
  ) + coord_cartesian(clip = "off")

ggsave(
  "birds.png",
  plot = last_plot(),
  width = 10,
  height = 15,
  dpi = 200,
  bg = "#fb8b24"
)