library(ggplot2)



library(ggstream)
library(tidyverse)
library(patchwork)
library(showtext)
showtext_opts(dpi = 450)
showtext_auto(enable = TRUE)
library(ggtext)

font_add_google(family = "Bangers", name = "Bangers")



capacity <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv"
  )
wind <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv"
  )
solar <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv"
  )
average_cost <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv"
  )

capacity <- capacity %>%
  mutate(across(where(anyNA), ~ replace_na(., 0)))

capacity <- capacity %>%
  mutate(prior = standalone_prior + hybrid_prior) %>%
  mutate(new = standalone_new + hybrid_new)

# https://github.com/davidsjoberg/ggstream


colors_legend <- c(
  "#FFB327",
  "#D1F1F9",
  "#4F607C",
  "#c5b689",
  "#8E038E",
  "#5A6D87",
  "#000000"
)




plot1 <- ggplot(capacity, aes(year, prior, fill = type)) +
  geom_stream(
    extra_span = .25,
    true_range = "none",
    bw = .85,
    size = 1.25,
    sorting = "onset"
  ) +
  geom_stream(
    geom = "contour",
    color = "white",
    extra_span = .25,
    true_range = "none",
    bw = .85,
    size = 0.09,
    sorting = "onset"
  ) +
  scale_fill_manual(
    values = colors_legend,
    name = NULL
  ) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  theme_minimal(base_family = "Bangers") +
  theme(
    plot.background = element_rect(fill = "grey84", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(color = "grey40", size = 14),
    legend.box.margin = margin(t = 30),
    legend.background = element_rect(
      color = "grey40",
      size = .3,
      fill = "grey95"
    ),
    legend.key.height = unit(.25, "lines"),
    legend.key.width = unit(2.5, "lines"),
    plot.margin = margin(rep(20, 4))
  ) +
  labs(title = "Prior Generation Capacity (Gigawatts)") +
  theme(plot.title = element_text(hjust = 0.5))


plot1

plot2 <- ggplot(capacity, aes(year, new, fill = type)) +
  geom_stream(
    extra_span = .25,
    true_range = "none",
    bw = .85,
    size = 1.25,
    sorting = "onset"
  ) +
  geom_stream(
    geom = "contour",
    color = "white",
    extra_span = .25,
    true_range = "none",
    bw = .85,
    size = 0.02,
    sorting = "onset"
  ) +
  scale_fill_manual(
    values = colors_legend,
    name = NULL
  ) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
  theme_minimal(base_family = "Bangers") +
  theme(
    plot.background = element_rect(fill = "grey84", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(rep(20, 4))
  ) +
  labs(title = "New Generation Capacity (Gigawatts)") +
  theme(plot.title = element_text(hjust = 0.5))

plot_final <- plot1 / plot2 +
  plot_annotation(
    title = "Power Generation Capacity",
    subtitle = "The shreamcharts describe the power generation from various sources (solar, nuclear, wind, etc) along with <br>
    their capacity over the years. The graphs are separated into New and Prior Generation <br> In Gigawatts. <br> ",
    caption = "Data: Berkeley Lab | Graphic: Github.com/SidhuK ",
    theme = theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      plot.subtitle = element_markdown(size = 15, hjust = 0.5),
      plot.caption = element_text(size = 11, hjust = 0.5),
      plot.background = element_rect(fill = "grey84", color = NA)
    )
  ) &
  theme(text = element_text("Bangers"))

plot_final


ggsave(
  "plot.png",
  plot = last_plot(),
  width = 12,
  height = 14,
  units = "in",
  dpi = 450
)