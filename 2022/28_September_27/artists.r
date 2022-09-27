artists <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv")


# variable	class	description
# state	character	state/territory
# race	character	race
# type	character	type of artists
# all_workers_n	double	all worker count
# artists_n	double	artist count
# artists_share	double	artist share
# location_quotient	double	Location quotients (LQ) measure
# an artist occupation's concentration in the labor force,
# relative to the U.S. labor force share. For example,
# an LQ of 1.2 indicates that the state's labor force in an occupation is 20 percent
# greater than the occupation's national labor force share. An LQ of 0.8 indicates that the state's
# labor force in an occupation is 20 percent below the occupation's national labor force share.

library(tidyverse)
library(ggdist)
library(showtext)
library(ggtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 250)
font_add_google("Girassol", "Girassol")


artists |> view()


max_art <- artists |>
    group_by(race, type) |>
    arrange(desc(location_quotient)) |>
    slice(1) |>
    na.omit() |>
    ungroup()


artists |>
    group_by(type, race) |>
    arrange(desc(location_quotient)) |>
    na.omit() |>
    ungroup() |>
    ggplot(aes(x = location_quotient, y = 0)) +
    ggrepel::geom_text_repel(
        data = max_art,
        aes(location_quotient, y = 0, label = state),
        nudge_x = .15,
        box.padding = 0.5,
        nudge_y = 2.5,
        segment.curvature = -0.2,
        segment.ncp = 3,
        segment.angle = 45,
        color = "#ea6303",
        size = 3,
        family = "Girassol"
    ) +
    stat_dots(
        color = "#3f3f3f",
        fill = "#3f3f3f",
        side = "right", dotsize = 0.7
    ) +
    facet_grid(
        rows = vars(str_wrap(type, 25)),
        cols = vars(race), scales = "free_x"
    ) +
    coord_cartesian() +
    theme_minimal() +
    labs(
        title = "Artists in the United States of America",
        subtitle = "States with the highest location quotient in the United States of America for various artists and
        artistic professionals.<br> Location quotients measures an artist occupation's concentration in the labor force share
        relative to the U.S. labor force share.<br>Higher LQ value means more share of artists in that state",
        x = "Location Quotient",
        caption = "Data: Art.GOV, Graphic: github.com/SidhuK"
    ) +
    theme(
        strip.text = element_text(
            family = "Girassol",
            color = "#c10303"
        ),
        strip.text.y.right = element_text(angle = 0),
        plot.title = element_markdown(
            face = "bold",
            family = "Girassol", color = "#c10303", size = 35
        ),
        plot.subtitle = element_markdown(
            face = "bold", color = "#ea6303", family = "Girassol",
            size = 15
        ),
        plot.caption = element_markdown(
            face = "bold",
            family = "Girassol", color = "#3f3f3f", size = 10, hjust = 0
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(family = "Girassol", size = 15, color = "#3f3f3f"),
        axis.text.x = element_text(family = "Girassol", color = "#3f3f3f")
    )


setwd("/Users/karatatiwantsinghsidhu/Documents/Code/TidyTuesday/2022/28_September_27")

ggsave("artists.png", plot = last_plot(), width = 16, height = 12, dpi = 250, bg = "#ffffff")
