# Reading the data

frog <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frog.csv")


# Loading Libraries
library(tidyverse)
library(waffle)
library(hrbrthemes)

frog |>
    ggplot() +
    geom_point(aes(x = Ordinal, y = Water, shape = Type, color = Structure)) +
    facet_wrap(vars(HabType)) +
    theme_minimal()



parts <- c(`Un-breached\nUS Population` = (318 - 11 - 79), `Premera` = 11, `Anthem` = 79)
waffle(
    parts,
    rows = 8, size = 1,
    colors = c("#969696", "#1879bf", "#009bda"), legend_pos = "bottom"
)



shallow_water <- frog |>
    filter(Water == "Shallow water") |>
    count(Type)

shallow_water

waffle(shallow_water,
    rows = 5, size = 1,
    colors = c("#969696", "#1879bf", "#009bda"), legend_pos = "bottom"
)

deep_water <- frog |>
    filter(Water == "Deep water") |>
    count(Type)

deep_water
waffle(deep_water,
    rows = 5, size = 1,
    colors = c("#969696", "#1879bf", "#009bda"), legend_pos = "bottom"
)
