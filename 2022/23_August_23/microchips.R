chips <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv")
tuesdata <- tidytuesdayR::tt_load(2022, week = 34)


library(tidyverse)


chips <- chips |> na.omit()


chips |>
    ggplot() +
    geom_line(aes(x = year, y = transistors_million)) +
    geom_line(aes(x = year, y = tdp_w))
