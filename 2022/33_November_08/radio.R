state_stations <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv")

library(tidyverse)

state_stations |>
    count(format) |>
    arrange(desc(n)) |>
    head(20)
