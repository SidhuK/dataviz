cities <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv")
outlook_meanings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv")



library(tidyverse)


cities |>
    gt::gt() |>
    gtExtras::gt_theme_538()


map(cities, class)


cities |>
    group_by(koppen) |>
    summarise(n()) |>
    ungroup()

cities |> view()
