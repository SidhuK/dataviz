characters <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv")
psych_stats <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv")


library(tidyverse)

lotr_psych <- psych_stats |>
    filter(uni_name == "Lord of the Rings")
# filter(question == "cool/dorky")
