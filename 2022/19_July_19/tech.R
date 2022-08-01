technology <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv")


library(tidyverse)



financial <- technology |>
    filter(category == "Financial")



cards <- financial |>
    filter(label == "Number of credit or debit cards in circulation")
