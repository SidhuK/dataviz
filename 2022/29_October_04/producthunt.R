product_hunt <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv")

library(tidyverse)


product_hunt |> filter(str_detect(category_tags, "GITHUB"))
