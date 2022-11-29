wcmatches <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")
worldcups <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")


library(tidyverse)


wcmatches <- wcmatches |> mutate(total_goals = home_score + away_score)


high_scores <- wcmatches |>
    arrange(desc(total_goals)) |>
    head(19)


high_scores <- high_scores |>
    select(-c(dayofweek, month, date, losing_team, winning_team, outcome))

high_scores |>
    ggplot()
