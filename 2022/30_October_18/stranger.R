episodes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv")
dialogue <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")


library(tidyverse)
library(tidytext)

# dialogue_most <- dialogue |>
#     na.omit() |>
#     group_by(stage_direction) |>
#     count() |>
#     arrange(desc(n)) |>
#     head(25)

# dialogue |>
#     na.omit() |>
#     mutate(duration = end_time - start_time) |>
#     group_by(stage_direction, duration) |>
#     summarise(total_time = n(stage_direction)) |>
#     arrange(desc(total_time))

dialogue_clean <- dialogue |>
    select(dialogue, season, episode, line) |>
    unnest_tokens(word, dialogue) |>
    anti_join(get_stopwords(), by = "word") |>
    na.omit()


bing_df <- get_sentiments("bing")

dialogue_join <- dialogue_clean |>
    inner_join(bing_df, by = "word")


most_common <- dialogue_join |>
    group_by(word, sentiment) |>
    tally() |>
    arrange(desc(n)) |>
    head(75)

most_common |>
    group_by(sentiment) |>
    summarise(n())

dialogue_j <- dialogue_join |> mutate(sent = ifelse(sentiment == "positive", 1, -1))

# dialogue_j |>
#     ggplot(aes(x = line, y = sent), color = sent) +
#     geom_line() +
#     facet_grid(
#         rows = vars(season),
#         cols = vars(episode), scales = "free"
#     )


most_common |> ggplot(aes(y = fct_reorder(word, n), x = n, fill = sentiment)) +
    geom_col() +
    theme_minimal() +
    scale_fill_manual(values = c("red", "green"))
