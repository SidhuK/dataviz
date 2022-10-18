episodes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv")
dialogue <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")


library(tidyverse)
library(tidytext)
library(showtext)
showtext.auto(enable = TRUE)
showtext_opts(dpi = 250)
font_add_google("Girassol", "Girassol")

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

most_common <- most_common |>
    mutate(n_new = ifelse(sentiment == "positive", n, -n))

dialogue_j <- dialogue_join |> mutate(sent = ifelse(sentiment == "positive", 1, -1))

# dialogue_j |>
#     ggplot(aes(x = line, y = sent), color = sent) +
#     geom_line() +
#     facet_grid(
#         rows = vars(season),
#         cols = vars(episode), scales = "free"
#     )


most_common |> ggplot(aes(x = fct_reorder(word, -n), y = n_new, fill = sentiment)) +
    geom_col() +
    geom_text(aes(label = word, color = sentiment, hjust = ifelse(n_new < 0, 1, 0)),
        vjust = 0.5, , size = 6,
        family = "Girassol", angle = 90
    ) +
    ggtext::geom_richtext(
        data = data.frame(
            x = 40, y = 800,
            label = "Stranger Things"
        ), mapping = aes(
            x = x,
            y = y, label = label
        ), size = 14, family = "Girassol",
        inherit.aes = FALSE,
        color = "#f03444",
        fill = NA,
        label.size = 0
    ) +
    ggtext::geom_richtext(
        data = data.frame(
            x = 40, y = 690,
            label = "Sentiment Analysis of Stranger Things Dialogue<br>
            using TidyText. Analyzed the 75 most common words occurring in <br> the first
            4 Seasons. "
        ), mapping = aes(
            x = x,
            y = y, label = label
        ), size = 7, family = "Girassol",
        inherit.aes = FALSE,
        color = "#1d3557",
        fill = NA,
        label.size = 0
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("#f03444", "#1d3557")) +
    scale_color_manual(values = c("#f03444", "#1d3557")) +
    theme(
        plot.background = element_rect(fill = "#fdf0d5"),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(family = "Girassol")
    )

setwd("/Users/karatatiwantsinghsidhu/Documents/Code/TidyTuesday/2022/30_October_18")

ggsave(plot = last_plot(), filename = "stranger_things.png", width = 13, height = 10, dpi = 250)
