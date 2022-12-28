tlBooks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv")

# star trek timeline distribution
# tng, tos, voy, DS9


library(tidyverse)
library(showtext)
library(ggtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 250)
font_add_google("Girassol", "Girassol")


tlBooks |> View()

tlBooks |>
    group_by(series) |>
    summarise(n()) |>
    view()


top4 <- tlBooks |>
    filter(series == "TNG" | series == "TOS" | series == "DS9" | series == "VOY") |>
    view()


label_position <- top4 |>
    group_by(series) |>
    summarise(lab_position = 2400) |>
    mutate(
        series_full =
            case_when(
                series == "VOY" ~ "Voyager",
                series == "TOS" ~ "The Original Series",
                series == "TNG" ~ "The Next Generation",
                series == "DS9" ~ "Deep Space Nine"
            )
    )


top4 |> ggplot(aes(x = year, y = series)) +
    geom_text(
        aes(y = series, x = lab_position, label = series_full),
        data = label_position,
        family = "Girassol",
        color = "#918741",
        vjust = 0.05, hjust = 0.01,
        size = 10
    ) +
    ggdist::stat_dots(binwidth = 0.45, dotsize = 3) +
    ggdist::stat_pointinterval(position = "dodge") +
    scale_x_continuous(limits = c(1990, 2700)) +
    theme_minimal() +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    )
