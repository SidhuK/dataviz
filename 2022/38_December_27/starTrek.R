tlBooks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv")


library(tidyverse)
library(showtext)
library(ggtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 250)
font_add_google("Girassol", "Girassol")
library(waffle)




# label_position <- top4 |>
#     group_by(series) |>
#     summarise(lab_position = mean(year) + 5) |>
#     mutate(
#         series_full =
#             case_when(
#                 series == "VOY" ~ "Voyager",
#                 series == "TOS" ~ "The Original Series",
#                 series == "TNG" ~ "The Next Generation",
#                 series == "ENT" ~ "Enterprise"
#             )
#     )


# top4 |> ggplot(aes(x = year, y = series)) +
#     geom_text(
#         aes(y = series, x = lab_position, label = series_full),
#         data = label_position,
#         family = "Girassol",
#         color = "#918741",
#         vjust = 0.05, hjust = 0.01,
#         size = 10
#     ) +
#     ggdist::stat_dots(binwidth = 0.5) +
#     ggdist::stat_pointinterval(position = "dodge") +
#     theme_minimal() +
#     theme(
#         axis.text.y = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank()
#     )



number_books <- tlBooks |>
    group_by(series, format) |>
    summarise(total_books = n())



number_books |>
    ggplot(aes(fill = fct_reorder(format, total_books), values = total_books)) +
    geom_waffle(
        n_rows = 10,
        size = 0.33,
        colour = "white",
        flip = TRUE
    ) +
    facet_wrap(~series, nrow = 2, strip.position = "bottom") +
    theme_minimal()
