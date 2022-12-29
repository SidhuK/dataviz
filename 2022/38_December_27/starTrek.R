tlBooks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv")

setwd("/Users/karatatiwantsinghsidhu/Documents/Code/TidyTuesday/2022/38_December_27/")


library(tidyverse)
library(showtext)
library(ggtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 250)
font_add_google("Bebas Neue", "Bebas Neue")
library(waffle)



number_books <- tlBooks |>
    group_by(series, format) |>
    summarise(total_books = n()) |>
    ungroup() |>
    filter(series != "NA") |>
    na.omit()


ggannotate::ggannotate()


number_books |>
    ggplot(aes(fill = fct_rev(format), values = total_books)) +
    geom_waffle(
        n_rows = 10,
        size = 0.5,
        colour = "#000000",
        flip = TRUE,
    ) +
    facet_grid(~ factor(series,
        levels = c("TNG", "TOS", "VOY", "DS9", "ENT", "ST", "SCE", "NF", "SE", "TLE", "TAS", "TNN", "SGZ", "CHA")
    )) +
    theme_void() +
    scale_fill_manual(
        values = c("#a71313", "#2b53a7", "#d6a444")
    ) +
    labs(
        title = "The Star Trek universe",
        subtitle = "The data this week comes from the {rtrek} package.
        The Start Trek Universe is further classified into different series.
        For each of these series, different media formats are shown in the chart.<br> This chart shows the number of <span style = 'color:#d6a444;'>Books</span>,
        <span style = 'color:#2b53a7;'>Episodes</span> and
        <span style = 'color:#a71313;'>Stories</span> per series. Each square represents a single instance of the corresponding media. <br>",
        caption = "Data: {rtrek}| Graphic: Github.com/SidhuK"
    ) +
    theme(
        plot.background = element_rect(fill = "#100326"),
        axis.title.x = element_text(color = "#FFFFFF"),
        strip.text = element_text(
            color = "#c1c730",
            family = "Bebas Neue", size = 15
        ),
        plot.subtitle = element_markdown(
            family = "Bebas Neue",
            color = "#c1c730",
            size = 12
        ),
        plot.title = element_markdown(
            family = "Bebas Neue",
            color = "#c1c730",
            size = 40
        ),
        plot.caption = element_markdown(
            family = "Bebas Neue",
            color = "#c1c730",
            size = 8
        ),
        legend.position = "none"
    )



ggsave("starTrek.png",
    plot = last_plot(), width = 14, height = 7,
    dpi = 250, units = "in"
)
