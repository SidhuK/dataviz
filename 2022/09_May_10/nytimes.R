

library(tidyverse)
library(showtext)
showtext_opts(dpi = 450)
showtext_auto(enable = TRUE)
library(ggtext)
library(ggrepel)
font_add_google(family = "Roboto", name = "Roboto")
font_add_google(family = "Outfit", name = "Outfit")



nyt_titles <-
    readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv")
nyt_full <-
    readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv")




colors_legend <- c(
    "#FFB327",
    "#08748f",
    "#4F607C",
    "#2d6554",
    "#8E038E",
    "#5A6D87",
    "#000000",
    "#725050",
    "#542ea5",
    "#304d30",
    "#8E038E"
)


nytitiles_weeks <- nyt_titles %>%
    mutate(
        decade = (year %/% 10) * 10
    ) %>%
    group_by(decade) %>%
    slice(which.max(total_weeks)) %>%
    mutate(title_new = paste(title, "(", total_weeks, "Weeks )"))




nytitiles_weeks






nyt_titles %>%
    mutate(
        decade = (year %/% 10) * 10
    ) %>%
    ggplot(aes(
        x = as.factor(reorder(decade, -decade)),
        y = total_weeks,
        group = decade,
        color = as.factor(decade)
    )) +
    geom_jitter(width = 0.25, alpha = 0.2, size = 0.85) +
    geom_text_repel(aes(
        label = title_new,
        family = "Outfit"
    ),
    size = 4.5,
    data = nytitiles_weeks, alpha = 0.9,
    box.padding = 0.35
    ) +
    geom_point(
        data = nytitiles_weeks,
        aes(
            x = as.factor(decade),
            y = total_weeks
        ),
        size = 2
    ) +
    theme_minimal() +
    labs(
        title = "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/The_New_York_Times_logo.png/800px-The_New_York_Times_logo.png' width = '300'> ",
        subtitle = "**Bestsellers; by Decade.** <br>Looking at the number of weeks each book stayed on the NYTimes bestsellers<br> list every decade. Most popular bestselling book with the most weeks on the <br> list is labelled for each decade.",
        caption = "Data: Post45 | Graphic: github.com/SidhuK"
    ) +
    ylab("Weeks") +
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_markdown(family = "Cookie", size = 10),
        plot.subtitle = element_markdown(family = "Outfit", size = 18),
        plot.caption = element_markdown(family = "Outfit", size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text = element_text(size = 14),
    ) +
    scale_color_manual(
        values = colors_legend,
        name = NULL
    ) +
    coord_flip()



ggsave("nytimes.png", plot = last_plot(), width = 12, height = 16, dpi = 450, bg = "#fffcf6")