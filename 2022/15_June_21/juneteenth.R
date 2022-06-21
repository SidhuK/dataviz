library(tidyverse)
library(showtext)
font_add_google("Fira Mono", "Fira Mono")
showtext_auto(enable = TRUE)
font_add_google("Bebas Neue", "Bebas Neue")
library(ggtext)

# DuBois Styling guide
# https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.md


# loading the data
blackpast <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv")


# color palette
pal <- c(rep("#dc143c", 50))


# Looking at just the US.
us_blackpast <- blackpast |>
    filter(country == "United States")


# n >5 for timeline, to avoid a few points
us_black_movement <- us_blackpast |>
    count(subject, sort = TRUE) |>
    filter(n > 5)



# dataframe for the timeline
us_civil_rights <- us_blackpast |>
    filter(subject %in% us_black_movement$subject) |>
    na.omit()


# Selected a few labels to put on the timeline
start_subject <- us_civil_rights |>
    mutate(year = as.integer(year)) |>
    group_by(subject) |>
    arrange(year) |>
    slice_head(n = 1) |>
    mutate(events_new = gsub("([a-z0-9]* [a-z0-9]* [a-z0-9]* [a-z0-9]*) ", "\\1\n ", events)) |> # making a new line after a few words to make the labels more readable.
    filter(subject %in% c(
        "Black Education",
        "Anti-Slavery Campaign",
        "Emancipation",
        "Black Business",
        "Resistance to Enslavement",
        "Civil Rights Legislation",
        "Civil Rights",
        "The Civil Rights Movement",
        "Slave Laws",
        "Jim Crow Legislation",
        "Antebellum Slavery",
        "Racial Violence",
        "Crime and Punishment",
        "Major Juridicial Decisions", "African Americans and the Media"
    )) # selected a few labels


# plot the graph

us_civil_rights |>
    group_by(subject) |>
    summarize(
        year_start = as.integer(min(year)),
        year_end = as.integer(max(year))
    ) |>
    na.omit() |>
    ungroup() |>
    ggplot() +
    geom_segment(aes(x = year_start, y = fct_reorder(subject, -year_start), xend = year_end, yend = subject),
        size = 4.8, lineend = "butt", color = "grey20"
    ) +
    geom_segment(aes(x = year_start, y = fct_reorder(subject, -year_start), xend = year_end, yend = subject, color = subject),
        size = 4, lineend = "butt"
    ) +
    geom_text(aes(x = year_end + 6, y = subject, label = subject),
        hjust = 0, vjust = -0.2, family = "Bebas Neue", size = 6, color = "grey55"
    ) +
    geom_text(aes(
        x = year_end + 6, y = subject,
        label = paste0("(", year_start, "-", year_end, ")")
    ),
    hjust = 0, vjust = 1.1,
    family = "Bebas Neue",
    size = 4.5,
    color = "grey70"
    ) +
    ggrepel::geom_text_repel(
        data = start_subject,
        aes(x = year, y = subject, label = events_new),
        color = "grey10",
        size = 2.7,
        nudge_x = -90,
        hjust = 0.5,
        segment.size = 0.7,
        segment.curvature = -0.7,
        segment.angle = 65,
        segment.square = TRUE,
        segment.inflect = TRUE,
        family = "Fira Mono",
        min.segment.length = 0.1,
        box.padding = unit(0.45, "lines"),
    ) +
    geom_point(aes(x = year_start, y = subject, fill = subject),
        size = 6, shape = 22, color = "grey30"
    ) +
    scale_x_continuous(
        breaks = seq(from = 1600, to = 2010, by = 50),
        position = "top"
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(
            family = "Bebas Neue",
            size = 17
        ),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    expand_limits(x = c(1500, 2100)) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    geom_richtext(
        data = data.frame(
            x = 1640, y = 6,
            label = "<br>A Timeline of some of the most noteworthy moments, groups, laws and movements <br>
               that influnced the History of the African-American <br>& Black Community in
        the United States of America."
        ), mapping = aes(
            x = x,
            y = y, label = label
        ), size = 8, family = "Bebas Neue",
        inherit.aes = FALSE,
        color = "grey50",
        fill = "#ffffff",
        label.size = 0
    ) +
    geom_text(
        data = data.frame(
            x = 1635, y = 7.55,
            label = "Black History in the U.S."
        ), mapping = aes(
            x = x,
            y = y, label = label
        ), size = 25, family = "Bebas Neue",
        inherit.aes = FALSE
    ) +
    geom_text(
        data = data.frame(
            x = 1635, y = 4,
            label = "Data: blackpast.org | Graphic: Github.com/SidhuK"
        ), mapping = aes(
            x = x,
            y = y, label = label
        ), size = 6, family = "Bebas Neue",
        inherit.aes = FALSE, color = "grey60"
    )


# save the plot
ggsave("juneteenth.png", plot = last_plot(), width = 18, height = 19, dpi = 300, bg = "#ffffff")
