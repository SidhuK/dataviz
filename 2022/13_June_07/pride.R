# Loading the packages
library(tidyverse)
library(ggsvg)
library(showtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 450)
library(grid)
library(ggtext)

font_add_google("Atomic Age", "Atomic Age")
font_add_google("Federant", "Federant")



# Loading the data
pride_aggregates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv")
fortune_aggregates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv")
static_list <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv")
pride_sponsors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv")
corp_by_politicians <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politician.csv")
donors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/donors.csv")



pride_aggregates <- pride_aggregates |>
    janitor::clean_names()


# ----Creating the plot-----


# Using top 30 donors
pride_aggregates <- pride_aggregates |>
    arrange(desc(total_contributed)) |>
    filter(company != "Grand Total") |>
    head(30)


# new column for labels
pride_aggregates <- pride_aggregates |>
    mutate(company_total = paste0(company, " ($", floor(total_contributed), ")"))


# New DF for points
pride_agg_clean <- pride_aggregates |>
    rowwise() |>
    mutate(
        y = list(1:(number_of_politicians_contributed_to))
    ) |>
    ungroup() |>
    unnest(y)

# SVG for politician
svg_building <- "https://www.svgrepo.com/download/113766/politician.svg"
build_svg <- paste(readLines(svg_building), collapse = "\n")

# Map for states
politicians <- corp_by_politicians |>
    filter(Politician != "Grand Total") |>
    count(State, sort = TRUE)

politicians <- politicians |>
    mutate(
        state = State,
        values = n
    )

map_plot <- usmap::plot_usmap(
    data = politicians, labels = TRUE, values = "n"
) +
    scale_fill_continuous(name = "Number of Politicians", low = "#ffeeae", high = "#ff9f10") +
    theme(legend.position = "none")

map_plot


# Palette for bars
pal <- c(
    "#ff9c08",
    rep("#4c4c4c", length(pride_aggregates$company) - 1)
)


# Actual plot
ggplot(pride_agg_clean) +
    geom_point_svg(aes(x = fct_reorder(company, total_contributed), y = -y),
        svg = build_svg,
        size = 6
    ) +
    geom_segment(aes(
        x = company, y = 0, xend = company,
        yend = total_contributed / 10000,
        color = fct_reorder(company, -total_contributed)
    ), data = pride_aggregates, size = 6, lineend = "round") +
    geom_text(aes(
        x = company, y = (total_contributed / 10000) + 1,
        label = company_total
    ),
    data = pride_aggregates, hjust = 0, vjust = 0.5, size = 8,
    family = "Federant"
    ) +
    scale_color_manual(values = pal, guide = "none") +
    annotation_custom(ggplotGrob(map_plot),
        xmin = 0, xmax = 10,
        ymin = 40, ymax = 80
    ) +
    coord_flip(
        ylim = c(-70, 85),
        xlim = c(0, 32)
    ) +
    theme_void() +
    labs(
        title = "Companies funding Anti-LGBTQ+ Campaigns",
        caption = "**Data** : Data For Progress | **Graphic** : github.com/SidhuK",
        subtitle = "Data for Progress finds that in between their yearly parade appearances while misrepresenting themselves as LGBTQ+ allies, <br>
         dozens of fortune500 corporations (and others) are contributing to state politicians behind some of the most bigoted and harmful policies in over a decade." # nolint # nolint
    ) +
    geom_text(
        data = data.frame(x = 9, y = 60, label = "Politicians per state"),
        mapping = aes(x = x, y = y, label = label),
        family = "Federant", fontface = 2, inherit.aes = FALSE
    ) +
    geom_text(
        data = data.frame(x = 31, y = 40, label = "Amount Donated"),
        mapping = aes(x = x, y = y, label = label),
        size = 7, family = "Federant", fontface = 2, inherit.aes = FALSE
    ) +
    geom_text(
        data = data.frame(x = 31, y = -35, label = "Number of Politicians"),
        mapping = aes(x = x, y = y, label = label),
        size = 7, family = "Federant", fontface = 2, inherit.aes = FALSE
    ) +
    theme(
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_markdown(
            family = "Federant", face = "bold", size = 42,
            hjust = 0.5
        ),
        plot.subtitle = element_markdown(
            family = "Federant", face = "bold", size = 17,
            hjust = 0.5
        ),
        plot.caption = element_markdown(
            family = "Federant", face = 2, size = 12,
            colour = "grey40",
            hjust = 0.5
        ),
    )


# Saving the plot
ggsave("pride.png",
    plot = last_plot(),
    width = 24, height = 18, units = "in", dpi = 400,
    bg = "#ffffff"
)
