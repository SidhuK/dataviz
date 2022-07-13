flights <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv")


library(tidyverse)
library(ggtext)
library(showtext)
showtext.auto(enable = TRUE)
font_add_google("Girassol", "Girassol")

countries <- flights |>
    count(STATE_NAME) |>
    arrange(desc(n))


airports <- flights |>
    group_by(STATE_NAME, APT_NAME) |>
    mutate(total = sum(FLT_DEP_1, FLT_ARR_1)) |>
    ungroup()


top10 <- airports |>
    group_by(`Pivot Label`) |>
    arrange(desc(total)) |>
    slice_head(n = 1) |>
    arrange(desc(total)) |>
    head(10) |>
    janitor::clean_names()  |> 

pal <- c("#fb8500", rep("grey70", times = 9))




top10 |>
    ggplot() +
    geom_col(aes(x = fct_reorder(apt_name, total), y = total, fill = apt_name)) +
    theme_minimal() +
    labs(
        title = "Busiest Airports in Europe",
        subtitle = "Airports with the most flights (combined arrivals + departures) since 2016.<br>
        Amsterdam - Schiphol Airport looks to have the most number of flights arriving and departing from. <br>
        The numbers after each bar represent the total number of arrivals and departures (in millions)",
        caption = "Data: Eurocontrol | Graphic: github.com/SidhuK"
    ) + geom_text(aes(
        x = apt_name, y = total,
        label =round(total/1000000, digits = 3)
    ), hjust = 0,family = "Girassol", size = 6) +
    theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Girassol", size = 14),
        plot.title = element_text(family = "Girassol", size = 30),
        plot.subtitle = element_markdown(family = "Girassol", size = 16),
        plot.caption = element_text(family = "Girassol", color = "#fb8500", size = 14),
        panel.grid = element_blank()
    
    ) +
    scale_fill_manual(values = pal, guide = "none") +
    coord_flip(        ylim  = c(0, 3000000))




ggsave("airports.png", plot = last_plot(), width = 15, height = 12, dpi = 300, bg = "#ffffff")
