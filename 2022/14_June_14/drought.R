drought <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv")



library(tidyverse)
library(geofacet)
library(sysfonts)
library(ggtext)
library(patchwork)

states <- drought |>
    count(state) |>
    mutate(state = str_to_title(state)) |>
    mutate(state = str_replace(state, "\\-", " ")) |>
    mutate(state_tx = state.abb[match(state, state.name)])


drought_2 <- drought |>
    mutate(date2 = as.integer(str_extract(DATE, "\\d\\d\\d\\d\\d\\d\\d\\d"))) |>
    filter(date2 > 20000101) |>
    mutate(year = as.integer(str_extract(date2, "\\d\\d\\d\\d"))) |>
    mutate(state = str_to_title(state)) |>
    mutate(state = str_replace(state, "\\-", " ")) |>
    mutate(state_abb = state.abb[match(state, state.name)])


# big letters for the states, use geom_text()


drought_cleaned <- drought_2 |>
    group_by(year, state, state_abb) |>
    summarise(across(D0:W4, ~ mean(.x)))



mainland_us <- us_state_grid2 %>% filter(!code %in% c("HI", "AK"))


dry <- ggplot(data = drought_cleaned) +
    geom_text(aes(x = 2015, y = 70, label = state_abb),
        size = 25,
        color = "grey90",
        family = "Bebas Neue"
    ) +
    scale_x_continuous(
        limits = c(2000, 2022),
        breaks = c(2000, 2022)
    ) +
    geom_area(aes(x = year, y = D0), fill = "#d3c453") +
    geom_area(aes(x = year, y = D1), fill = "#e5995e") +
    geom_area(aes(x = year, y = D2), fill = "#dd7137") +
    geom_area(aes(x = year, y = D3), fill = "#c64430") +
    geom_area(aes(x = year, y = D4), fill = "#96190b") +
    labs(
        subtitle = "<span style = 'color:#d3c453;'>Abnormally dry |</span>
        <span style = 'color:#e5995e;'>Moderate drought |</span>
        <span style = 'color:#dd7137;'>Severe drought |</span>
        <span style = 'color:#c64430;'>Extreme drought |</span>
        <span style = 'color:#96190b;'>Exceptional drought</span>"
    ) +
    geofacet::facet_geo(~state_abb, grid = mainland_us) +
    theme_minimal() +
    theme(
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 8, family = "Bebas Neue"),
        panel.spacing = unit(1.5, "lines"),
        plot.subtitle = element_markdown(hjust = 0.5, vjust = 0.5, size = 12, family = "Bebas Neue")
    )

dry

wet <- ggplot(data = drought_cleaned) +
    geom_text(aes(x = 2015, y = 70, label = state_abb),
        size = 25,
        color = "grey90",
        family = "Bebas Neue"
    ) +
    scale_x_continuous(
        limits = c(2000, 2022),
        breaks = c(2000, 2022)
    ) +
    geom_area(aes(x = year, y = W0), fill = "#BFEFFF") +
    geom_area(aes(x = year, y = W1), fill = "#00BFFF") +
    geom_area(aes(x = year, y = W2), fill = "#0099CC") +
    geom_area(aes(x = year, y = W3), fill = "#0179a1") +
    geom_area(aes(x = year, y = W4), fill = "#325C74") +
    labs(
        subtitle = "<br><br><br><span style = 'color:#BFEFFF;'>Abnormally wet |</span>
        <span style = 'color:#00BFFF;'>Moderate wet |</span>
        <span style = 'color:#0099CC;'>Severe wet |</span>
        <span style = 'color:#0179a1;'>Extreme wet |</span>
        <span style = 'color:#325C74;'>Exceptional wet</span>"
    ) +
    geofacet::facet_geo(~state_abb, grid = mainland_us) +
    theme_minimal() +
    theme(
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 8, family = "Bebas Neue"),
        panel.spacing = unit(1.5, "lines"),
        plot.subtitle = element_markdown(hjust = 0.5, vjust = 0.5, size = 12, family = "Bebas Neue")
    )

wet

drought_plot <- dry / wet

drought_plot + plot_annotation(
    title = "Drought Conditions in USA",
    subtitle = "For the 'Lower 48'/Mainland US states only, Graphic is represented by state,
    and the color represents the drought condition.. <br> Top graph shows the dry conditions, and
    the bottom graph shows the wet conditions.
    <br> Data from 2000 to 2022 <br><br>",
    caption = "Data: Drought.Gov | Graphic: github.com/SidhuK", ,
    theme = theme(
        plot.title = element_text(size = 65, hjust = 0.5),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5)
    )
) &
    theme(text = element_text("Bebas Neue"))

ggsave("drought.png", plot = last_plot(), width = 20, height = 30, dpi = 250, bg = "#ffffff")
