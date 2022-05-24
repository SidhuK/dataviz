# Loading Packages

library(tidyverse)
library(ggsvg)
library(ggtext)
library(showtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 450)

font_add_google("Alegreya Sans SC", "Alegreya Sans SC")

font_add_google("Overpass Mono", "Overpass Mono")

# Reading the data
sevens <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv"
  )
fifteens <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv"
  )


# # Fifteens (Sevens look more interesting)
# fifteens |>
#   count(winner, sort = TRUE)
# 
# 
# fifteens_clean <- fifteens |>
#   mutate(year = lubridate::year(date))
# 
# 
# fifteens_wins <- fifteens_clean |>
#   group_by(winner, year) |>
#   summarise(wins = n()) |>
#   arrange(desc(wins))
# 
# fifteens_new <- fifteens_wins |>
#   group_by(winner) |>
#   summarise(wins_total = sum(wins)) |>
#   mutate(winner_wins = paste0(winner, "(", wins_total, ")")) |>
#   arrange(desc(wins_total)) |>
#   head(25)
# 
# 
# 
# joined_fifteens <- full_join(fifteens_wins, fifteens_new) |>
#   na.omit() |>
#   filter(winner != "Draw")
# 
# rugby_url <- "https://www.svgrepo.com/show/118062/rugby.svg"
# 
# rugby_url <-
#   "https://www.svgrepo.com/show/68545/regulation-rugby-ball.svg"
# 
# rugby_svg <- paste(readLines(rugby_url), collapse = "\n")
# 
# 
# 
# 
# joined_fifteens |>
#   ggplot(aes(year,
#              reorder(winner_wins, wins_total),
#              size = wins,)) +
#   geom_point(color = "black") +
#   geom_point(shape = 21,
#              fill = NA,
#              stroke = .2) +
#   coord_cartesian(clip = "off") +
#   scale_x_continuous(expand = c(0, 0), breaks = 1982:2022) +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_color_manual(values = c("#cd8d2a", "grey75", "#a45128")) +
#   scale_alpha_manual(values = c(1, .67, .33)) +
#   scale_size_continuous(breaks = c(3, 6, 9, 12, 15), limit = c(0, 15)) +
#   hrbrthemes::theme_ipsum() +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.x = element_text(
#       angle = 90,
#       vjust = 0.5,
#       hjust = 1,
#       margin = margin(
#         t = 7,
#         r = 0,
#         b = 0,
#         l = 0
#       )
#     ),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_line(color = "grey87", size = .3),
#     panel.grid.major.y = element_line(color = "#b0c6ce", size = .7),
#   )
# 
# 
# 
# joined_fifteens |>
#   ggplot(aes(year,
#              reorder(winner_wins, wins_total))) +
#   geom_point_svg(svg = rugby_svg,
#                  aes(size = wins),
#                  fill = "black") +
#   scale_svg_default() +
#   coord_cartesian(clip = "off") +
#   scale_x_continuous(expand = c(0, 0), breaks = 1982:2022) +
#   scale_size_continuous(breaks = c(3, 6, 9, 12, 15), limit = c(0, 15)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_alpha_manual(values = c(1, .67, .33)) +
#   hrbrthemes::theme_ipsum() +
#   theme(
#     axis.text.x = element_text(
#       angle = 90,
#       vjust = 0.5,
#       hjust = 1,
#       margin = margin(
#         t = 10,
#         r = 0,
#         b = 0,
#         l = 0
#       )
#     ),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_line(color = "grey87", size = .3),
#     panel.grid.major.y = element_line(color = "#b0c6ce", size = .7),
#   )
# 



# Sevens


# Get the year from the date column
sevens_clean <- sevens |>
  mutate(year = lubridate::year(date))

# countries with wins by year

sevens_win <- sevens_clean |>
  group_by(winner, year) |>
  summarise(wins = n()) |>
  arrange(desc(wins))


# total wins per country, new column added to use as y-axis.
sevens_new <- sevens_win |>
  group_by(winner) |>
  summarise(wins_total = sum(wins)) |>
  mutate(winner_wins = paste0(winner, "(", wins_total, ")")) |>
  arrange(desc(wins_total)) |>
  head(25)



# join two dataframes, remove draws and na values to keep just the top 25 countries
joined_sevens <- full_join(sevens_win, sevens_new) |>
  na.omit() |>
  filter(winner != "Draw")


# ggsvg url
rugby_url <- "https://www.svgrepo.com/show/118062/rugby.svg"

rugby_url <-
  "https://www.svgrepo.com/show/68545/regulation-rugby-ball.svg"

rugby_svg <- paste(readLines(rugby_url), collapse = "\n")





# ggpoints plot. svg takes a while to render
joined_sevens |>
  ggplot(aes(year,
             reorder(winner_wins, wins_total),
             size = wins,)) +
  geom_point(color = "black") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), breaks = 1997:2022) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title = "<img src = 'https://upload.wikimedia.org/wikipedia/en/f/f3/World_Rugby_Sevens_Series_logo.png' width = 35>
    Womens Rugby Sevens  Wins (1997-2022)",
    subtitle = "Year-by-Year Timeline of wins for the Top 25 Countries (by number of total wins) in Women's Sevens Rugby. <br>
    Number of wins for each country per year represented by the size of the rugby ball.<br>",
    caption = "Data: @ScrumQueens | Graphic: Github.com/SidhuK"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_markdown(
      family = "Alegreya Sans SC",
      size = 20,
      hjust = 0.5,
      margin = margin(
        t = 7,
        r = 0,
        b = 10,
        l = 0
      ),
      color = "#404040"
    ),
    plot.title = element_markdown(
      family = "Alegreya Sans SC",
      size = 50,
      hjust = 0.5,
      face = "bold",
      color = "#990000"
    ),
    plot.caption = element_markdown(
      family = "Overpass Mono",
      size = 15,
      color = "grey75",
      hjust = 0.5
    ),
    axis.text = element_text(
      family = "Overpass Mono",
      size = 15,
      color = "#00001a"
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      margin = margin(
        t = 7,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey87", size = .3),
    panel.grid.major.y = element_line(color = "#e6e6ff", size = .7),
    legend.position = "bottom",
    legend.text = element_text(color = "grey40", size = 14),
    legend.box.margin = margin(t = 30),
    legend.background = element_rect(color = "#fafaf5",
                                     size = 0.1,
                                     fill = "#fafaf5"),
    legend.key.height = unit(1.25, "lines"),
    legend.key.width = unit(3.5, "lines"),
    plot.margin = margin(rep(20, 4))
  )



# ggsvg final plot


joined_sevens |>
  ggplot(aes(year,
             reorder(winner_wins, wins_total))) +
  geom_point_svg(svg = rugby_svg,
                 aes(size = wins),
                 fill = "black") +
  scale_svg_default() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), breaks = 1997:2022) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title = "<img src = 'https://upload.wikimedia.org/wikipedia/en/f/f3/World_Rugby_Sevens_Series_logo.png' width = 35>
    Womens Rugby Sevens  Wins (1997-2022)",
    subtitle = "Year-by-Year Timeline of wins for the Top 25 Countries (by number of total wins) in Women's Sevens Rugby. <br>
    Number of wins for each country per year represented by the size of the rugby ball.<br>",
    caption = "Data: @ScrumQueens | Graphic: Github.com/SidhuK"
  ) +
  scale_size_continuous(name = "Wins") +
  theme_minimal() +
  theme(
    plot.subtitle = element_markdown(
      family = "Alegreya Sans SC",
      size = 20,
      hjust = 0.5,
      margin = margin(
        t = 7,
        r = 0,
        b = 10,
        l = 0
      ),
      color = "#404040"
    ),
    plot.title = element_markdown(
      family = "Alegreya Sans SC",
      size = 55,
      hjust = 0.5,
      face = "bold",
      color = "#990000"
    ),
    plot.caption = element_markdown(
      family = "Overpass Mono",
      size = 15,
      color = "grey75",
      hjust = 0.5
    ),
    axis.text = element_text(
      family = "Overpass Mono",
      size = 15,
      color = "#00001a"
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      margin = margin(
        t = 7,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey87", size = .3),
    panel.grid.major.y = element_line(color = "#e6e6ff", size = .7),
    legend.position = "bottom",
    legend.title = element_text(size = 14, family = "Overpass Mono"),
    legend.text = element_text(color = "grey40", size = 14, family = "Overpass Mono"),
    legend.box.margin = margin(t = 30),
    legend.background = element_rect(color = "#fafaf5",
                                     size = 0.1,
                                     fill = "#fafaf5"),
    legend.key.height = unit(1.25, "lines"),
    legend.key.width = unit(3.5, "lines"),
    plot.margin = margin(rep(20, 4))
  )


# save the plot

ggsave(
  "rugby7_ball.png",
  plot = last_plot(),
  width = 23,
  height = 15,
  dpi = 450,
  bg = "#fafaf5"
)


