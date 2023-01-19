artists <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv"
  )
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-01-17/readme.md

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(showtext)
showtext_auto(enable = TRUE)
showtext_opts(dpi = 250)
font_add_google("Roboto", "roboto")

# artist_name column has a lot of repeated names, so I'm going to remove them
artists_2 <- artists |>
  distinct(artist_name, .keep_all = TRUE)


# how many artists
total_artists <- artists_2 |>
  count(artist_nationality, sort = TRUE)


# artists with N/A in their name
art_na <-
  artists_2 |> filter(artist_name == str_extract(artist_name, "N/A\\d*"))


# remove them
art <- anti_join(artists_2, art_na)


# add a row number/uniquely identify each artist - this will be used to plot the graph
art <- art |>
  group_by(artist_nationality) |>
  mutate(id = row_number())



# add the total number of artists from each country
art <- art |> left_join(total_artists)


# add a code for each artist, removing N/A
art <- art |>
  filter(artist_nationality != "N/A") |>
  filter(artist_race != "N/A") |>
  filter(n >= 6) |>
  mutate(code = paste0(artist_race, "/", artist_gender))


# how many distinct codes are there?
length(unique(art$code))

# make a vector of 8 colors
pal <-
  c(
    "#377eb8",
             "#4daf4a",
             "#984ea3",
             "#ff7f00",
             "#a65628",
             "#f781bf",
             "#3397b5",
             "#999999"
  )



# make the plot
ggplot(art, aes(
  x = fct_reorder2(artist_nationality,-n, code),
  y = id,
  label = artist_name,
  color = code
)) +
  geom_text(size = 2.5, family = "roboto") +
  ggtext::geom_richtext(
    data = data.frame(x = 4, y = 90, label = "The demographic representation of artists through editions of Janson's History of Art<br> and Gardner's Art Through the Ages, two of the most popular art<br> history textbooks used in the American education system.<br> The chart looks at artists from some of the most<br> represented nationalities, and their demographics such as their gender and race,<br> including: Female (<span style='color:#377eb8'>American Indian</span>, <span style='color:#984ea3'>Asian</span>, <span style='color:#a65628'>Black</span> & <span style='color:#3397b5'>White</span>) <br> & Male (<span style='color:#4daf4a'>American Indian</span>, <span style='color:#ff7f00'>Asian</span>, <span style='color:#f781bf'>Black</span> & <span style='color:#999999'>White</span>)"),
    mapping = aes(x = x, y = y, label = label),
    size = 5,
    family = "roboto",
    fontface = "bold",
    inherit.aes = FALSE,
    color = "grey45",
    fill = "#ffffff",
    label.size = 0
  ) +
  geom_text(
    data = data.frame(x = 4, y = 104, label = "Art History and Diversity"),
    mapping = aes(x = x, y = y, label = label),
    size = 18,
    family = "roboto",
    fontface = "bold",
    inherit.aes = FALSE,
    color = "grey30"
  ) +
  theme_void() +
  labs(caption = "Source: arthistory data package | Graphic: Github.com/SidhuK") +
  scale_color_manual(values = pal) +
  theme(
    axis.text.x = element_text(
      size = 10,
      family = "roboto",
      face = "bold"
    ),
    legend.position = "none",
    plot.caption = element_text(
      hjust = 0.5,
      family = "roboto",
      size = 8,
      face = "bold",
      color = "grey45"
    )
  )


# save the plot
ggsave(
  "art.png",
  plot = last_plot(),
  width = 20,
  height = 15,
  units = "in",
  dpi = 200,
  bg = "white"
)
