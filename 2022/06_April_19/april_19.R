library(tidyverse)
library(waffle)
library(ggtext)

# Get the Data
big_dave <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv'
  )
times <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv'
  )


big_dave %>%
  filter(nchar(answer) > 10) %>%
  group_by(answer) %>%
  summarise(common  = n()) %>%
  arrange(desc(common)) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(
    x = reorder(answer, -common),
    y = common,
    fill = answer
  ))



#plot the waffle chart
times %>%
  filter(nchar(answer) >= 1) %>%
  group_by(answer) %>%
  summarise(common  = n()) %>%
  arrange(desc(common)) %>%
  head(10) %>%
  ggplot(aes(fill = answer, values = common)) +
  geom_waffle(
    n_rows = 10,
    size = 0.33,
    colour = "white",
    flip = TRUE
  ) + coord_equal() +
  theme_void() +
  theme_enhance_waffle() +
  scale_fill_manual(
    name = NULL,
    values = c (
      "#BB6464",
      "#CDB699" ,
      "#C3DBD9",
      "#C8F2EF" ,
      "#655D8A" ,
      "#7897AB",
      "#D885A3",
      "#FDCEB9",
      "#54BAB9" ,
      "#E9DAC1"
    )
  ) +
  theme(
    plot.background = element_rect("#F0ECE3"),
    legend.text = element_text(color = "black",
                               size = 25),
    legend.title = element_blank()
  ) +
  labs(
    title = "Times Crossword Puzzle - Most Common Answers",
    subtitle = " Data collected from 2012-12-27 to 2021-09-12, consisting of over 100 thousand observed answers <br>
       cryptics.georgeho.org is a dataset of cryptic crossword1 clues, indicators and charades,
       collected from various blogs and digital archives.",
    caption = "Data Source: cryptics.georgeho.org <br>
       Graphic: github.com/SidhuK"
  ) +
  theme(plot.subtitle = element_markdown(),
        plot.caption = element_markdown())

ggsave(
  "times_answers.png",
  width = 15,
  height = 20,
  dpi = 200,
  plot = last_plot()
)
