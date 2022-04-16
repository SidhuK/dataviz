# Get the Data
# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

#Dataset Description:
#variable	class	description
#package	character	Package name
#version	character	Pkg version number
#date	datetime	Date and time when package was uploaded/updated
#rnw	integer	RNW (Sweave) based vignette count
#rmd	integer	RMD based vignette count

#Load the packages, not using most of them, just easy to have it all loaded.
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(magick)
library(scales)


# read the data using tidytuesdayR

tt <- tidytuesdayR::tt_load('2022-03-15')

# rename the data to a new dataframe

cran <- tt$cran

# data at a glance
glimpse(cran)
cran_summary <- skim(cran)

## cleaning up date variable
cran_r <- cran %>%
  mutate(date_clean = as.Date(date),
         date_clean = if_else(is.na(date_clean),
                              as.Date(date, format = "%c"),
                              date_clean)) %>%
  filter(!is.na(date_clean)) %>%
  select(-date) %>%
  rename(date = date_clean) %>%
  group_by(package) %>%
  slice(1) %>%
  select(package, date)

counts_overall <- cran_r %>%
  arrange(date) %>%
  ungroup() %>%
  mutate(cumsum = row_number())

# ggplot logo
ggplot_logo <- image_read("https://ggplot2.tidyverse.org/logo.png")
image_info(ggplot_logo)
tidy_logo <- image_read("https://tidyverse.tidyverse.org/logo.png")

# Making the actual plot

plot <- counts_overall %>%
  ggplot(aes(x = date, y = cumsum)) +
  geom_line(color = "black", size = 1.5) +
  scale_x_date(breaks = date_breaks("4 year"),
               labels = date_format("%Y")) +
  labs(
    y = "Total Number of Packages",
    x = NULL,
    title = "CRAN Packages",
    subtitle = str_wrap(
      "The figure shows sum of all packages on CRAN over a period of time. ",
      width = 50
    ),
    caption = "Figure: Karat Sidhu\nData: Robert Flight\nCode: https://github.com/SidhuK/TidyTuesday"
  ) +
  theme() + theme(
    plot.subtitle = element_text(
      size = 10,
      face = "bold",
      colour = "gray0",
      hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7,
      face = "italic",
      colour = "gray0"
    ),
    axis.ticks = element_line(colour = "gray0",
                              size = 0.9),
    panel.grid.major = element_line(
      colour = "gray96",
      size = 0.4,
      linetype = "dashed"
    ),
    panel.grid.minor = element_line(size = 0.4,
                                    linetype = "dashed"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(family = "mono",
                             colour = "gray0"),
    plot.title = element_text(size = 18,
                              face = "bold", hjust = 0.5),
    panel.background = element_rect(
      fill = "antiquewhite2",
      colour = "black",
      size = 1,
      linetype = "longdash"
    ),
    plot.background = element_rect(
      fill = "aliceblue",
      colour = "black",
      size = 1.5,
      linetype = "solid"
    ),
    legend.position = "none"
  ) + labs(x = "Year")

plot
grid::grid.raster(
  ggplot_logo,
  x = 0.01,
  y = 0.01,
  just = c('left', 'bottom'),
  width = unit(0.4, 'inches')
)
grid::grid.raster(
  tidy_logo,
  x = 0.05,
  y = 0.01,
  just = c('left', 'bottom'),
  width = unit(0.4, 'inches')
)
