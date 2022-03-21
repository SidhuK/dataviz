# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# Loading the packages

library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(magick)
library(streamgraph)
library(gganimate)
library(hrbrthemes)

# Loading the dataset

tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
babynames <- tuesdata$babynames

# Take a general overview of the data
skim(babynames)


# check for most common names
babynames %>% 
  count(name) %>% arrange(desc(n))


# make a new subset of most frequently occurring names through the years.
babynames_common <- babynames %>% 
  group_by(name) %>% 
  mutate(freq = n()) %>% 
  ungroup() %>% 
  filter(freq > 275) %>%
  select(-freq)



sharedName <- babynames %>%
  mutate(male = ifelse(sex == "M", n, 0), female = ifelse(sex == "F", n, 0)) %>%
  group_by(name) %>%
  summarize(Male = as.numeric(sum(male)), 
            Female = as.numeric(sum(female)),
            count = as.numeric(sum(n)),
            AvgYear = round(as.numeric(sum(year * n) / sum(n)),0)) %>%
  filter(Male > 30000 & Female > 30000) %>%
  collect


babynames_popular <- babynames %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total))
babynames_popular

babynames_top7 <-  babynames %>% 
  filter(name %in% 
           c("James","John", "Robert", 
             "Mary", "William", "David","Joseph","Ricard", "Elizabeth")) 


babynames_top7 %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  geom_point() +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(year)






# babynames_common %>% count(name) %>% arrange(desc(n))
# 
# ggplot(babynames_common, aes(x=year, y=name, group = name))


# Common Babynames
# babynames_common %>%
#   ggplot( aes(x=year, y=n, group=name, color=name)) +
#   geom_line() +
#   geom_point() +
#   ggtitle("Popularity of American names in the previous 30 years") +
#   theme_ipsum() +
#   ylab("Number of babies born") +
#   transition_reveal(year)


# Tried it using the beer model - not great
# babynames_common %>% 
#   ggplot(aes(year, name, size = n, color = sex)) +
#   geom_point(data = babynames_common %>% group_by(name, year), 
#              color = "white", alpha = 1) +
#   geom_point(shape = 21, fill = NA, stroke = .5) +
#   coord_cartesian(clip = "off") +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_color_manual(values = c("#cd8d2a", "grey75", "#a45128")) + 
#   scale_fill_manual(values = c("#cd8d2a", "grey75", "#a45128")) +
#   scale_alpha_manual(values = c(1, .67, .33)) +
#   scale_size_area(max_size = 20)

streamgraph::streamgraph(babynames_top7, key="name", value="n", date="year", height="300px",
                                  width="1000px") %>%
  streamgraph::sg_legend(show=TRUE, label="names: ")
babies


# ggplot logo
ggplot_logo <- image_read("https://ggplot2.tidyverse.org/logo.png")
image_info(ggplot_logo)
tidy_logo <- image_read("https://tidyverse.tidyverse.org/logo.png")




# Adding the images at the bottom
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


