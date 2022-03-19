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

#Load the packages needed.
library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(skimr)
library(patchwork)
library(ggThemeAssist)
library(ggtext)
library(scales)

# read the data using tidytuesdayR

tt <- tidytuesdayR::tt_load('2022-03-15')

# rename the data to a new dataframe

cran <- tt$cran 

# data at a glance
glimpse(cran)
cran_summary <- skim(cran)

## cleaning up date variable
cran_r <-cran %>%
  mutate(
    date_clean = as.Date(date),
    date_clean = if_else(
      is.na(date_clean),
      as.Date(date, format = "%c"), 
      date_clean
    )
  ) %>%
  filter(!is.na(date_clean)) %>% 
  select(-date) %>%
  rename(
    date = date_clean
  ) %>%
  group_by(package) %>%
  slice(1) %>%
  select(package,date)
counts_overall <-
  cran_r %>%
  arrange(date) %>%
  ungroup() %>%
  mutate(cumsum = row_number()) 
counts_tidy <-
  cran_r %>%
  filter(str_detect(package,"tidy")) %>%
  arrange(date) %>%
  ungroup() %>%
  mutate(cumsum = row_number()) 


# Make plot

counts_overall %>%
  ggplot(aes(x = date, y = cumsum)) +
  geom_line(color = "grey", size = 1.5) +
  scale_x_date(breaks = date_breaks("4 year"),
               labels = date_format("%Y")) +
  geom_line(data = counts_tidy, aes(x = date, y = cumsum), color = "#ccb144", size = 2) +
  labs(y = "Cumulative sum of packages",
       x = NULL,
       title = "Development of 'tidy' packages on CRAN",
       subtitle = str_wrap("Top panel shows cumulative sum of all packages on CRAN. Zoomed panel (bottom, gold line) displays trends in packages that contain 'tidy' in name.", width = 50),
       caption = "Figure: Karat Sidhu\nData: Robert Flight\nCode: https://github.com/Sdbock/tidytuesday") +
  theme()
