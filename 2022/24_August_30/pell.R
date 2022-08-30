
# Pell Awards
# The data this week comes from US
# Department of EducationU.S. Department of Education.
# The data this week is already packaged in
# an R package hosted called Pell grant in CRAN.
# The original data source is US Department of Education.
# This package vignette talks in more detail about
# the data and how it was sourced.
# To check out how the data was sourced and cleaned check download
# data directly page in the vignette.
# Credit: Arafath Hossain


pell <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv")


library(tidyverse)

pell |>
    filter(NAME == "Texas A&m University")
