# Loading Libraries
library(tidyverse)


# Loading Data
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)
death_fuel <- tuesdata$death_fuel
indoor_pollution <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv'
  )

# Cleaning Data Columns
indoor_pollution <- indoor_pollution %>%
  rename(Percentage_Deaths =
           `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`)

# Adding the continent column, removing the un-needed columns
df_new <-
  full_join(indoor_pollution,
            death_fuel,
            by = c("Entity", "Code", "Year" = "Year...3")) %>%
  select(
    -c(
      `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: All Ages (Number)...4`,
      `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: All Ages (Number)...5`,
      Year...6
    )
  )

# Not all of them contain continents, so making another dataframe to get it.
df_continent <- df_new %>%
  group_by(Entity, Continent) %>%
  summarise(Total = n()) %>%  na.omit()


# New Dataframe with each country containing continent
df_cleaned <- left_join(df_new, df_continent, by = "Entity") %>%
  select(-c(Continent.x, Total)) %>%
  na.omit() %>%
  rename(Continent = Continent.y)

# Difference in deaths in 1990 vs 2019
df_difference <- df_cleaned %>%
  filter(Year == c(1990, 2019)) %>%
  group_by(Entity, Continent) %>%
  summarise(Change = diff(Percentage_Deaths)) %>%
  arrange(desc(Change)) %>%
  mutate(Gain = case_when(Change > 0 ~ "Gain",
                          Change < 0 ~ "Loss"))




# Plot for the biggest movers in either direction

df_difference %>%
  arrange(desc(Change)) %>%
  filter(Change > 0 | Change < -9) %>%
  ggplot(aes(
    x = reorder(Entity, Change),
    fill = Gain,
    weight = Change
  )) +
  geom_bar() +
  scale_fill_manual(values = c(Gain = "#DFDFDE",
                               Loss = "#F56D91")) +
  theme_minimal() +
  facet_wrap(vars(Gain), scales = "free") +
  coord_flip() +
  labs(
    y = "Percent - Change in Death Rate",
    x = element_blank(),
    title = "Indoor Air Pollution Death Rates",
    subtitle  = "Indoor air pollution is caused by burning solid fuel sources – such as firewood, crop waste, and dung – for cooking and heating.
The burning of such fuels, particularly in poor households, results in air pollution that leads to respiratory diseases which can result in premature death. The WHO calls indoor air pollution “the world’s largest single environmental health risk.”
Between 1990 and 2019 the following countries showed the biggest increases and decreases in the percentage of death-rate",
caption = "Data Source: OurWorldInData.org | Graphic : github.com/SidhuK"
  ) +
  theme(
    legend.position = "none",
    plot.title  = element_text(face = "bold"),
    plot.background = element_rect("#8D8DAA"),
    panel.grid = element_blank(),
    strip.text.x = element_blank(),
    text =  element_text(colour = "white"),
    axis.text = element_text(color = "white")
  )



ggsave(
  "change.svg",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 200
)
