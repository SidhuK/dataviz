# Loading Packages
library(tidyverse)
library(patchwork)


# reading raw data
news_orgs <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv'
  )



# selecting just nyc data, we'll be using. 

new_york_news <- news_orgs %>%
  filter(country == "United States" &
           primary_language == "English" & state == "NY") %>%
  mutate(budget_editorial = as.factor(budget_percent_editorial)) %>%
  mutate(budget_editorial = as.numeric(budget_editorial)) %>%
  mutate(budget_editorial = (budget_editorial * 10) + 5) %>%
  mutate(budget_revenue = as.factor(budget_percent_revenue_generation)) %>%
  mutate(budget_revenue = as.numeric(budget_revenue)) %>%
  mutate(budget_revenue = (budget_revenue * 10) + 5) %>%
  select(publication_name,
         budget_revenue,
         budget_editorial,
         tax_status_current) %>%
  na.omit() # can't use NA's so removed.



new_york_news <- tibble::rowid_to_column(new_york_news, "ID")

# angle needed for geom_col 
new_york_news <- new_york_news %>%
  mutate(angle = 90 - 360 * (ID - 0.5) / nrow(new_york_news)) # 0.5 makes it in the middle of the bar.


# add hjust values depending on the angle
new_york_news$hjust <- ifelse(new_york_news$angle < -90, 1, 0)
new_york_news$angle <-
  ifelse(new_york_news$angle < -90,
         new_york_news$angle + 180,
         new_york_news$angle)

# editorial budget
p1<- ggplot(
  new_york_news,
  aes(x = publication_name,
      y = budget_editorial,
      fill = tax_status_current)
) +
  geom_bar(stat = "identity",
           alpha = 0.9,
           color = "grey35") +
  ylim(-50, 120) +
  theme_minimal() +
  coord_polar() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = unit(rep(-2, 4), "cm")     # This remove unnecessary margin around plot
  ) +
  scale_fill_brewer(palette = "Accent", direction = 1) +
  geom_text(
    aes(
      x = publication_name,
      y = budget_editorial + 10,
      label = publication_name,
      angle = angle,
      hjust = hjust
    ),
    size = 2.5,
    alpha = 0.7,
    inherit.aes = FALSE
  )



# revenue generation budget.

p2<- ggplot(new_york_news,
       aes(x = publication_name,
           y = budget_revenue,
           fill = tax_status_current)) +
  geom_bar(stat = "identity",
           alpha = 0.9,
           color = "grey35") +
  ylim(-50, 120) +
  theme_minimal() +
  coord_polar() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = unit(rep(-2, 4), "cm")     # This remove unnecessary margin around plot
  ) +
  scale_fill_brewer(palette = "Accent", direction = 1) +
  geom_text(
    aes(
      x = publication_name,
      y = budget_revenue + 10,
      label = publication_name,
      angle = angle,
      hjust = hjust
    ),
    size = 2.5,
    alpha = 0.7,
    inherit.aes = FALSE
  )


# combining graph
p <- p1 + p2

p_final<- p + plot_annotation(
  title = 'New York News Budget Allocation',
  subtitle = 'Budget Revenue vs Editorial',
  caption = 'Data : TidyTuesday, Graphic: github.com/SidhuK'
)


p_final


# save for editing on affinity.
ggsave("export.svg", p_final, 
       width = 20,
       height = 15,
       dpi = 300,
)


# spare plot for legend.
ggplot(new_york_news,
       aes(x = publication_name,
           y = budget_revenue,
           fill = tax_status_current)) +
  geom_bar(stat = "identity",
           alpha = 0.8,
           color = "grey35") +
  ylim(-50, 120) +
  theme_minimal() +
  coord_polar() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2, 4), "cm")     # This remove unnecessary margin around plot
  ) +
  scale_fill_brewer(palette = "Accent", direction = 1) +
  geom_text(
    aes(
      x = publication_name,
      y = budget_revenue + 10,
      label = publication_name,
      angle = angle,
      hjust = hjust
    ),
    size = 2.5,
    alpha = 0.7,
    inherit.aes = FALSE
  )


ggsave("export2.svg", 
       width = 20,
       height = 15,
       dpi = 300,
)

