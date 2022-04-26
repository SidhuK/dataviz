library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(showtext)
library(patchwork)
showtext_auto()
font_add_google("Inter", "Inter")


hidden_gems <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv'
  )


# hidden_gems %>%
#   group_by(author_name) %>%
#   summarise(total = n()) %>%
#   arrange(desc(total))
#
# get_sentiments("bing")
#
# hidden_gems %>%
#   filter(grepl(" R | Python", review))
#


hidden_gems <- hidden_gems %>%
  select(-c(link_forum,
            link_twitter,
            notebook))

hidden_gems

review_plot <- hidden_gems %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  head(50) %>%
  ggplot(aes(
    label = word,
    size = freq,
    color = freq
  )) +
  geom_text_wordcloud(family = "Inter",
                      eccentricity = 1,
                      area_corr = TRUE) +
  theme_minimal() +
  labs(title = "Review") +
  scale_size_area(max_size = 17) +
  theme(plot.background = element_rect(fill = "#000B49", color = "#000B49")) +
  scale_color_gradient(low = "#FF7272", high = "#9B0000")



title_plot <- hidden_gems %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  head(50) %>%
  ggplot(aes(
    label = word,
    size = freq,
    color = freq
  )) +
  geom_text_wordcloud(family = "Inter",
                      eccentricity = 1,
                      area_corr = TRUE) +
  theme_minimal() +
  labs(title = "Title") +
  scale_size_area(max_size = 17) +
  theme(plot.background = element_rect(fill = "#000B49", color = "#000B49")) +
  scale_color_gradient(low = "#3E497A", high = "#F1D00A")



combined_plot <- title_plot + review_plot

combined_plot


ggsave(
  "kaggle.png",
  plot = last_plot(),
  width = 20,
  height = 10,
  dpi = 250
)
