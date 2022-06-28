

library(tidyverse)
library(ggtext)
library(showtext)
showtext_auto(enable = TRUE)
font_add_google("Graduate", "Graduate")

paygap <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv"
  )



clubs <-
  c(
    "AFC BOURNEMOUTH LIMITED",
    "THE ARSENAL FOOTBALL CLUB PUBLIC LIMITED COMPANY",
    "RECON SPORTS LIMITED",
    "BRENTFORD FC LIMITED",
    "BRIGHTON AND HOVE ALBION FOOTBALL CLUB, LIMITED(THE)",
    "CHELSEA FOOTBALL CLUB LIMITED",
    "CPFC LIMITED",
    "EVERTON FOOTBALL CLUB COMPANY, LIMITED",
    "FULHAM FOOTBALL CLUB LIMITED",
    "LEEDS UNITED FOOTBALL CLUB LIMITED",
    "LEICESTER CITY FOOTBALL CLUB LIMITED",
    "THE LIVERPOOL FOOTBALL CLUB AND ATHLETIC GROUNDS LIMITED",
    "MANCHESTER CITY FOOTBALL CLUB LIMITED",
    "MANCHESTER UNITED LIMITED",
    "MANCHESTER UNITED FOOTBALL CLUB LIMITED",
    "NEWCASTLE UNITED FOOTBALL COMPANY LIMITED",
    "NOTTINGHAM FOREST FOOTBALL CLUB LIMITED",
    "SOUTHAMPTON FOOTBALL CLUB LIMITED",
    "Tottenham Hotspur Football & Athletic Co. Ltd",
    "TOTTENHAM HOTSPUR FOOTBALL & ATHLETIC CO. LTD",
    "WEST HAM UNITED FOOTBALL CLUB LIMITED",
    "WOLVERHAMPTON WANDERERS FOOTBALL CLUB (1986) LIMITED"
  )

premier_league <- paygap |>
  filter(employer_name %in% clubs)

average_paygap <- mean(paygap$diff_median_hourly_percent)

logos <- read_csv("logos.csv")


pl <- premier_league |>
  mutate(year = lubridate::year(date_submitted))

epl <- full_join(pl, logos, by = "employer_name")


epl <- epl |>
  mutate(image = paste0("<img src ='", url_logo_espn, "' width='20'>"))


epl_average <- epl |>
  group_by(team, image) |>
  summarise(average = mean(diff_median_hourly_percent))


# ggplot(epl) +
#   aes(x = image, y = diff_mean_hourly_percent) +
#   geom_col(fill = "#112446") +
#   coord_flip() +
#   theme_minimal() +
#   facet_wrap(vars(year), scales = "free", nrow = 1L) +
#   theme(
#     axis.text.y = element_markdown(color = "black", size = 11)
#   )



epl |>
  ggplot(aes(y = image, x = diff_median_hourly_percent)) +
  geom_point(aes(color = as.factor(year))) +
  geom_richtext(aes(x = 90, y = image, label = image),
    hjust = 0, label.color = NA,
  ) +
  geom_text(aes(x = 100, y = image, label = team),
    family = "Graduate",
    hjust = 0, size = 7, color = "grey55"
  ) +
  geom_vline(aes(xintercept = average_paygap), linetype = "dashed") +
  geom_point(
    data = epl_average, aes(x = average, y = image),
    inherit.aes = FALSE, size = 4, shape = 18
  ) +
  geom_segment(
    data = epl_average,
    aes(x = average, xend = average_paygap, y = image, yend = image)
  ) +
  geom_curve(
    data = data.frame(x = 12.2, y = 5.5, xend = 22, yend = 5.5),
    mapping = aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(
      30,
      unit(0.1, "inches"), "last", "closed"
    ), inherit.aes = FALSE
  ) +
  geom_text(
    data = data.frame(
      x = 45, y = 6,
      label = "UK (Nationwide) Median - 12.21 %"
    ), mapping = aes(x = x, y = y, label = label),
    family = "Graduate", inherit.aes = FALSE
  ) +
  labs(
    title = "Gender pay gap in the English Premier League",
    subtitle = "Median Difference between male & female  pay for the 20 EPL clubs from 2018 to 2022,
    <br> Positive value indicates men's median hourly pay is higher.<br>
    The black points for each club represent the difference in the average % difference vs the UK Median Average",
    caption = "Data:  gender-pay-gap.service.gov.uk
    Graphic: Github.com/SidhuK"
  ) +
  xlab("Median Hourly % Difference") +
  scale_color_manual(name = "Year", values = c(
    "#0485ff",
    "#ffc400",
    "#ff4000",
    "#8ac926",
    "#fb00ff"
  )) +
  expand_limits(x = c(-75, 150)) +
  scale_x_continuous(
    breaks = seq(from = -75, to = 75, by = 25)
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(color = "grey40", size = 7, family = "Abel"),
    legend.background = element_rect(
      color = "#ffffff",
      size = 0.2,
    ),
    legend.title = element_text(family = "Graduate", size = 11),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Graduate", size = 11, hjust = 0),
    axis.text.x = element_text(family = "Graduate", size = 9),
    legend.key.height = unit(1.25, "lines"),
    legend.key.width = unit(2.5, "lines"),
    plot.title = element_text(size = 25, family = "Graduate"),
    plot.subtitle = element_markdown(size = 15, family = "Graduate"),
    plot.caption = element_text(size = 11, family = "Graduate"),
    panel.grid.major.y = element_blank()
  )



ggsave("epl_pay.png", plot = last_plot(), bg = "#ffffff", width = 16, height = 10, dpi = 300)
