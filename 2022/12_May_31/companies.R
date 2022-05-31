
# loading the libraries

library(tidyverse)
library(ggdist)
library(showtext)
library(ggtext)
sysfonts::font_add_google("Anton","Anton")

font_add_google("Anton","Anton")

font_add_google("Bebas Neue","Bebas Neue")


# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-05-31

# add data
poll <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv"
  )
reputation <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv"
  )



# Using the following palette()
# https://colorhunt.co/palette/c0d8c0f5eedcdd4a48ecb390

# highest and lowest scores in all categories
max_min <- reputation |>
  group_by(name) |>
  slice(which(score == max(score)), which(score == min(score))) 


# max labels
maximum <- reputation |>
  group_by(name) |>
  slice(which(score == max(score)))

# min labels
minimum <- reputation |>
  group_by(name) |>
  slice(which(score == min(score)))


# median scores
median <- reputation |>
  group_by(name) |>
  summarise(median = median(score))



# text position (categories)
label_position <-reputation |> 
  group_by(name) |> 
  summarise(lab_position = min(score)+5) # +5 is to prevent a bit of overlapping


# main plot
reputation |>
  ggplot() +
  geom_text(
    aes(x =reorder(name, -lab_position),y = lab_position ,label = name),
    data = label_position,
    family = "Bebas Neue",
    color = "#C0D8C0",
   fontface = "bold", vjust = 0,hjust = 0.01,
    size = 25) +
  stat_dots(aes(x =factor(name) ,y = score),
            
            color = "#DD4A48",
            fill = "#DD4A48",
  side = "right", dotsize = 0.8) +
  stat_pointinterval(aes(x =name,y = score),
                     side = "left",position=position_nudge(x=-.03, y=0)) +
  ggrepel::geom_text_repel(aes(x = name,y = score,label = (company)),
                                size = 5,
                           family ="Bebas Neue",
                           data = maximum,
                                nudge_x = .15,
                                box.padding = 0.5,
                                nudge_y = 2,
                                segment.curvature = -0.2,
                                segment.ncp = 3,
                                segment.angle = 45,
                                color = "#DD4A48"
  )+
  ggrepel::geom_text_repel(aes(x = name,y = score,label = (company)),
                           size = 6,
                           family ="Bebas Neue",
                           data = minimum,
                           nudge_x = .15,
                           box.padding = 0.5,
                           nudge_y = -2,
                           segment.curvature = 0.2,
                           segment.ncp = 3,
                           segment.angle = 90,
                           color = "#DD4A48"
  ) +
  geom_text(aes(x=name, y = score, label = score), data = max_min,
            family ="Bebas Neue",
            
            color = "#DD4A48",
            fontface = "bold", vjust = 2.5,hjust = 0.5,
            size = 5) +
  geom_text(aes(x = name, y = median, label = median), data = median,
            size = 5,
            family ="Bebas Neue",
            color = "#DD4A48",
            fontface = "bold", vjust = 2.5,hjust = 0.5) +
  labs(
    title = "TOP 100 BRANDS IN AMERICA",
    subtitle = "This survey is the result of a partnership between Axios and Harris Poll to gauge the reputation
    of the most visible brands <br> 
    in America, based on 20 years of Harris Poll research. Each brand's repuation is 
    further divided into different subcategories. <br> 
    All ranks and ratings applicable for current year only. The three numerical 
    values for each category represent the corresponding(from left to right) <br>
    the minimum, the median and the maximum score
    ",
    caption = "P&S = Product and Service <br>
    Data: Axios-Harris Poll | Graphic : Github.com/SidhuK"
  ) +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown( face = "bold",size = 70,family = "Bebas Neue", color ="#DD4A48", hjust = 0.5), 
        plot.subtitle = element_markdown( face = "bold",size = 20, color ="#ECB390",family = "Bebas Neue", hjust = 0.5),
        plot.caption = element_markdown( face = "bold",size = 10, color ="#ECB390",family = "Bebas Neue", hjust = 0.5),
        plot.background = element_rect(color = NA, fill = "#F5EEDC"),
        plot.margin =unit(c(1,1,1,1), "cm" ))

 

ggsave("companies2.png", plot = last_plot(), width = 23, height = 17, dpi = 350)


