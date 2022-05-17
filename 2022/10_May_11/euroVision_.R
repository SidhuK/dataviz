library(tidyverse)
library(gt)
library(gtExtras)


# load data

eurovision <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv"
  )


# remove columns not used

eurovision <- eurovision |>
  select(
    -c(
      event_url,
      artist_url,
      country_emoji,
      rank_ordinal,
      running_order,
      qualified,
      event
    )
  )

# finale winners

eurovision <- eurovision |>
  filter(section == "grand-final") |>
  group_by(year) |>
  mutate(average_points = mean(total_points)) |>
  ungroup()



# table data

eurovision_table <- eurovision |>
  group_by(artist_country) |>
  filter(section == "grand-final") |>
  filter(winner == "TRUE") |>
  select(-c(section, rank, winner)) |>
  select(year, everything())


# make the table

table  <- eurovision_table |>
  gt() |>
  gt_color_rows(total_points:average_points,
                type = "discrete",
                palette = "ggsci::orange_material",
  ) |>
  gt_img_rows(columns = image_url,
              img_source = "web",
              height = 30) |>
  gt_theme_538() |>
  tab_header(
    title = "Eurovision Song Contest Winners 2004 - 2022",
    subtitle = "Winners of the Eurovision Song contest grand finales from 2004 to 2022. An average of 26 countries participated
    in the song contest and the following countries won after advancing to the final rounds. The table details the winning artists
    and the total points for the corresponding winners; in addition total scores for each winner and the average score for that year.
    Data grouped by country."
  ) |>
  tab_source_note(source_note = md(
    glue::glue(
      "Data : {fontawesome::fa('twitter')} Tanya Shapiro | Graphic : {fontawesome::fa('github')} github.com/SidhuK"
    )
  )) |>
  tab_footnote(footnote = "Highest Points by a winner",
               locations = cells_row_groups(groups = "Portugal")) |>
  tab_style(locations = cells_title(groups = 'title'), # format the main title
            style = list(
              cell_text(
                font = google_font(name = 'Bebas Neue'),
                size = px(60),
                color = 'indianred',
                weight = 700
              )
            )) |>
  gt_merge_stack(col1 = host_city,
                 col2 = host_country) |> # trim the table a bit
  gt_merge_stack(col1 = artist,
                 col2 = song) |>
  tab_options(table.background.color = "#f1ebda",
              column_labels.background.color = "#f1ebda") # set the bg color




table


# save the table
gtsave(table,
       "Eurovision.png",
       vwidth = 1000,
       vheight = 2000)
