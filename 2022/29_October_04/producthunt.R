product_hunt <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv")

library(tidyverse)
library(gt)
library(gtExtras)
library(webshot2)

products <- product_hunt |> select(-c(makers, last_updated, hunter, id, product_ranking))


top_25 <- products |>
    arrange(-upvotes) |>
    head(25)

top_25 |> view()

top_25

table <- top_25 |>
    select(c(
        name, product_description,
        category_tags, main_image,
        release_date, product_of_the_day_date,
        upvotes
    )) |>
    gt() |>
    gt_img_rows(
        columns = main_image,
        img_source = "web",
        height = 50
    ) |>
    gt_merge_stack(
        col1 = name,
        col2 = category_tags
    ) |>
    gt_plt_bar(column = upvotes, color = "#da552f", keep_column = TRUE) |>
    tab_header(
        title = "ProductHunt Top 25 (2014 - 2021)",
        subtitle = "The 25 most upvoted products on Product Hunt from 2014 to 2021.
        For “The Gamer and the Nihilist,” an essay
        in Components, Andrew Thompson and collaborators created a
        dataset of 76,000+ tech products on Product Hunt, a popular
        social network for launching and promoting such things.
        The dataset includes the name, description, launch date, upvote count,
        and other details for every product from 2014 to 2021 in the platform’s
        sitemap."
    ) |>
    fmt_date(
        columns = release_date,
        date_style = 3
    ) |>
    fmt_date(
        columns = product_of_the_day_date,
        date_style = 3
    ) |>
    gt_theme_nytimes() |>
    tab_options(
        table.background.color = "#ffffff",
        column_labels.background.color = "#ffffff",
        table.font.size = "17px"
    ) |>
    tab_style(
        locations = cells_title(groups = "title"), # format the main title
        style = list(
            cell_text(
                font = google_font(name = "Proxima Nova"),
                size = px(60),
                color = "#da552f",
                weight = 700
            )
        )
    ) |>
    opt_table_font(
        font = google_font(name = "Proxima Nova")
    ) |>
    tab_source_note(source_note = md(
        glue::glue(
            "Data : {fontawesome::fa('internet-explorer')} Components.one | Graphic : {fontawesome::fa('github')} github.com/SidhuK"
        )
    )) |>
    cols_label(
        name = "Name",
        main_image = "Product Image", release_date = "Release Date",
        product_description = "Description",
        product_of_the_day_date = "Product of the Day Date",
        upvotes = " Upvotes",
        upvotes = ""
    ) |>
    opt_stylize(style = 1, color = "gray")

table
setwd("/Users/karatatiwantsinghsidhu/Documents/Code/TidyTuesday/2022/29_October_04")


gtsave(table,
    "product_hunt2.png",
    vwidth = 1600,
    vheight = 1250
)
