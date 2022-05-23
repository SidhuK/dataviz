library(tidyverse)
library(gt)
library(gtExtras)
library(showtext)
library(RColorBrewer)


showtext_auto()
sysfonts::font_add("Premier League", regular = "FontsFree-Net-Barclays-Premier-League.ttf")

table <- read.csv("pl_table_gw_38.csv")








logos <-
    read.csv(
        "https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/team_mapping.csv"
    )

logos_cleaned <- logos |>
    select(squad_fbref, url_logo_espn) |>
    rename(Squad = "squad_fbref")

table <- table |>
    select(-c(Goalkeeper, Notes, Attendance))

table



table2 <- inner_join(table,
    logos_cleaned,
    by = "Squad"
) |>
    relocate(Rk, url_logo_espn)


table2

final_table <- table2 |>
    rename(` ` = "url_logo_espn") |>
    rename(Rank = "Rk") |>
    rename(`Top Scorer` = "Top.Team.Scorer") |>
    gt() |>
    gt_color_rows(
        columns = W,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$W), min(table2$W))
    ) |>
    gt_color_rows(
        columns = D,
        type = "discrete",
        direction = -1,
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$D), min(table2$D))
    ) |>
    gt_color_rows(
        columns = L,
        type = "discrete",
        direction = -1,
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$L), min(table2$L))
    ) |>
    gt_color_rows(
        columns = GF,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$GF), min(table2$GF))
    ) |>
    gt_color_rows(
        columns = GA,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        direction = -1,
        domain = c(min(table2$GA), max(table2$GA))
    ) |>
    gt_color_rows(
        columns = GD,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$GD), min(table2$GD))
    ) |>
    gt_color_rows(
        columns = Pts,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$Pts), min(table2$Pts))
    ) |>
    gt_color_rows(
        xG,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$xG), min(table2$xG))
    ) |>
    gt_color_rows(
        xGA,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        direction = -1,
        domain = c(min(table2$xGA), max(table2$xGA))
    ) |>
    gt_color_rows(
        xGD,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$xGD), min(table2$xGD))
    ) |>
    gt_color_rows(
        xGD.90,
        type = "discrete",
        palette = "RColorBrewer::Spectral",
        domain = c(max(table2$xGD.90), min(table2$xGD.90))
    ) |>
    gt_img_rows(
        columns = ` `,
        img_source = "web",
        height = 35
    ) |>
    tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(rows = Squad == "Manchester Utd")
    ) |>
    gt_theme_espn() |>
    tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(
            columns = Squad
        )
    ) |>
    tab_header(
        title = "2021-22 Premier League Standings",
        subtitle = "Final Results for the English Premier League saw Manchester City win the league after an exciting last few weeks of competition
    with Liverpool. The competition came down to the final few minutes of the season where City were crowned winners.
    The top 4 teams qualified for the UCL, 5th and 6th place teams are in the Europa League while the 7th place West Ham
    secured the UECL qualification. The bottom 3 teams, Norwich, Watford and Burnley were relegated to the English Championship (2nd tier competition);
    Everton and Leeds United narrowly escaped relegation."
    ) |>
    tab_footnote(
        footnote = "xG = Expected Goals (Higher is better)",
        locations = cells_column_labels(columns = xG)
    ) |>
    tab_footnote(
        footnote = "xGA = Expect Goals Against (Lower is better) ",
        locations = cells_column_labels(columns = xGA)
    ) |>
    tab_footnote(
        footnote = "xGD = xG Difference (xG - xGA (High is better))",
        locations = cells_column_labels(columns = xGD)
    ) |>
    tab_footnote(
        footnote = "xGD.90 = xG Difference per 90 mins (Higher is better)",
        locations = cells_column_labels(columns = xGD.90)
    ) |>
    tab_footnote(
        footnote = "The team that I support :(",
        locations = cells_body(
            columns = Squad,
            rows = 6
        )
    ) |>
    tab_source_note(
        source_note = "Data : FBRef | Table : Github.com/SidhuK"
    ) |>
    tab_style(
        locations = cells_title(groups = "title"), # format the main title
        style = list(
            cell_text(
                font = "Premier League",
                size = px(40),
                color = "black",
                weight = 700
            )
        )
    ) |>
    tab_options(
        table.background.color = "#eeeeee",
        column_labels.background.color = "#eeeeee"
    ) # set the bg color



gtsave(final_table,
    "premierLeague.png",
    vwidth = 900,
    vheight = 1500
)