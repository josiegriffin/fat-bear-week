# fat bear week

library(tidyverse)

appearances <- read_csv("data/appearances.csv", show_col_types = FALSE)
summary_df <- read_csv("data/summary.csv", show_col_types = FALSE)
bears <- read_csv("data/bears.csv", show_col_types = FALSE)

bear_names <- bears |>
  mutate(bear_id = as.character(bear_id)) |>
  select(bear_id, name)

# Build "<bear_id> <name>" labels, falling back to just the bear_id when
# no name is on record (e.g. "480 Otis" vs. "410")
label_bears <- function(bear_id) {
  tibble(bear_id = as.character(bear_id)) |>
    left_join(bear_names, by = "bear_id") |>
    mutate(label = if_else(is.na(name), bear_id, str_c(bear_id, name, sep = " "))) |>
    pull(label)
}

# Note: 2014-2020 entries reflect finalists only, since the full roster
# wasn't tracked for those years (roster_complete_for_year == FALSE)
entries_per_year <- appearances |>
  mutate(bear_label = label_bears(bear_id)) |>
  arrange(bear_label) |>
  summarize(entries = str_flatten(bear_label, collapse = ", "), .by = year)

summary_out <- summary_df |>
  select(-any_of("entries")) |>
  left_join(entries_per_year, by = "year") |>
  relocate(entries, .after = year) |>
  mutate(
    year = as.integer(year),
    champion = label_bears(champion),
    runner_up = label_bears(runner_up)
  )

write_csv(summary_out, "data/summary.csv")

# --- Per-bear career chart ---------------------------------------------
# Shows each qualifying bear's yearly result across seasons. "Qualifying"
# means the bear has at least `min_appearances` rows in appearances.csv;
# 2014-2020 only tracked finalists (roster_complete_for_year == FALSE), so
# that period is shaded and annotated.

career_chart_data <- function(min_appearances) {
  qualifying <- appearances |>
    count(bear_id, name = "n_appearances") |>
    filter(n_appearances >= min_appearances)

  appearances |>
    inner_join(qualifying, by = "bear_id") |>
    left_join(bear_names, by = "bear_id") |>
    mutate(
      label = if_else(is.na(name), bear_id, str_c(bear_id, name, sep = " ")),
      status = case_when(
        result == "champion" ~ "Champion",
        result == "runner_up" ~ "Runner-up",
        result == "in_progress" ~ "In progress (2026)",
        TRUE ~ "Entered"
      ) |> factor(levels = c("Entered", "In progress (2026)", "Runner-up", "Champion"))
    ) |>
    mutate(first_year = min(year), .by = bear_id) |>
    mutate(label = fct_reorder(label, -first_year, .fun = min))
}

plot_career_chart <- function(min_appearances) {
  df <- career_chart_data(min_appearances)

  career_span <- df |>
    summarize(min_year = min(year), max_year = max(year), .by = c(bear_id, label))

  ggplot(df, aes(y = label)) +
    annotate(
      "rect",
      xmin = -Inf, xmax = 2020.5, ymin = -Inf, ymax = Inf,
      fill = "#efe9df", alpha = 0.6
    ) +
    annotate(
      "text",
      x = 2017.25, y = Inf, label = "2014\u20132020: finalists only",
      vjust = 1.5, color = "grey40", size = 3.2
    ) +
    geom_segment(
      data = career_span,
      aes(x = min_year, xend = max_year, yend = label),
      color = "grey75"
    ) +
    geom_point(
      aes(x = year, shape = status, color = status, fill = status, size = status),
      stroke = 1.1
    ) +
    scale_shape_manual(values = c(
      "Entered" = 21, "In progress (2026)" = 21, "Runner-up" = 21, "Champion" = 23
    )) +
    scale_color_manual(values = c(
      "Entered" = "grey55", "In progress (2026)" = "black",
      "Runner-up" = "#4472a8", "Champion" = "#1f3864"
    )) +
    scale_fill_manual(values = c(
      "Entered" = "grey55", "In progress (2026)" = NA,
      "Runner-up" = "#7fa4cf", "Champion" = "#1f3864"
    )) +
    scale_size_manual(values = c(
      "Entered" = 3, "In progress (2026)" = 3, "Runner-up" = 3, "Champion" = 4.5
    )) +
    scale_x_continuous(breaks = min(df$year):max(df$year)) +
    labs(
      title = "Fat Bear Week careers",
      subtitle = str_glue("Bears with {min_appearances}+ appearances"),
      x = NULL, y = NULL, shape = NULL, color = NULL, fill = NULL, size = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )
}

ggsave("career_chart_3plus.png", plot_career_chart(3), width = 8, height = 6, dpi = 150)
ggsave("career_chart_4plus.png", plot_career_chart(4), width = 8, height = 6, dpi = 150)
