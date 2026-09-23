# fat bear week

library(tidyverse)
library(ggstar)

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
# Shows each qualifying bear's yearly result across seasons (2026, still
# in progress, is excluded). "Qualifying" means the bear has at least 4
# appearances among 2014-2025.

min_appearances <- 4

completed_appearances <- appearances |>
  filter(year != 2026)

qualifying <- completed_appearances |>
  count(bear_id, name = "n_appearances") |>
  filter(n_appearances >= min_appearances)

career_data <- completed_appearances |>
  inner_join(qualifying, by = "bear_id") |>
  left_join(bear_names, by = "bear_id") |>
  mutate(
    label = if_else(is.na(name), bear_id, str_c(bear_id, name, sep = " ")),
    status = case_when(
      result == "champion" ~ "Champion",
      result == "runner_up" ~ "Runner-up",
      TRUE ~ "Entered"
    ) |> factor(levels = c("Entered", "Runner-up", "Champion"))
  ) |>
  mutate(first_year = min(year), .by = bear_id) |>
  mutate(label = fct_reorder(label, -first_year, .fun = min))

career_span <- career_data |>
  summarize(min_year = min(year), max_year = max(year), .by = c(bear_id, label))

career_chart <- ggplot(career_data, aes(y = label)) +
  geom_segment(
    data = career_span,
    aes(x = min_year, xend = max_year, yend = label),
    color = "grey60"
  ) +
  # starshape 15 is a plain circle and 1 is a five-pointed star (see
  # ggstar::show_starshapes()); fill distinguishes entered vs. runner-up
  geom_star(
    aes(x = year, starshape = status, fill = status, size = status),
    color = "black"
  ) +
  scale_starshape_manual(name = NULL, values = c("Entered" = 15, "Runner-up" = 15, "Champion" = 1)) +
  scale_fill_manual(name = NULL, values = c("Entered" = NA, "Runner-up" = "black", "Champion" = "black")) +
  scale_size_manual(
    values = c("Entered" = 2.5, "Runner-up" = 2.5, "Champion" = 3.5),
    guide = "none"
  ) +
  scale_x_continuous(breaks = min(career_data$year):max(career_data$year)) +
  labs(
    title = "Fat Bear Week",
    subtitle = str_glue("These bears have made the most appearances in the bracket."),
    x = NULL, y = NULL, size = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 28, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "black", margin = margin(b = 2), hjust = 0.5),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 12, margin = margin(r = 0)),
    legend.text = element_text(size = 12, margin = margin(l = 1)),
    legend.position = "top",
    legend.margin = margin(10, 0, 5, 0),
    legend.box.spacing = unit(0, "pt"),
    legend.key.size = unit(0.8, "lines"),
    legend.spacing.x = unit(12, "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )
career_chart

ggsave("career_chart.png", career_chart, width = 8, height = 6, dpi = 150)
ggsave("career_chart.svg", career_chart, width = 8, height = 6)
