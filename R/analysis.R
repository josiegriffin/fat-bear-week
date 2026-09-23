# fat bear week

library(tidyverse)
library(ggstar)
library(patchwork)
library(ggtext)

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

# --- Per-bear contest chart ---------------------------------------------
# Shows each qualifying bear's yearly result across seasons (2026, still
# in progress, is excluded). "Qualifying" means the bear has at least 4
# appearances among 2014-2025, plus 409 added by request regardless of her
# count (2: champion in 2015 and 2018) - TODO: this pushes the chart to 8
# bears; bump the lowest one back out once we decide who.
min_appearances <- 4
force_include <- c("409")

completed_appearances <- appearances |>
  filter(year != 2026)

qualifying <- completed_appearances |>
  count(bear_id, name = "n_appearances") |>
  filter(n_appearances >= min_appearances | bear_id %in% force_include)

contest_data <- completed_appearances |>
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

contest_span <- contest_data |>
  summarize(min_year = min(year), max_year = max(year), .by = c(bear_id, label))

contest_years <- min(contest_data$year):max(contest_data$year)

# Show the first and last year in full (e.g. "2014"); abbreviate years in
# between to two digits with a leading apostrophe (e.g. "'15")
abbreviate_years <- function(year) {
  if_else(
    year %in% range(contest_years),
    as.character(year),
    str_c("'", str_sub(year, 3, 4))
  )
}

title_text <- "It's Fat Bear Week!"
subtitle_text <- "Below are the historical top competitors for fattest bear in Katmai National Park Alaska, which has occurred annually since 2014. All bears have a numerical ID and some bears also have a name."
title_style <- element_text(face = "bold", size = 28, hjust = 0.5)

# Both the subtitle and caption are boxed with this same style. Applying it
# via plot_annotation() (rather than each chart's own labs()/theme()) means
# both boxes are sized against the same reference frame - the full
# plot/figure width - so they always render at the same width as each
# other, instead of one being scoped to a single panel's narrower width.
boxed_text <- function(outer_margin) {
  element_textbox_simple(
    size = 11, face = "bold", color = "black", hjust = 0.5, halign = 0.5,
    margin = outer_margin, padding = margin(5, 8, 5, 8),
    linetype = 1, box.color = "grey60", linewidth = 0.4, fill = NA
  )
}

contest_chart <- ggplot(contest_data, aes(y = label)) +
  geom_segment(
    data = contest_span,
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
  scale_x_continuous(breaks = contest_years, labels = abbreviate_years, limits = range(contest_years)) +
  labs(x = NULL, y = NULL, size = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
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

# Title and subtitle applied via plot_annotation() (see boxed_text() above)
# rather than contest_chart's own labs()/theme(), so the subtitle box width
# matches the caption's when this chart is reused (as contest_chart_top) in
# the combined contest_and_overlay figure below
contest_chart_standalone <- contest_chart +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    theme = theme(plot.title = title_style, plot.subtitle = boxed_text(margin(t = 4, b = 6)))
  )
contest_chart_standalone

# --- Katmai Conservancy revenue & total votes, matched to the same year
# range as the contest chart -----------------------------------------
# Revenue is missing for 2014-2016 and 2025; votes are missing for
# 2015-2016. Votes are scaled onto the same numeric range as revenue so
# both series can share a single y-axis (with a secondary axis showing
# the true vote counts).
overlay_data <- summary_out |>
  filter(year %in% contest_years) |>
  select(year, total_revenue, total_votes)

# Years missing either value (2014-2016 revenue, 2015-2016 votes) are
# dropped pairwise rather than imputed
revenue_votes_cor <- cor(
  overlay_data$total_revenue, overlay_data$total_votes,
  use = "pairwise.complete.obs"
)

caption_text <- str_glue(
  "The total revenue for the Katmai Conservancy that supports the bears is ",
  "highly correlated (r = {round(revenue_votes_cor, 2)}) to the number of ",
  "votes cast in the Fat Bear Week contest. (Data sourced from public 990 ",
  "tax filings)"
)

scale_factor <- max(overlay_data$total_revenue, na.rm = TRUE) /
  max(overlay_data$total_votes, na.rm = TRUE)

# The 2018-2025 votes points are drawn with a solid line, but 2014's vote
# total is otherwise isolated (2015-2016 are missing); connect it to the
# rest of the series with a dotted line to signal that the gap is bridged
# for visual continuity only, not an actual observed trend
votes_gap <- overlay_data |>
  filter(year %in% c(2014, 2018))

# Grayscale only: distinguish the two series by both shade and linetype
# so they're still distinguishable from each other
overlay_chart <- ggplot(overlay_data, aes(x = year)) +
  geom_line(aes(y = total_revenue, color = "Revenue", linetype = "Revenue")) +
  geom_point(aes(y = total_revenue, color = "Revenue"), size = 2) +
  geom_line(aes(y = total_votes * scale_factor, color = "Total votes", linetype = "Total votes")) +
  geom_line(
    data = votes_gap,
    aes(y = total_votes * scale_factor),
    # Custom dash pattern ("11") packs the dots closer together than the
    # built-in "dotted" linetype
    linetype = "11", color = "black", inherit.aes = TRUE
  ) +
  geom_point(aes(y = total_votes * scale_factor, color = "Total votes"), size = 2) +
  scale_x_continuous(breaks = contest_years, labels = abbreviate_years, limits = range(contest_years)) +
  scale_y_continuous(
    name = "Total revenue",
    labels = scales::label_dollar(scale = 1e-6, suffix = "M"),
    sec.axis = sec_axis(~ . / scale_factor, name = "Total votes", labels = scales::label_comma())
  ) +
  scale_color_manual(name = NULL, values = c("Revenue" = "grey40", "Total votes" = "black")) +
  scale_linetype_manual(name = NULL, values = c("Revenue" = "dashed", "Total votes" = "solid")) +
  labs(x = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 12),
    # The left title's true gap is larger than its 8pt margin because
    # patchwork pads the shared left axis-text column to match the width
    # of the contest chart's (much wider) bear-name axis text above it.
    # The right side gets no such padding, so its margin is increased to
    # compensate and make the two gaps look equal.
    axis.title.y = element_text(size = 13, margin = margin(r = 4)),
    axis.title.y.right = element_text(size = 13, margin = margin(l = 24)),
    # Bottom placement keeps the legend away from the shared year-axis
    # row, which sits above this panel in the stacked figure
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
overlay_chart

# For the stacked figure, drop the top chart's x-axis text/ticks (the
# bottom chart's shows the shared years) but keep its vertical gridlines
# so each bear's points still visually line up with the year below
contest_chart_top <- contest_chart +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(b = 2)
  )

# Move the shared year axis to the top of the overlay panel, so it sits
# between the two charts rather than repeating below the contest chart
# and again below the overlay chart
overlay_chart_bottom <- overlay_chart +
  scale_x_continuous(
    breaks = contest_years, labels = abbreviate_years, limits = range(contest_years),
    position = "top"
  ) +
  theme(
    axis.text.x.top = element_text(color = "black", size = 12, face = "bold"),
    axis.ticks.x.top = element_blank(),
    plot.margin = margin(t = 2)
  )

contest_and_overlay <- contest_chart_top / overlay_chart_bottom +
  plot_layout(heights = c(3, 1.5), axis_titles = "collect") +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    # TODO: replace with real caption text (e.g. data source, units, notes)
    caption = caption_text,
    theme = theme(
      plot.title = title_style,
      plot.subtitle = boxed_text(margin(t = 4, b = 6)),
      plot.caption = boxed_text(margin(t = 6, b = 2))
    )
  )
contest_and_overlay

ggsave("contest_chart.png", contest_chart_standalone, width = 8, height = 6, dpi = 150)
ggsave("contest_chart.svg", contest_chart_standalone, width = 8, height = 6)
ggsave("contest_and_revenue.png", contest_and_overlay, width = 8, height = 9, dpi = 150)
ggsave("contest_and_revenue.svg", contest_and_overlay, width = 8, height = 9)
