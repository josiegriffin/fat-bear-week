# fat bear week

library(tidyverse)

appearances <- read_csv("data/appearances.csv", show_col_types = FALSE)
summary_df <- read_csv("data/summary.csv", show_col_types = FALSE)

# Note: 2014-2020 entries reflect finalists only, since the full roster
# wasn't tracked for those years (roster_complete_for_year == FALSE)
entries_per_year <- appearances |>
  count(year, name = "entries")

summary_out <- summary_df |>
  left_join(entries_per_year, by = "year") |>
  relocate(entries, .after = year)

write_csv(summary_out, "data/summary.csv")
