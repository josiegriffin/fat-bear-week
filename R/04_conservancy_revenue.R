library(tidyverse)

# Katmai Conservancy IRS Form 990 total revenue by tax/calendar year, from
# ProPublica Nonprofit Explorer (EIN 81-2861724) and, for 2021 and 2024
# (not yet indexed by ProPublica), the Conservancy's own posted 990 PDFs.
# Org-wide revenue, not a Fat Bear Week-specific figure (see chat log).
# Conservancy formed August 2016, so no data before 2017; 2025 not yet filed.
revenue <- tribble(
  ~year, ~total_revenue,
  2017,  203307,
  2018,  215785,
  2019,  23982,
  2020,  549706,
  2021,  788985,
  2022,  650561,
  2023,  1628460,
  2024,  1325542
)

summary_tbl <- read_csv("data/summary.csv", show_col_types = FALSE, col_types = cols(
  champion = col_character(),
  runner_up = col_character()
))

summary_tbl <- summary_tbl |>
  left_join(revenue, by = "year")

write_csv(summary_tbl, "data/summary.csv")

print(summary_tbl, n = 12)
