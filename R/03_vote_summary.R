library(tidyverse)

# hand-compiled from fresh 2026-09-22 lookups (see chat log), independent of
# the vote figures already in data/years.csv
vote_summary <- tribble(
  ~year, ~est_votes,               ~precision,                  ~source,                                          ~champion, ~runner_up,
  2014,  "1,700",                  "Stated, likely rounded",    "Katmai Conservancy (via Popular Science)",      "480",     "410",
  2015,  "Not found",              NA,                          NA,                                                "409",     "480",
  2016,  "Not found",              NA,                          NA,                                                "480",     "435",
  2017,  "Not found",              NA,                          NA,                                                "480",     "747",
  2018,  "Under ~62,000 (implied)","Inferred",                  "Derived from 2019 coverage",                     "409",     "747",
  2019,  "187,000",                "Rounded",                   "EcoWatch",                                        "435",     "775",
  2020,  "More than 640,000",      "Lower bound",                "Explore.org Hall of Champions",                  "747",     "32",
  2021,  "793,463",                "Exact",                     "NPS recap blog",                                  "480",     "151",
  2022,  "More than 1 million",    "Lower bound",                "Explore.org",                                     "747",     "901",
  2023,  "~1.3M or ~1.4M",         "Rounded, sources conflict", "Explore.org vs. NPS",                             "128",     "32",
  2024,  "~1.2M",                  "Rounded",                   "Explore.org",                                     "128",     "32",
  2025,  "1.5M, 1.6M, or 1.7M",    "Rounded, sources conflict", "Explore.org/NPS vs. Smithsonian vs. Wikipedia",  "32",      "856"
)

# single-point numeric estimate derived from est_votes: exact figures kept as-is,
# ranges/"more than" values collapsed to a midpoint or stated lower bound
vote_summary <- vote_summary |>
  mutate(total_votes = c(
    1700, NA, NA, NA,
    62000, 187000, 640000, 793463,
    1000000, 1350000, 1200000, 1600000
  ))

vote_summary <- vote_summary |>
  select(year, est_votes, total_votes, precision, source, champion, runner_up)

write_csv(vote_summary, "data/summary.csv")

print(vote_summary, n = 12)
