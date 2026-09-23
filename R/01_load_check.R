library(tidyverse)

# load curated data with explicit column types
years <- read_csv("data/years.csv", col_types = cols(
  year = col_integer(),
  start_date = col_date(),
  end_date = col_date(),
  n_competitors = col_integer(),
  roster_complete = col_logical(),
  total_votes = col_integer(),
  total_votes_approx = col_logical(),
  champion_id = col_character(),
  runner_up_id = col_character(),
  champion_final_votes = col_integer(),
  runner_up_final_votes = col_integer(),
  final_votes_approx = col_logical(),
  format_notes = col_character(),
  sources = col_character(),
  notes = col_character()
))

bears <- read_csv("data/bears.csv", col_types = cols(
  bear_id = col_character(),
  name = col_character(),
  sex = col_character(),
  birth_year_est = col_integer(),
  birth_year_basis = col_character(),
  first_year_identified = col_integer(),
  mother_id = col_character(),
  mother_basis = col_character(),
  aliases = col_character(),
  sources = col_character(),
  notes = col_character()
))

appearances <- read_csv("data/appearances.csv", col_types = cols(
  year = col_integer(),
  bear_id = col_character(),
  entry_type = col_character(),
  n_cubs_in_entry = col_integer(),
  result = col_character(),
  roster_complete_for_year = col_logical(),
  source_keys = col_character(),
  notes = col_character()
))

sources <- read_csv("data/sources.csv", col_types = cols(
  source_key = col_character(),
  url = col_character(),
  description = col_character()
))

cat("Rows loaded: years =", nrow(years), " bears =", nrow(bears),
    " appearances =", nrow(appearances), " sources =", nrow(sources), "\n\n")

# --- check 1: every appearances$bear_id exists in bears$bear_id ---
unknown_bear_ids <- appearances |>
  filter(!bear_id %in% bears$bear_id) |>
  distinct(bear_id)

cat("Check 1: appearance bear_ids not found in bears.csv\n")
print(unknown_bear_ids)
cat("\n")

# --- check 2: every mother_id exists in bears$bear_id or is NA ---
unknown_mother_ids <- bears |>
  filter(!is.na(mother_id)) |>
  filter(!mother_id %in% bears$bear_id) |>
  select(bear_id, name, mother_id, mother_basis)

cat("Check 2: mother_ids not found in bears.csv\n")
print(unknown_mother_ids)
cat("\n")

# --- check 3: competitor counts match n_competitors for years with a complete roster ---
appearance_counts <- appearances |>
  count(year, name = "n_appearances")

count_check <- years |>
  filter(roster_complete == TRUE) |>
  select(year, n_competitors) |>
  left_join(appearance_counts, by = "year") |>
  mutate(mismatch = n_competitors != n_appearances)

cat("Check 3: n_competitors vs actual appearance rows (roster_complete years only)\n")
print(count_check)
cat("\n")

# --- check 4: champion_id / runner_up_id exist in bears.csv ---
unknown_result_ids <- years |>
  select(year, champion_id, runner_up_id) |>
  pivot_longer(cols = c(champion_id, runner_up_id), names_to = "role", values_to = "bear_id") |>
  filter(!is.na(bear_id)) |>
  filter(!bear_id %in% bears$bear_id)

cat("Check 4: champion/runner_up ids not found in bears.csv\n")
print(unknown_result_ids)
cat("\n")

# --- check 5: source keys referenced in sources/source_keys columns exist in sources.csv ---
known_sources <- sources$source_key

check_source_keys <- function(df, col, label) {
  keys <- df |>
    filter(!is.na(.data[[col]])) |>
    pull(.data[[col]]) |>
    str_split(";") |>
    unlist() |>
    unique()
  missing <- setdiff(keys, known_sources)
  cat("Check 5 (", label, "): source keys not found in sources.csv\n", sep = "")
  print(missing)
  cat("\n")
}

check_source_keys(years, "sources", "years.csv")
check_source_keys(bears, "sources", "bears.csv")
check_source_keys(appearances, "source_keys", "appearances.csv")

# --- summary: NA counts per column ---
cat("NA counts per column\n")
cat("-- years --\n")
print(colSums(is.na(years)))
cat("-- bears --\n")
print(colSums(is.na(bears)))
cat("-- appearances --\n")
print(colSums(is.na(appearances)))
