# Fat Bear Week curated tables

Hand-compiled from NPS, Explore.org and news sources (see `sources.csv`). `build.py` regenerates the CSVs.
Every row carries source keys; nothing is estimated beyond what a source states, except where marked.

## Files
- `years.csv`: one row per tournament, 2014–2026. Totals and final-round votes; `*_approx` = source gave a rounded or "more than" figure.
- `bears.csv`: one row per bear (40). `birth_year_est` is derived from the age/ID-year stated in bios; `birth_year_basis` says how.
- `appearances.csv`: bear × year. Full rosters for 2021–2026; for 2014–2020 only finalists (plus 480 in 2018) are listed.
- `sources.csv`: source key → URL.

## Known gaps and flags
- **Rosters 2014–2020**: not compiled. 2020's bracket exists only as an image on the NPS release; earlier years need Wayback snapshots. Competitor counts for these years are NA.
- **Total votes conflict**: 2023 (~1.3M Explore vs "nearly 1.4M" NPS) and 2025 (1.7M / 1.6M / 1.5M depending on source). Values stored are Explore's; conflicts noted.
- **Final-round votes** are only available for 2019, 2021, 2024 (approx), 2025. 2022 has a semifinal count (747 37,940 vs 435 30,430) not stored here; it belongs in a `matches` table.
- **Bear 609 = 909 Jr.?** Both are described as the 2022 Fat Bear Junior champion; I treated them as one bear (`609`). Verify.
- **128 Grazer's age**: 2024 bio says she was a cub in 2005; the 2022 bio says first identified 2009 at ~17–19. Birth year stored as 2004 with the conflict noted.
- **Sex unknown** for 99, 775 Lefty and 132's 2021 cub in the sources used.
- **Lineage** is recorded for 14 bears, mostly the 409 → 909/910 line, the 435 line (89, 335, 719 → 519, 26?), the 128 line (428, 903, 128 Jr.) and 402 (503, 812?). `mother_basis` distinguishes stated / believed / suspected / inferred.
- Pre-2021 `result` values other than champion/runner-up are unknown.
