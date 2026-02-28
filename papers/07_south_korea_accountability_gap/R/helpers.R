## R/helpers.R
## South Korea Accountability Gap — Shared constants
## Sourced by: 01_analysis.qmd, 03_pub_figures_tables.qmd, R/analysis_additions.R

# ── Wave year lookup (Korea ABS) ──────────────────────────────────────────────
# W1: ~2003, W2: ~2006 (int_year not recorded); W3–W6 confirmed from int_year
wave_years <- c("1" = 2003, "2" = 2006, "3" = 2011,
                "4" = 2015, "5" = 2019, "6" = 2022)

# ── Colour palette — exploratory / analysis figures ───────────────────────────
# Used in 01_analysis.qmd and R/analysis_additions.R.
# 03_pub_figures_tables.qmd uses a separate greyscale-compatible palette for
# Brill print requirements (defined locally in that script).
col_pre      <- "#2166AC"   # deep blue — pre-shock / 2016
col_post     <- "#D6604D"   # muted red — post-shock / 2019
col_exec     <- "#4393C3"   # lighter blue — executive institutions
col_intermed <- "#D73027"   # red — intermediary/accountability institutions
col_rising   <- "#D73027"   # rising series
col_stable   <- "#878787"   # stable/flat series
col_shade    <- "#F0F0F0"   # background shading
