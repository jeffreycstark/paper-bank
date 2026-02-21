## ──────────────────────────────────────────────────────────────────────────────
## 00 — Data Preparation: The Satisfaction Paradox (South Korea, 2001–2019)
##
## Purpose: Load harmonized ABS data, subset to Korea (waves 1–5, 2001–2019),
##          construct analysis variables for the economic performance /
##          institutional trust / democratic satisfaction nexus, and save the
##          analysis dataset.
##
## Data sources:
##   ABS — Asian Barometer Survey (country_num == 3, Korea, waves 1–5)
##         Wave 1 ≈ 2001, Wave 2 ≈ 2003, Wave 3 ≈ 2006, Wave 4 ≈ 2011,
##         Wave 5 ≈ 2015–2016.  Wave 6 excluded (post-2019).
##   Comparators (optional): Taiwan (country_num == 7), Japan (country_num == 1)
##
## Input:   abs_harmonized_path  (via _data_config.R)
## Output:  analysis/kor_abs.rds          — Korea ABS panel (waves 1–5)
##          analysis/comp_abs.rds         — Comparator countries (optional)
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/south-korea-hollow-satisfaction")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

# ─── 1. ABS — Asian Barometer Survey ─────────────────────────────────────────

abs <- readRDS(abs_harmonized_path)
cat("ABS harmonized:", nrow(abs), "obs,", n_distinct(abs$country_num), "countries\n")

# Korea = country_num 3; restrict to waves 1–5 (2001–2019)
kor_abs <- abs |>
  filter(country_num == 3, wave %in% 1:5)

cat("Korea (ABS, waves 1–5): n =", nrow(kor_abs), "\n")
cat("Waves:", sort(unique(kor_abs$wave)), "\n\n")

# Optional comparators: Taiwan (7), Japan (1)
comp_abs <- abs |>
  filter(country_num %in% c(1, 7), wave %in% 1:5)

cat("Comparators (Japan + Taiwan, waves 1–5): n =", nrow(comp_abs), "\n\n")

# ─── 2. Construct core analysis variables ────────────────────────────────────
#
# Key variable domains (confirm exact names against abs_harmonized codebook):
#
#  Economic performance perceptions
#    - personal_econ_retro   : personal economic situation, retrospective
#    - national_econ_retro   : national economic situation, retrospective
#    - national_econ_prosp   : national economic situation, prospective
#
#  Institutional trust
#    - trust_president / trust_govt
#    - trust_legislature / trust_parl
#    - trust_courts / trust_judicial
#    - trust_parties
#    - trust_military
#    - trust_civil_service
#    - trust_media
#    - trust_elections / trust_ec
#
#  Democratic attitudes
#    - dem_satisfaction      : satisfaction with democracy (1–4, higher = more)
#    - dem_preference        : support for democracy as best system
#    - dem_performance       : perceived performance of democracy
#    - dem_suitability       : democracy suitable for country
#
#  Controls
#    - age, female, education, urban, income_quintile
#    - wave (time)
#
# NOTE: Variable name mapping TBD pending codebook review.
#       See survey-data-prep/codebooks/ for authoritative names.

# [VARIABLE CONSTRUCTION — fill in once codebook confirmed]
# kor_abs <- kor_abs |>
#   mutate(
#     econ_retro   = ...,
#     trust_index  = rowMeans(across(starts_with("trust_")), na.rm = TRUE),
#     dem_sat      = dem_satisfaction,
#     ...
#   )

# ─── 3. Save ─────────────────────────────────────────────────────────────────

saveRDS(kor_abs,  file.path(analysis_dir, "kor_abs.rds"))
cat("✓ Saved kor_abs.rds\n")

saveRDS(comp_abs, file.path(analysis_dir, "comp_abs.rds"))
cat("✓ Saved comp_abs.rds\n")
