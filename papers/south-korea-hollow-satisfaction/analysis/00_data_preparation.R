## ──────────────────────────────────────────────────────────────────────────────
## 00 — Data Preparation: The Satisfaction Paradox (South Korea)
##
## Purpose: Load harmonized ABS and KAMOS data, subset to Korea,
##          construct variable clusters, normalize scales, and save
##          analysis datasets for downstream QMD files.
##
## Data sources:
##   ABS   — Asian Barometer Survey, Korea (country == 3), waves 1–6
##   KAMOS — Korean Academic Multimode Open Survey, waves 1 and 4 (2016, 2019)
##
## Wave year lookup (Korea ABS):
##   W1 ≈ 2003 | W2 ≈ 2006 | W3 ≈ 2011 | W4 ≈ 2015 | W5 ≈ 2019 | W6 ≈ 2022
##   Note: int_year not recorded in W1–W2; approximate years used.
##
## Input:   abs_harmonized_path   (via _data_config.R)
##          kamos_harmonized_path (via _data_config.R)
## Output:  analysis/kor_abs.rds     — Korea ABS (all waves, cluster vars)
##          analysis/kor_kamos.rds   — KAMOS W1 + W4 with institution long format
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/south-korea-hollow-satisfaction")
analysis_dir <- file.path(paper_dir, "analysis")

wave_years <- c("1" = 2003, "2" = 2006, "3" = 2011,
                "4" = 2015, "5" = 2019, "6" = 2022)

# ─── 1. ABS — load and filter to Korea ───────────────────────────────────────

abs_raw <- readRDS(abs_harmonized_path)
cat("ABS harmonized:", nrow(abs_raw), "obs,", n_distinct(abs_raw$country), "countries\n")

kor <- abs_raw |>
  filter(country == 3) |>
  mutate(
    survey_year = wave_years[as.character(wave)],
    wave        = as.integer(wave)
  )

cat("Korea (ABS): n =", nrow(kor), "| waves:", paste(sort(unique(kor$wave)), collapse = " "), "\n")

# ─── 2. ABS — select and label variable clusters ──────────────────────────────
#
# Scale conventions (confirmed from ABS harmonized codebook):
#   Trust / agreement items : 1–4  (higher = more trust / agreement)
#   Economic items          : 1–5  (higher = better conditions / more optimistic)
#   Participation items     : 1–5  (higher = more participation)
#   Binary items            : 0/1  (1 = yes / voted / witnessed)
#
# All variables normalized to [0, 1] range for slope comparability.
# Variables marked REVERSE need to be inverted so higher = better/more democratic.

normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(x)
  (x - rng[1]) / diff(rng)
}

kor <- kor |>
  mutate(

    # ── Cluster 1: Democratic satisfaction ──────────────────────────────────
    # Higher = more satisfied
    sat_democracy      = normalize_01(democracy_satisfaction),   # 1–4 W1–W6
    sat_govt           = normalize_01(gov_sat_national),         # 1–4 W1–W6

    # ── Cluster 2: Democratic quality perceptions ────────────────────────────
    # Higher = perceives more/better democracy
    qual_extent        = normalize_01(dem_extent_current),       # 1–4 W1–W6
    qual_current_govt  = normalize_01(dem_country_present_govt), # 1–4 W1–W6
    qual_pref_dem      = normalize_01(dem_always_preferable),    # 1–3 W1–W6
    # system_needs_change: higher raw = more change needed → REVERSE
    qual_no_change_needed = normalize_01(5 - system_needs_change), # 1–4 W1–W6
    qual_system_support   = normalize_01(system_deserves_support),  # 1–4 W1–W6

    # ── Cluster 3: Political participation ──────────────────────────────────
    # Higher = more participation / efficacy
    part_voted         = normalize_01(voted_last_election),      # binary W1–W6
    part_petition      = normalize_01(action_petition),          # 1–5 W1–W6
    part_demonstration = normalize_01(action_demonstration),     # 1–5 W1–W6
    part_contact       = normalize_01(action_contact_elected),   # 1–5 W1–W6
    part_efficacy_int  = normalize_01(efficacy_ability_participate), # 1–4 W1–W6
    # efficacy_no_influence: higher raw = LESS efficacy → REVERSE
    part_efficacy_ext  = normalize_01(5 - efficacy_no_influence),    # 1–4 W1–W6

    # ── Cluster 4: Economic evaluations ─────────────────────────────────────
    # Higher = better conditions / more satisfied / more optimistic
    econ_national      = normalize_01(econ_national_now),        # 1–5 W1–W6
    econ_hh_sat        = normalize_01(hh_income_sat),            # 1–4 W1–W6
    econ_outlook       = normalize_01(econ_outlook_1yr),         # 1–5 W1–W6
    econ_status        = normalize_01(subjective_social_status), # continuous W1–W6

    # ── Cluster 5: Accountability and responsiveness ─────────────────────────
    # Higher = more accountable / transparent / responsive
    # govt_withholds_info: higher raw = MORE withholding → REVERSE
    acc_transparency   = normalize_01(5 - govt_withholds_info),  # 1–4 W2–W6
    acc_responsive     = normalize_01(govt_responds_people),     # 1–4 W1–W6
    acc_elections      = normalize_01(election_free_fair),       # 1–4 W1–W6
    # gov_courts_powerless: higher raw = MORE powerless → REVERSE
    acc_courts         = normalize_01(5 - gov_courts_powerless), # 1–4 W1–W6
    acc_free_speech    = normalize_01(dem_free_speech),          # 1–4 W1–W6

    # ── Institutional trust (secondary) ─────────────────────────────────────
    trust_govt         = normalize_01(trust_national_government), # 1–4 W1–W6
    trust_parliament   = normalize_01(trust_parliament),          # 1–4 W1–W6
    trust_courts       = normalize_01(trust_courts),              # 1–4 W1–W6
    trust_parties      = normalize_01(trust_political_parties),   # 1–4 W1–W6
    trust_military     = normalize_01(trust_military),            # 1–4 W1–W6
    trust_press        = normalize_01(trust_newspapers)           # 1–4 W1–W6
  )

# ─── 3. ABS — diagnostic check ───────────────────────────────────────────────

analysis_vars <- c(
  "sat_democracy", "sat_govt",
  "qual_extent", "qual_current_govt", "qual_pref_dem",
  "qual_no_change_needed", "qual_system_support",
  "part_voted", "part_petition", "part_demonstration",
  "part_contact", "part_efficacy_int", "part_efficacy_ext",
  "econ_national", "econ_hh_sat", "econ_outlook", "econ_status",
  "acc_transparency", "acc_responsive", "acc_elections",
  "acc_courts", "acc_free_speech",
  "trust_govt", "trust_parliament", "trust_courts",
  "trust_parties", "trust_military", "trust_press"
)

missing_check <- kor |>
  select(wave, all_of(analysis_vars)) |>
  group_by(wave) |>
  summarise(across(everything(), ~ mean(is.na(.)), .names = "na_{.col}")) |>
  pivot_longer(-wave, names_to = "variable", values_to = "pct_missing") |>
  filter(pct_missing > 0.15)  # flag >15% missing as potential harmonization issue

if (nrow(missing_check) > 0) {
  cat("\nWARNING: Variables with >15% missing by wave:\n")
  print(missing_check)
} else {
  cat("\n✓ All analysis variables within acceptable missingness thresholds.\n")
}

# ─── 4. Save ABS ─────────────────────────────────────────────────────────────

saveRDS(kor, file.path(analysis_dir, "kor_abs.rds"))
cat("✓ Saved kor_abs.rds (n =", nrow(kor), ")\n")

# ─── 5. KAMOS — load and prepare ─────────────────────────────────────────────

kamos_raw <- readRDS(kamos_harmonized_path)
cat("\nKAMOS harmonized: n =", nrow(kamos_raw), "| waves:", paste(sort(unique(kamos_raw$wave)), collapse = " "), "\n")

kamos <- kamos_raw |>
  # Combine the two overlapping legislative trust items
  mutate(
    trust_legislative = (trust_national_assembly + trust_legislature) / 2,
    wave_label        = if_else(wave == 1, "Wave 1 (2016)", "Wave 4 (2019)")
  )

cat("KAMOS National Assembly × Legislature correlation:",
    round(cor(kamos$trust_national_assembly, kamos$trust_legislature,
              use = "complete.obs"), 2), "\n")

# ─── 6. Save KAMOS ───────────────────────────────────────────────────────────

saveRDS(kamos, file.path(analysis_dir, "kor_kamos.rds"))
cat("✓ Saved kor_kamos.rds (n =", nrow(kamos), ")\n")

cat("\n── Data preparation complete ────────────────────────────────────────────\n")
cat("Run 01_abs_trends.qmd and 02_kamos_critical_juncture.qmd for analysis.\n")
