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
## Variable coverage notes (verified against harmonized data):
##   DROPPED — dem_country_present_govt (qual_current_govt): 54–72% missing in
##     ALL waves; non-random subsample, cannot use for trend analysis.
##   DROPPED — subjective_social_status (econ_status): 24–32% missing W4–W6,
##     increasing trend, unreliable for late-wave comparisons.
##   W1 only — most accountability vars not asked in W1; acc_courts also missing W2.
##   W1–W2 — system_needs_change / system_deserves_support not asked until W3.
##   W1, W3 — participation actions (petition, demonstration, contact) have
##     ~85–95% missing in W3 (2011); a genuine ABS harmonization gap.
##     Trend lines for these will show W2, W4–W6 (W3 missing).
##   W2 — efficacy_no_influence not asked; part_efficacy_ext missing W2 only.
##   W1, W6 — hh_income_sat not asked; econ_hh_sat available W2–W5 only.
##
## Input:   abs_harmonized_path   (via _data_config.R)
##          kamos_harmonized_path (via _data_config.R)
## Output:  analysis/kor_abs.rds     — Korea ABS (all waves, cluster vars)
##          analysis/kor_kamos.rds   — KAMOS W1 + W4
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
    # NOTE: dem_country_present_govt EXCLUDED — 54–72% missing in every wave.
    qual_extent           = normalize_01(dem_extent_current),      # 1–4; W2–W6 clean, W1 54% miss
    qual_pref_dem         = normalize_01(dem_always_preferable),   # 1–3; W2–W6 clean, W1 ok
    # system_needs_change: higher raw = more change needed → REVERSE
    # Both items only asked from W3 onward; W1–W2 will be NA (handled downstream)
    qual_no_change_needed = normalize_01(5 - system_needs_change), # 1–4; W3–W6 only
    qual_system_support   = normalize_01(system_deserves_support),  # 1–4; W3–W6 only

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
    # NOTE: subjective_social_status — 1–10 scale (97/98/99 → NA in harmonization).
    #   Absence of values 7–9 is distributional: Korean respondents cluster at
    #   the lower end of the status ladder with a small top-coded group at 10.
    #   Main concern: 24–32% missing W4–W6 (vs 0–10% W1–W3); missingness
    #   may be non-random. Use with caution in late-wave trend comparisons.
    # NOTE: econ_next_generation_life / econ_generation_opportunity EXCLUDED —
    #   100% missing W1–W3, only available W4–W6.
    econ_status        = normalize_01(subjective_social_status), # 1–10; see note above
    econ_national      = normalize_01(econ_national_now),   # 1–5; W1–W6 clean
    econ_hh_now        = normalize_01(econ_family_now),     # 1–5; W1–W6 clean (<1% miss)
    econ_hh_change     = normalize_01(econ_family_change),  # 1–5; W1–W6 clean (<1% miss)
    econ_hh_sat        = normalize_01(hh_income_sat),       # 1–4; W2–W5 only (W1, W6 miss)
    econ_outlook       = normalize_01(econ_outlook_1yr),    # 1–5; W1–W6 clean
    econ_hh_outlook    = normalize_01(econ_family_outlook), # 1–5; W1–W6 clean (<3% miss)

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
  "qual_extent", "qual_pref_dem",
  "qual_no_change_needed", "qual_system_support",
  "part_voted", "part_petition", "part_demonstration",
  "part_contact", "part_efficacy_int", "part_efficacy_ext",
  "econ_status", "econ_national", "econ_hh_now", "econ_hh_change",
  "econ_hh_sat", "econ_outlook", "econ_hh_outlook",
  "acc_transparency", "acc_responsive", "acc_elections",
  "acc_courts", "acc_free_speech",
  "trust_govt", "trust_parliament", "trust_courts",
  "trust_parties", "trust_military", "trust_press"
)
# Expected wave-level gaps (not flagged as errors):
expected_gaps <- tribble(
  ~variable,              ~waves_missing,   ~reason,
  "acc_transparency",     "W1",             "not asked W1",
  "acc_responsive",       "W1",             "not asked W1",
  "acc_elections",        "W1",             "not asked W1",
  "acc_free_speech",      "W1",             "not asked W1",
  "acc_courts",           "W1, W2",         "not asked W1–W2",
  "econ_hh_sat",          "W1, W6",         "not asked W1 or W6",
  "qual_no_change_needed","W1, W2",         "not asked until W3",
  "qual_system_support",  "W1, W2",         "not asked until W3",
  "qual_extent",          "W1 (~54%)",      "partial W1 coverage",
  "part_petition",        "W1, W3",         "ABS harmonization gap",
  "part_demonstration",   "W1, W3",         "ABS harmonization gap",
  "part_contact",         "W1, W3",         "ABS harmonization gap",
  "part_efficacy_ext",    "W2",             "not asked W2",
  "econ_status",          "W4–W6 (~25–32%)", "higher item non-response in later waves"
)
cat("\nExpected coverage gaps (by design):\n")
print(expected_gaps, n = Inf)

missing_check <- kor |>
  select(wave, all_of(analysis_vars)) |>
  group_by(wave) |>
  summarise(across(everything(), ~ mean(is.na(.)), .names = "na_{.col}")) |>
  pivot_longer(-wave, names_to = "variable", values_to = "pct_missing") |>
  mutate(variable = str_remove(variable, "^na_")) |>
  filter(pct_missing > 0.15) |>
  # Suppress documented expected gaps
  anti_join(
    bind_rows(
      # All acc_* vars not asked in W1; acc_elections also 15.6% missing W2
      expand_grid(variable = c("acc_transparency","acc_responsive",
                               "acc_elections","acc_free_speech"), wave = 1L),
      tibble(variable = "acc_elections", wave = 2L),  # 15.6% missing W2
      # acc_courts not asked W1–W2
      expand_grid(variable = "acc_courts", wave = c(1L, 2L)),
      # econ_hh_sat not asked W1 or W6
      expand_grid(variable = "econ_hh_sat", wave = c(1L, 6L)),
      # quality items not asked W1–W2
      expand_grid(variable = c("qual_no_change_needed","qual_system_support"),
                  wave = c(1L, 2L)),
      # qual_extent partial W1
      tibble(variable = "qual_extent", wave = 1L),
      # participation actions: W1 and W3
      expand_grid(variable = c("part_petition","part_demonstration","part_contact"),
                  wave = c(1L, 3L)),
      # part_contact also ~95% missing in W1
      tibble(variable = "part_contact", wave = 1L),
      # part_efficacy_ext not asked W2
      tibble(variable = "part_efficacy_ext", wave = 2L),
      # econ_status (subjective_social_status): 24–32% missing W4–W6; flagged in comments
      expand_grid(variable = "econ_status", wave = c(4L, 5L, 6L))
    ),
    by = c("variable", "wave")
  )

if (nrow(missing_check) > 0) {
  cat("\nWARNING: Unexpected missingness (>15%, not in documented gaps):\n")
  print(missing_check, n = Inf)
} else {
  cat("\n✓ All missingness accounted for — no unexpected gaps.\n")
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
