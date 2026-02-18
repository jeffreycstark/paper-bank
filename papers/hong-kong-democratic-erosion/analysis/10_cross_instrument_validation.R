## ──────────────────────────────────────────────────────────────────────────────
## 10 — Cross-Instrument Validation: ABS Trust vs. WVS Confidence
##
## Purpose: Exploit the temporal overlap between ABS Wave 5 (~2019) and
## WVS Wave 7 (~2018) in Thailand and the Philippines to validate that the
## trust/confidence wording distinction does not alter institutional
## rank-orderings or gradient inference.
##
## Tests:
##   (1) Mean-level comparison of overlapping institutions
##   (2) Rank-order (Spearman) correlation of institutional means
##   (3) Null gradient in non-autocratizing contexts (should be ~0)
##
## Data: ABS harmonized .rds + WVS harmonized .parquet from survey-data-prep
## Output: results/cross_instrument_validation.RData
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(arrow)

# ─── 1. Load data ────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))

abs_data <- readRDS(abs_harmonized_path)
wvs_data <- read_parquet(wvs_harmonized_path)

cat("ABS:", nrow(abs_data), "obs |",
    "WVS:", nrow(wvs_data), "obs\n")

# ─── 2. Institution crosswalk ────────────────────────────────────────────────
# Only institutions present in BOTH instruments.
# ABS: trust_national_government, trust_parliament, trust_police, trust_courts,
#      trust_military, trust_president
# WVS: trust_government, trust_parliament, trust_police, trust_courts,
#      trust_armed_forces, trust_political_parties, trust_press

institution_crosswalk <- tribble(
  ~institution,    ~abs_var,                    ~wvs_var,              ~sensitivity_rank,
  "Police",        "trust_police",              "trust_police",        1,
  "Military",      "trust_military",            "trust_armed_forces",  2,
  "Government",    "trust_national_government", "trust_government",    3,
  "Courts",        "trust_courts",              "trust_courts",        4,
  "Parliament",    "trust_parliament",          "trust_parliament",    5
)
# sensitivity_rank: 1 = most coercive, higher = less coercive
# Matches the gradient logic from scripts 08/09

# ─── 3. Country selection ────────────────────────────────────────────────────
# Thailand: ABS W5 = 2019, WVS W7 = 2018 (~1 year gap)
# Philippines: ABS W5 = 2019, WVS W7 = 2019 (~0 gap)

countries <- tribble(
  ~country_name,   ~abs_code, ~wvs_code, ~abs_wave, ~wvs_wave,
  "Thailand",      8,         "THA",     5,         7,
  "Philippines",   6,         "PHL",     5,         7
)

# ─── 4. Compute institutional means for each country × instrument ────────────

compute_means <- function(country_name, abs_code, wvs_code,
                          abs_wave, wvs_wave) {

  # ABS subset
  abs_sub <- abs_data |>
    filter(country == abs_code, wave == abs_wave)

  # WVS subset
  wvs_sub <- wvs_data |>
    filter(country == wvs_code, wave == wvs_wave)

  cat(sprintf("\n%s — ABS W%d: n=%d | WVS W%d: n=%d\n",
              country_name, abs_wave, nrow(abs_sub),
              wvs_wave, nrow(wvs_sub)))

  # Weighted mean helper
  wmean <- function(x, w) {
    valid <- !is.na(x) & !is.na(w)
    if (sum(valid) < 10) return(NA_real_)
    sum(x[valid] * w[valid]) / sum(w[valid])
  }

  # Compute means for each institution in each instrument
  results <- institution_crosswalk |>
    rowwise() |>
    mutate(
      country = country_name,
      abs_n = sum(!is.na(abs_sub[[abs_var]]), na.rm = TRUE),
      abs_mean = wmean(abs_sub[[abs_var]], abs_sub$weight),
      wvs_n = sum(!is.na(wvs_sub[[wvs_var]]), na.rm = TRUE),
      wvs_mean = wmean(wvs_sub[[wvs_var]], wvs_sub$weight)
    ) |>
    ungroup() |>
    mutate(
      abs_rank = rank(-abs_mean),   # rank by descending mean (1 = highest)
      wvs_rank = rank(-wvs_mean),
      mean_diff = wvs_mean - abs_mean
    )

  results
}

all_means <- pmap_dfr(countries, compute_means)

# ─── 5. Rank-order correlation ──────────────────────────────────────────────

cat("\n═══ RANK-ORDER CORRELATION (ABS vs. WVS) ═══\n")

rank_cors <- all_means |>
  group_by(country) |>
  summarise(
    spearman_r = cor(abs_mean, wvs_mean, method = "spearman"),
    pearson_r = cor(abs_mean, wvs_mean, method = "pearson"),
    spearman_rank_r = cor(abs_rank, wvs_rank, method = "spearman"),
    .groups = "drop"
  )

for (i in seq_len(nrow(rank_cors))) {
  rc <- rank_cors[i, ]
  cat(sprintf("\n%s:\n  Pearson r (means): %.3f\n  Spearman rho (means): %.3f\n  Spearman rho (ranks): %.3f\n",
              rc$country, rc$pearson_r, rc$spearman_r, rc$spearman_rank_r))
}

# ─── 6. Mean-level comparison table ─────────────────────────────────────────

cat("\n═══ MEAN-LEVEL COMPARISON ═══\n")

all_means |>
  select(country, institution, abs_mean, wvs_mean, abs_rank, wvs_rank, mean_diff) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = 20)

# ─── 7. Null gradient: sensitivity rank vs. mean trust ──────────────────────
# In non-autocratizing contexts, the correlation between coercive salience
# (sensitivity_rank) and institutional trust should be near zero.
# A strong negative correlation would suggest coercive institutions are
# systematically more trusted—the signature we expect ONLY under falsification.

cat("\n═══ NULL GRADIENT (sensitivity rank vs. mean trust) ═══\n")

null_gradients <- all_means |>
  group_by(country) |>
  summarise(
    # ABS instrument
    abs_gradient_r = cor(sensitivity_rank, abs_mean, method = "pearson"),
    abs_gradient_rho = cor(sensitivity_rank, abs_mean, method = "spearman"),
    abs_gradient_p = cor.test(sensitivity_rank, abs_mean,
                              method = "pearson")$p.value,
    # WVS instrument
    wvs_gradient_r = cor(sensitivity_rank, wvs_mean, method = "pearson"),
    wvs_gradient_rho = cor(sensitivity_rank, wvs_mean, method = "spearman"),
    wvs_gradient_p = cor.test(sensitivity_rank, wvs_mean,
                              method = "pearson")$p.value,
    .groups = "drop"
  )

for (i in seq_len(nrow(null_gradients))) {
  ng <- null_gradients[i, ]
  cat(sprintf("\n%s:\n  ABS null gradient:  r = %.3f (rho = %.3f, p = %.3f)\n  WVS null gradient:  r = %.3f (rho = %.3f, p = %.3f)\n",
              ng$country,
              ng$abs_gradient_r, ng$abs_gradient_rho, ng$abs_gradient_p,
              ng$wvs_gradient_r, ng$wvs_gradient_rho, ng$wvs_gradient_p))
}

# ─── 8. Save results ────────────────────────────────────────────────────────

cross_instrument_results <- list(
  means = all_means,
  rank_correlations = rank_cors,
  null_gradients = null_gradients,
  crosswalk = institution_crosswalk,
  countries = countries
)

save(cross_instrument_results,
     file = file.path("results", "cross_instrument_validation.RData"))

cat("\n✓ Results saved to results/cross_instrument_validation.RData\n")
