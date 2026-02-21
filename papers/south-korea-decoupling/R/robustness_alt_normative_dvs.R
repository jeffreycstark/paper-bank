# =============================================================================
# Robustness: Alternative normative commitment DVs
#
# Reviewer concern: "the analysis depends on a single variable"
# (dem_always_preferable). This script runs the core econ → DV model
# on multiple alternative normative commitment measures to show the
# Korea null / Taiwan negative pattern is not item-specific.
#
# Candidate variables (from ABS harmonized):
#   1. dem_best_form        — "Democracy is the best form of government" (W3–6, 1–4)
#   2. dem_vs_econ          — Democracy vs economic development tradeoff (W1–6, 1–5)
#   3. democracy_suitability — "Democracy is suitable for our country" (W1–6, 1–10)
#   4. democracy_efficacy   — "Democracy can solve society's problems" (W1–6, binary)
#
# For each: wave-by-wave OLS + pooled with wave FE, both countries.
# Key comparison: econ_index coefficient in Korea vs Taiwan.
# =============================================================================

library(tidyverse)
library(broom)

# --- Setup ---
project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
paper_dir    <- file.path(project_root, "papers/south-korea-decoupling")
results_dir  <- file.path(paper_dir, "analysis/results")

source(file.path(project_root, "_data_config.R"))

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

controls <- "age_n + gender + edu_n + urban_rural + polint_n"

normalize_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

extract_econ_coef <- function(model) {
  tidy(model, conf.int = TRUE) |>
    filter(term == "econ_index") |>
    select(estimate, std.error, statistic, p.value, conf.low, conf.high)
}

# =============================================================================
# Step 0: Merge alternative normative DVs from harmonized ABS
# =============================================================================
alt_norm_vars <- c("dem_best_form", "dem_vs_econ", "democracy_suitability",
                   "democracy_efficacy")

missing_vars <- setdiff(alt_norm_vars, names(dat))

if (length(missing_vars) > 0) {
  cat("Merging alternative normative DVs from harmonized ABS...\n")
  cat("  Missing:", paste(missing_vars, collapse = ", "), "\n")

  abs_all <- readRDS(abs_harmonized_path)

  merge_vars <- abs_all |>
    filter(country %in% c(3, 7)) |>
    select(wave, country, row_id, any_of(alt_norm_vars))

  if ("row_id" %in% names(dat)) {
    dat <- dat |> left_join(merge_vars, by = c("wave", "country", "row_id"),
                            suffix = c("", ".new"))
    # If some vars already existed, drop .new duplicates
    dat <- dat |> select(-ends_with(".new"))
  } else if ("idnumber" %in% names(dat)) {
    merge_vars2 <- abs_all |>
      filter(country %in% c(3, 7)) |>
      select(wave, country, idnumber, any_of(alt_norm_vars))
    dat <- dat |> left_join(merge_vars2, by = c("wave", "country", "idnumber"),
                            suffix = c("", ".new"))
    dat <- dat |> select(-ends_with(".new"))
  }
  cat("  Done.\n")
} else {
  cat("All alternative normative DVs already present.\n")
}

# =============================================================================
# Step 1: Normalize alternative DVs
# =============================================================================

# dem_vs_econ: category 5 = "both equally important" — recode to NA for
# ordinal analysis (it's not on the dem-econ continuum), then normalize
# remaining 1–4 where higher = more pro-democracy
dat <- dat |>
  mutate(
    dem_vs_econ_ord = if_else(dem_vs_econ == 5, NA_real_, as.numeric(dem_vs_econ))
  )

dat <- dat |>
  group_by(country_label) |>
  mutate(
    norm_best_form     = normalize_01(dem_best_form),
    norm_dem_vs_econ   = normalize_01(dem_vs_econ_ord),
    norm_suitability   = normalize_01(democracy_suitability),
    norm_efficacy      = as.numeric(democracy_efficacy)  # already binary 0/1
  ) |>
  ungroup()

# =============================================================================
# Step 2: Coverage check
# =============================================================================
cat("\n=== Alternative normative DV coverage ===\n")
dat |>
  group_by(country_label, wave) |>
  summarise(
    n = n(),
    best_form_ok    = sum(!is.na(dem_best_form)),
    vs_econ_ok      = sum(!is.na(dem_vs_econ_ord)),
    suitability_ok  = sum(!is.na(democracy_suitability)),
    efficacy_ok     = sum(!is.na(democracy_efficacy)),
    # also the primary DV for comparison
    pref_dem_ok     = sum(!is.na(dem_always_preferable)),
    .groups = "drop"
  ) |>
  print(n = 20)

# =============================================================================
# Step 3: Wave-by-wave OLS — econ → each alternative normative DV
# =============================================================================
cat("\n=== Wave-by-wave results ===\n")

alt_dvs <- list(
  c("norm_best_form",   "Democracy best form (W3-6)"),
  c("norm_dem_vs_econ", "Democracy > econ dev (W1-6)"),
  c("norm_suitability", "Democracy suitable (W1-6)"),
  c("norm_efficacy",    "Democracy efficacious (W1-6)"),
  # Include primary DV for direct comparison
  c("qual_pref_dem_n",  "Dem always preferable (primary)")
)

wave_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  for (w in 1:6) {
    sub <- dat |> filter(country_label == cntry, wave == w)
    if (nrow(sub) < 100) next

    for (dv_info in alt_dvs) {
      dv_var   <- dv_info[1]
      dv_label <- dv_info[2]

      if (all(is.na(sub[[dv_var]]))) next
      if (sum(!is.na(sub[[dv_var]])) < 100) next

      f <- as.formula(paste(dv_var, "~ econ_index +", controls))
      m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
      if (is.null(m)) next

      wave_results[[paste(cntry, w, dv_var, sep = "_")]] <-
        extract_econ_coef(m) |>
        mutate(country = cntry, wave = w, dv = dv_label,
               dv_var = dv_var, n = nobs(m),
               r_sq = summary(m)$r.squared)
    }
  }
}

wave_df <- bind_rows(wave_results)

# Print Korea
cat("\n--- Korea: wave-by-wave econ → normative DVs ---\n")
wave_df |>
  filter(country == "Korea") |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(dv, wave, estimate, p.value, sig, n) |>
  arrange(dv, wave) |>
  print(n = 50)

# Print Taiwan
cat("\n--- Taiwan: wave-by-wave econ → normative DVs ---\n")
wave_df |>
  filter(country == "Taiwan") |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(dv, wave, estimate, p.value, sig, n) |>
  arrange(dv, wave) |>
  print(n = 50)

# =============================================================================
# Step 4: Pooled models with wave FE
# =============================================================================
cat("\n=== Pooled models (wave FE) ===\n")
pooled_results <- list()

for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry)

  for (dv_info in alt_dvs) {
    dv_var   <- dv_info[1]
    dv_label <- dv_info[2]

    sub_ok <- sub |> filter(!is.na(.data[[dv_var]]))
    if (nrow(sub_ok) < 200) next

    f <- as.formula(paste(dv_var, "~ econ_index + factor(wave) +", controls))
    m <- tryCatch(lm(f, data = sub_ok), error = function(e) NULL)
    if (is.null(m)) next

    pooled_results[[paste(cntry, dv_var, sep = "_")]] <-
      extract_econ_coef(m) |>
      mutate(country = cntry, dv = dv_label, dv_var = dv_var,
             n = nobs(m), r_sq = summary(m)$r.squared)
  }
}

pooled_df <- bind_rows(pooled_results)

pooled_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(country, dv, estimate, std.error, p.value, sig, n) |>
  arrange(dv, country) |>
  print(n = 20)

# =============================================================================
# Step 5: Cross-country interaction test for each alternative DV
# =============================================================================
cat("\n=== Cross-country interaction (econ × Korea) ===\n")
xc_results <- list()

for (dv_info in alt_dvs) {
  dv_var   <- dv_info[1]
  dv_label <- dv_info[2]

  both <- dat |>
    filter(!is.na(.data[[dv_var]])) |>
    mutate(is_korea = as.numeric(country_label == "Korea"))

  if (nrow(both) < 500) next

  f <- as.formula(paste(dv_var, "~ econ_index * is_korea + factor(wave) +", controls))
  m <- tryCatch(lm(f, data = both), error = function(e) NULL)
  if (is.null(m)) next

  xc_results[[dv_var]] <-
    tidy(m, conf.int = TRUE) |>
    filter(term %in% c("econ_index", "is_korea", "econ_index:is_korea")) |>
    mutate(dv = dv_label, dv_var = dv_var, n = nobs(m))
}

xc_df <- bind_rows(xc_results)

xc_df |>
  mutate(sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                         p.value < 0.05  ~ "*",   TRUE ~ "")) |>
  select(dv, term, estimate, std.error, p.value, sig, n) |>
  print(n = 30)

# =============================================================================
# Step 6: Summary table — pooled econ betas side by side
# =============================================================================
cat("\n=== SUMMARY: Pooled econ → normative DVs ===\n")
cat("  (Korea should be ~0 or weakly negative; Taiwan should be negative)\n\n")

pooled_df |>
  mutate(
    sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                    p.value < 0.05  ~ "*",   TRUE ~ ""),
    result = sprintf("%.3f%s", estimate, sig)
  ) |>
  select(dv, country, result, n) |>
  pivot_wider(names_from = country, values_from = c(result, n),
              names_glue = "{country}_{.value}") |>
  select(dv, Korea_result, Korea_n, Taiwan_result, Taiwan_n) |>
  print()

# =============================================================================
# Step 7: Save results for manuscript integration
# =============================================================================
alt_norm_results <- list(
  wave_by_wave   = wave_df,
  pooled         = pooled_df,
  cross_country  = xc_df
)

saveRDS(alt_norm_results, file.path(results_dir, "alt_normative_dvs.rds"))
cat("\n✓ Saved to", file.path(results_dir, "alt_normative_dvs.rds"), "\n")
