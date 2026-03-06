# 09_abstract_pref_investigation.R
# What drives abstract democratic preference? Cohort vs period effects,
# predictor profiles, and wave FE magnitude.
# Tasks A-F from CC task specification.

library(tidyverse)
library(sandwich)
library(lmtest)
library(broom)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))
paper_dir    <- file.path(project_root, "papers/03b_sk_satisfaction_paradox")
results_dir  <- file.path(paper_dir, "analysis/results")
source(file.path(paper_dir, "R", "helpers.R"))

# ── Load data ────────────────────────────────────────────────────────────────
dat <- readRDS(file.path(results_dir, "analysis_data.rds"))
abs_all <- readRDS(abs_harmonized_path)

# We need additional variables not in analysis_data.rds
# Merge by row position (same filter, same order)
abs_sub <- abs_all |>
  filter(country %in% c(3, 7))
stopifnot(nrow(abs_sub) == nrow(dat))

# Add variables for extended predictor model
dat$trust_generalized   <- abs_sub$trust_generalized_ordinal
dat$trust_gen_binary     <- abs_sub$trust_generalized_binary
dat$efficacy_participate <- abs_sub$efficacy_ability_participate
dat$efficacy_no_influence <- abs_sub$efficacy_no_influence
dat$efficacy_complicated <- abs_sub$efficacy_politics_complicated
dat$pol_discuss_raw      <- abs_sub$pol_discuss
dat$pol_news_follow_raw  <- abs_sub$pol_news_follow
dat$nat_proud_raw        <- abs_sub$nat_proud_citizen
dat$has_party_id         <- abs_sub$has_party_id
dat$sat_president        <- abs_sub$sat_president_govt
dat$dem_free_speech       <- abs_sub$dem_free_speech
dat$system_proud_raw     <- abs_sub$system_proud
dat$system_prefer_raw    <- abs_sub$system_prefer
dat$system_capable_raw   <- abs_sub$system_capable
dat$system_deserves_raw  <- abs_sub$system_deserves_support
dat$voted_last           <- abs_sub$voted_last_election
dat$dem_vs_econ          <- abs_sub$dem_vs_econ
dat$dem_suitability      <- abs_sub$democracy_suitability
dat$dem_best_form        <- abs_sub$dem_best_form

# Derive birth year (same logic as 07_r2_1)
yr_kr <- c("1"=2003,"2"=2006,"3"=2011,"4"=2015,"5"=2019,"6"=2022)
yr_tw <- c("1"=2001,"2"=2006,"3"=2010,"4"=2014,"5"=2019,"6"=2022)

dat <- dat |>
  mutate(
    survey_year = case_when(
      !is.na(int_year) ~ as.integer(int_year),
      country_label == "Korea"  ~ as.integer(yr_kr[as.character(wave)]),
      country_label == "Taiwan" ~ as.integer(yr_tw[as.character(wave)])
    ),
    birth_year = survey_year - age
  )

# Normalize additional variables within country
dat <- dat |>
  group_by(country_label) |>
  mutate(
    trust_gen_n       = normalize_01(trust_generalized),
    efficacy_part_n   = normalize_01(efficacy_participate),
    efficacy_noinf_n  = normalize_01(efficacy_no_influence),
    efficacy_comp_n   = normalize_01(efficacy_complicated),
    pol_discuss_n     = normalize_01(pol_discuss_raw),
    pol_news_n        = normalize_01(pol_news_follow_raw),
    nat_proud_n2      = normalize_01(nat_proud_raw),
    sat_president_n   = normalize_01(sat_president),
    dem_free_speech_n = normalize_01(dem_free_speech),
    dem_vs_econ_n     = normalize_01(dem_vs_econ),
    dem_suitability_n = normalize_01(dem_suitability),
    dem_best_form_n   = normalize_01(dem_best_form),
    voted_last_n      = normalize_01(voted_last)
  ) |>
  ungroup()

# Wave factor
dat$wave_f <- factor(dat$wave)

# ── Convenience: Korea and Taiwan subsets ─────────────────────────────────────
kr <- dat |> filter(country_label == "Korea")
tw <- dat |> filter(country_label == "Taiwan")

results <- list()

# ═══════════════════════════════════════════════════════════════════════════════
# TASK A: Age group means on "always preferable" by wave (Korea)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════\n")
cat("TASK A: Age group × wave — % 'always preferable' (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

kr <- kr |>
  mutate(
    age_group4 = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 59 ~ "45-59",
      age >= 60             ~ "60+"
    ),
    age_group4 = factor(age_group4, levels = c("18-29","30-44","45-59","60+"))
  )

task_a <- kr |>
  filter(!is.na(dem_always_preferable), !is.na(age_group4)) |>
  group_by(wave, survey_year, age_group4) |>
  summarise(
    n = n(),
    pct_always = mean(dem_always_preferable == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Also compute overall
task_a_overall <- kr |>
  filter(!is.na(dem_always_preferable)) |>
  group_by(wave, survey_year) |>
  summarise(
    n = n(),
    pct_always = mean(dem_always_preferable == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(age_group4 = "Overall")

task_a_full <- bind_rows(task_a, task_a_overall) |>
  arrange(wave, age_group4)

# Pivot for display
task_a_wide <- task_a_full |>
  select(wave, survey_year, age_group4, pct_always) |>
  pivot_wider(names_from = age_group4, values_from = pct_always) |>
  arrange(wave)

cat("% choosing 'democracy is always preferable' by age group and wave:\n\n")
print(task_a_wide, width = 120)

# Age group composition over time
cat("\n\nAge group composition (% of sample) by wave:\n")
age_comp <- kr |>
  filter(!is.na(age_group4)) |>
  group_by(wave) |>
  mutate(wave_n = n()) |>
  group_by(wave, age_group4) |>
  summarise(pct = n() / first(wave_n) * 100, .groups = "drop") |>
  pivot_wider(names_from = age_group4, values_from = pct)
print(age_comp, width = 120)

results$task_a_means <- task_a_wide
results$task_a_composition <- age_comp

# ═══════════════════════════════════════════════════════════════════════════════
# TASK B: Birth cohort tracking (Korea)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("TASK B: Birth cohort tracking — % 'always preferable' (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

kr <- kr |>
  mutate(
    birth_cohort = case_when(
      birth_year <= 1955              ~ "<=1955",
      birth_year >= 1956 & birth_year <= 1965 ~ "1956-1965",
      birth_year >= 1966 & birth_year <= 1975 ~ "1966-1975",
      birth_year >= 1976 & birth_year <= 1985 ~ "1976-1985",
      birth_year >= 1986              ~ "1986+"
    ),
    birth_cohort = factor(birth_cohort,
                          levels = c("<=1955","1956-1965","1966-1975","1976-1985","1986+"))
  )

task_b <- kr |>
  filter(!is.na(dem_always_preferable), !is.na(birth_cohort)) |>
  group_by(birth_cohort, wave, survey_year) |>
  summarise(
    n = n(),
    pct_always = mean(dem_always_preferable == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Pivot: birth cohort rows, wave columns
task_b_wide <- task_b |>
  mutate(wave_label = paste0("W", wave, " (", survey_year, ")")) |>
  select(birth_cohort, wave_label, pct_always) |>
  pivot_wider(names_from = wave_label, values_from = pct_always) |>
  arrange(birth_cohort)

cat("% 'always preferable' by birth cohort across waves:\n\n")
print(task_b_wide, width = 150)

# Also show n per cohort-wave
task_b_n <- task_b |>
  mutate(wave_label = paste0("W", wave, " (", survey_year, ")")) |>
  select(birth_cohort, wave_label, n) |>
  pivot_wider(names_from = wave_label, values_from = n) |>
  arrange(birth_cohort)

cat("\nN per birth cohort × wave:\n\n")
print(task_b_n, width = 150)

results$task_b_means <- task_b_wide
results$task_b_n <- task_b_n
results$task_b_detail <- task_b

# ═══════════════════════════════════════════════════════════════════════════════
# TASK C: Full predictor profile for dem_always_pref (Korea, pooled with wave FE)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("TASK C: Full predictor profile — standard model (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

fml_c <- qual_pref_dem_n ~ econ_index + age_n + gender + edu_n +
  urban_rural + polint_n + wave_f

m_c <- lm(fml_c, data = kr)
task_c <- tidy_hc2(m_c) |>
  mutate(sig = sig_stars(p.value))

cat("DV: qual_pref_dem_n (normalized; higher = LESS preferable)\n")
cat("N =", nobs(m_c), ", R2 =", round(summary(m_c)$r.squared, 4), "\n\n")
print(task_c |> select(term, estimate, std.error, p.value, sig), n = 20)

results$task_c <- task_c
results$task_c_r2 <- summary(m_c)$r.squared
results$task_c_n <- nobs(m_c)

# ═══════════════════════════════════════════════════════════════════════════════
# TASK D: Extended predictor model (Korea)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("TASK D: Extended predictor model (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

# Check coverage of extended vars
cat("Variable coverage (non-NA counts, Korea):\n")
extended_vars <- c("trust_index", "trust_gen_n", "efficacy_part_n",
                   "efficacy_noinf_n", "efficacy_comp_n",
                   "pol_discuss_n", "pol_news_n", "nat_proud_n2",
                   "sat_president_n", "dem_free_speech_n",
                   "dem_vs_econ_n", "dem_suitability_n", "dem_best_form_n",
                   "china_harm_n", "has_party_id", "voted_last_n")

coverage <- kr |>
  summarise(across(all_of(extended_vars), ~sum(!is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_nonmissing") |>
  mutate(pct = round(n_nonmissing / nrow(kr) * 100, 1))
print(coverage, n = 20)

# Model D: include everything with reasonable coverage (>30%)
# Start with the broadest model, drop vars with <30% coverage
usable_vars <- coverage |> filter(pct > 30) |> pull(variable)
cat("\nUsable variables (>30% coverage):", paste(usable_vars, collapse = ", "), "\n")

# Build formula
fml_d_str <- paste("qual_pref_dem_n ~ econ_index + age_n + gender + edu_n + urban_rural + polint_n +",
                   paste(usable_vars, collapse = " + "),
                   "+ wave_f")
fml_d <- as.formula(fml_d_str)

m_d <- lm(fml_d, data = kr)
task_d <- tidy_hc2(m_d) |>
  mutate(sig = sig_stars(p.value))

cat("\nDV: qual_pref_dem_n (higher = LESS preferable)\n")
cat("N =", nobs(m_d), ", R2 =", round(summary(m_d)$r.squared, 4), "\n\n")
print(task_d |> select(term, estimate, std.error, p.value, sig), n = 30)

results$task_d_korea <- task_d
results$task_d_korea_r2 <- summary(m_d)$r.squared
results$task_d_korea_n <- nobs(m_d)
results$task_d_formula <- fml_d_str

# ═══════════════════════════════════════════════════════════════════════════════
# TASK E: Wave fixed effects magnitude
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("TASK E: Wave FE magnitude (Korea)\n")
cat("══════════════════════════════════════════════════\n\n")

# Compare wave FEs across models with increasing controls
# Model E1: wave FE only (unconditional trend)
m_e1 <- lm(qual_pref_dem_n ~ wave_f, data = kr)
# Model E2: wave FE + demographics
m_e2 <- lm(qual_pref_dem_n ~ wave_f + age_n + gender + edu_n + urban_rural, data = kr)
# Model E3: wave FE + demographics + polint + econ
m_e3 <- lm(qual_pref_dem_n ~ wave_f + age_n + gender + edu_n + urban_rural + polint_n + econ_index, data = kr)
# Model E4: wave FE + all extended predictors (same as Task D)
m_e4 <- m_d

# Extract wave FE coefficients from each
extract_wave_fes <- function(model, label) {
  tidy_hc2(model) |>
    filter(str_detect(term, "^wave_f")) |>
    mutate(model = label) |>
    select(model, term, estimate, std.error, p.value)
}

task_e <- bind_rows(
  extract_wave_fes(m_e1, "Wave FE only"),
  extract_wave_fes(m_e2, "Wave FE + demographics"),
  extract_wave_fes(m_e3, "Wave FE + demo + polint + econ"),
  extract_wave_fes(m_e4, "Wave FE + all predictors")
)

cat("Wave FE coefficients (W1 = reference) across model specifications:\n\n")

task_e_wide <- task_e |>
  mutate(coef_str = sprintf("%.3f (p=%.3f)", estimate, p.value)) |>
  select(term, model, coef_str) |>
  pivot_wider(names_from = model, values_from = coef_str)
print(task_e_wide, width = 150)

cat("\nR-squared progression:\n")
cat("  Wave FE only:                 R2 =", round(summary(m_e1)$r.squared, 4), "\n")
cat("  + demographics:               R2 =", round(summary(m_e2)$r.squared, 4), "\n")
cat("  + demo + polint + econ:       R2 =", round(summary(m_e3)$r.squared, 4), "\n")
cat("  + all extended predictors:    R2 =", round(summary(m_e4)$r.squared, 4), "\n")

results$task_e <- task_e
results$task_e_r2 <- c(
  wave_only = summary(m_e1)$r.squared,
  plus_demo = summary(m_e2)$r.squared,
  plus_econ = summary(m_e3)$r.squared,
  plus_all  = summary(m_e4)$r.squared
)

# ═══════════════════════════════════════════════════════════════════════════════
# TASK F: Same models for Taiwan
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("TASK F: Taiwan comparison — Tasks C and D\n")
cat("══════════════════════════════════════════════════\n\n")

tw <- tw |>
  group_by(country_label) |>
  mutate(
    trust_gen_n       = normalize_01(trust_generalized),
    efficacy_part_n   = normalize_01(efficacy_participate),
    efficacy_noinf_n  = normalize_01(efficacy_no_influence),
    efficacy_comp_n   = normalize_01(efficacy_complicated),
    pol_discuss_n     = normalize_01(pol_discuss_raw),
    pol_news_n        = normalize_01(pol_news_follow_raw),
    nat_proud_n2      = normalize_01(nat_proud_raw),
    sat_president_n   = normalize_01(sat_president),
    dem_free_speech_n = normalize_01(dem_free_speech),
    dem_vs_econ_n     = normalize_01(dem_vs_econ),
    dem_suitability_n = normalize_01(dem_suitability),
    dem_best_form_n   = normalize_01(dem_best_form),
    china_harm_n      = normalize_01(5L - intl_china_asia_goodharm),
    voted_last_n      = normalize_01(voted_last)
  ) |>
  ungroup()

tw$wave_f <- factor(tw$wave)

# Task C for Taiwan
cat("--- TASK C (Taiwan): Standard predictor model ---\n\n")
m_c_tw <- lm(qual_pref_dem_n ~ econ_index + age_n + gender + edu_n +
               urban_rural + polint_n + wave_f, data = tw)
task_c_tw <- tidy_hc2(m_c_tw) |> mutate(sig = sig_stars(p.value))

cat("DV: qual_pref_dem_n (higher = LESS preferable)\n")
cat("N =", nobs(m_c_tw), ", R2 =", round(summary(m_c_tw)$r.squared, 4), "\n\n")
print(task_c_tw |> select(term, estimate, std.error, p.value, sig), n = 20)

results$task_c_taiwan <- task_c_tw

# Task D for Taiwan: check coverage and build model
cat("\n--- TASK D (Taiwan): Extended predictor model ---\n\n")

coverage_tw <- tw |>
  summarise(across(all_of(extended_vars), ~sum(!is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_nonmissing") |>
  mutate(pct = round(n_nonmissing / nrow(tw) * 100, 1))

cat("Variable coverage (Taiwan):\n")
print(coverage_tw, n = 20)

usable_vars_tw <- coverage_tw |> filter(pct > 30) |> pull(variable)
cat("\nUsable variables (>30% coverage):", paste(usable_vars_tw, collapse = ", "), "\n")

# Use intersection of usable vars for comparability
common_vars <- intersect(usable_vars, usable_vars_tw)

fml_d_tw_str <- paste("qual_pref_dem_n ~ econ_index + age_n + gender + edu_n + urban_rural + polint_n +",
                      paste(common_vars, collapse = " + "),
                      "+ wave_f")
fml_d_tw <- as.formula(fml_d_tw_str)

m_d_tw <- lm(fml_d_tw, data = tw)
task_d_tw <- tidy_hc2(m_d_tw) |> mutate(sig = sig_stars(p.value))

cat("\nDV: qual_pref_dem_n (higher = LESS preferable)\n")
cat("N =", nobs(m_d_tw), ", R2 =", round(summary(m_d_tw)$r.squared, 4), "\n\n")
print(task_d_tw |> select(term, estimate, std.error, p.value, sig), n = 30)

results$task_d_taiwan <- task_d_tw
results$task_d_taiwan_r2 <- summary(m_d_tw)$r.squared
results$task_d_taiwan_n <- nobs(m_d_tw)

# Taiwan wave FE comparison
cat("\n--- TASK E (Taiwan): Wave FE magnitude ---\n\n")

m_e1_tw <- lm(qual_pref_dem_n ~ wave_f, data = tw)
m_e2_tw <- lm(qual_pref_dem_n ~ wave_f + age_n + gender + edu_n + urban_rural, data = tw)
m_e3_tw <- lm(qual_pref_dem_n ~ wave_f + age_n + gender + edu_n + urban_rural + polint_n + econ_index, data = tw)
m_e4_tw <- m_d_tw

task_e_tw <- bind_rows(
  extract_wave_fes(m_e1_tw, "Wave FE only"),
  extract_wave_fes(m_e2_tw, "Wave FE + demographics"),
  extract_wave_fes(m_e3_tw, "Wave FE + demo + polint + econ"),
  extract_wave_fes(m_e4_tw, "Wave FE + all predictors")
)

task_e_tw_wide <- task_e_tw |>
  mutate(coef_str = sprintf("%.3f (p=%.3f)", estimate, p.value)) |>
  select(term, model, coef_str) |>
  pivot_wider(names_from = model, values_from = coef_str)
print(task_e_tw_wide, width = 150)

cat("\nR-squared progression (Taiwan):\n")
cat("  Wave FE only:                 R2 =", round(summary(m_e1_tw)$r.squared, 4), "\n")
cat("  + demographics:               R2 =", round(summary(m_e2_tw)$r.squared, 4), "\n")
cat("  + demo + polint + econ:       R2 =", round(summary(m_e3_tw)$r.squared, 4), "\n")
cat("  + all extended predictors:    R2 =", round(summary(m_e4_tw)$r.squared, 4), "\n")

results$task_e_taiwan <- task_e_tw
results$task_e_taiwan_r2 <- c(
  wave_only = summary(m_e1_tw)$r.squared,
  plus_demo = summary(m_e2_tw)$r.squared,
  plus_econ = summary(m_e3_tw)$r.squared,
  plus_all  = summary(m_e4_tw)$r.squared
)

# ═══════════════════════════════════════════════════════════════════════════════
# Taiwan cohort analysis for comparison
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("BONUS: Taiwan age group × wave — % 'always preferable'\n")
cat("══════════════════════════════════════════════════\n\n")

tw <- tw |>
  mutate(
    survey_year_tw = case_when(
      !is.na(int_year) ~ as.integer(int_year),
      TRUE ~ as.integer(yr_tw[as.character(wave)])
    ),
    birth_year_tw = survey_year_tw - age,
    age_group4 = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 59 ~ "45-59",
      age >= 60             ~ "60+"
    ),
    age_group4 = factor(age_group4, levels = c("18-29","30-44","45-59","60+")),
    birth_cohort = case_when(
      birth_year_tw <= 1955              ~ "<=1955",
      birth_year_tw >= 1956 & birth_year_tw <= 1965 ~ "1956-1965",
      birth_year_tw >= 1966 & birth_year_tw <= 1975 ~ "1966-1975",
      birth_year_tw >= 1976 & birth_year_tw <= 1985 ~ "1976-1985",
      birth_year_tw >= 1986              ~ "1986+"
    ),
    birth_cohort = factor(birth_cohort,
                          levels = c("<=1955","1956-1965","1966-1975","1976-1985","1986+"))
  )

task_a_tw <- tw |>
  filter(!is.na(dem_always_preferable), !is.na(age_group4)) |>
  group_by(wave, age_group4) |>
  summarise(pct_always = mean(dem_always_preferable == 1, na.rm = TRUE) * 100, .groups = "drop")

task_a_tw_overall <- tw |>
  filter(!is.na(dem_always_preferable)) |>
  group_by(wave) |>
  summarise(pct_always = mean(dem_always_preferable == 1, na.rm = TRUE) * 100, .groups = "drop") |>
  mutate(age_group4 = "Overall")

task_a_tw_full <- bind_rows(task_a_tw, task_a_tw_overall) |>
  select(wave, age_group4, pct_always) |>
  pivot_wider(names_from = age_group4, values_from = pct_always)
print(task_a_tw_full, width = 120)

task_b_tw <- tw |>
  filter(!is.na(dem_always_preferable), !is.na(birth_cohort)) |>
  group_by(birth_cohort, wave) |>
  summarise(pct_always = mean(dem_always_preferable == 1, na.rm = TRUE) * 100, .groups = "drop") |>
  mutate(wave_label = paste0("W", wave)) |>
  select(birth_cohort, wave_label, pct_always) |>
  pivot_wider(names_from = wave_label, values_from = pct_always)

cat("\nTaiwan birth cohort tracking:\n")
print(task_b_tw, width = 120)

results$task_a_taiwan <- task_a_tw_full
results$task_b_taiwan <- task_b_tw

# ═══════════════════════════════════════════════════════════════════════════════
# SAVE
# ═══════════════════════════════════════════════════════════════════════════════
saveRDS(results, file.path(results_dir, "abstract_pref_investigation.rds"))
cat("\n\nSaved: analysis/results/abstract_pref_investigation.rds\n")
cat("Done.\n")
