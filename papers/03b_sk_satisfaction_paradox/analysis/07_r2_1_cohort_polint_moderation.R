#!/usr/bin/env Rscript
# 07_r2_1_cohort_polint_moderation.R — R2.1 reviewer response analyses
#
# Tasks:
#   1. Age cohort moderation: pre-democratization (born <= 1975) vs post (born > 1975)
#      on auth_reject_index, with formal age x econ interaction test
#   2. Political interest moderation on auth rejection index
#   3. Wave-specific age x econ interactions on auth rejection
#
# Input:  analysis/results/analysis_data.rds
# Output: analysis/results/r2_1_results.rds

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(modelsummary)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/03b_sk_satisfaction_paradox")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

source(file.path(paper_dir, "R", "helpers.R"))

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

# Survey year lookup for birth year derivation
yr_kr <- c(`1` = 2003L, `2` = 2006L, `3` = 2011L, `4` = 2015L, `5` = 2019L, `6` = 2022L)
yr_tw <- c(`1` = 2001L, `2` = 2006L, `3` = 2010L, `4` = 2014L, `5` = 2019L, `6` = 2022L)

dat <- dat |>
  mutate(
    # Use int_year (date of interview) where available; fall back to survey year lookup for W1-W2
    survey_year = case_when(
      !is.na(int_year) ~ as.integer(int_year),
      country_label == "Korea"  ~ yr_kr[as.character(wave)],
      country_label == "Taiwan" ~ yr_tw[as.character(wave)]
    ),
    birth_year = survey_year - age,
    # Korea democratized 1987; Taiwan ~1996 (first direct presidential election)
    # Using 1975 cutoff: born <= 1975 came of age under authoritarianism
    age_cohort = case_when(
      birth_year <= 1975 ~ "Pre-democratization",
      birth_year >  1975 ~ "Post-democratization"
    ),
    age_cohort = factor(age_cohort, levels = c("Pre-democratization", "Post-democratization"))
  )

controls_str <- "age_n + gender + edu_n + urban_rural + polint_n"

cat("=== R2.1 Reviewer Response Analyses ===\n")
cat("Loaded:", nrow(dat), "observations\n\n")

# Korea only for cohort analyses
kr <- dat |> filter(country_label == "Korea")

cat("Korea birth year range:", range(kr$birth_year, na.rm = TRUE), "\n")
cat("Korea age cohort distribution:\n")
kr |> count(age_cohort) |> print()
cat("\nCohort x wave:\n")
kr |> count(wave, age_cohort) |> pivot_wider(names_from = age_cohort, values_from = n) |> print()
cat("\n")


# =============================================================================
# TASK 1: Age cohort moderation on authoritarian rejection
# =============================================================================
cat("━━━ TASK 1: Age Cohort Moderation on Auth Rejection ━━━\n\n")

# 1a: Split-sample models
cat("1a: Split-sample OLS — auth_reject_index ~ econ_index (Korea, by cohort)\n\n")

cohort_split_results <- list()
cohort_models <- list()

for (cohort in c("Pre-democratization", "Post-democratization")) {
  sub <- kr |> filter(age_cohort == cohort)
  if (nrow(sub) < 100) next

  f <- as.formula(paste("auth_reject_index ~ econ_index + factor(wave) +", controls_str))
  m <- lm(f, data = sub)
  cohort_models[[cohort]] <- m

  ec <- extract_coef(m, "econ_index")
  cohort_split_results[[cohort]] <- ec |>
    mutate(cohort = cohort, n = nobs(m), r_sq = summary(m)$r.squared)

  cat(sprintf("  %s (n = %d):\n", cohort, nobs(m)))
  cat(sprintf("    b = %.4f (SE = %.4f), p = %.4f %s, R2 = %.4f\n",
              ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value), summary(m)$r.squared))
}

cohort_split_df <- bind_rows(cohort_split_results)

# 1b: Formal interaction test
cat("\n1b: Formal interaction — econ_index x age_cohort (Korea pooled)\n\n")

# Binary: 1 = pre-democratization
kr <- kr |> mutate(pre_dem = as.numeric(age_cohort == "Pre-democratization"))

f_int <- as.formula(paste("auth_reject_index ~ econ_index * pre_dem + factor(wave) +", controls_str))
m_int <- lm(f_int, data = kr)

int_tidy <- tidy_hc2(m_int) |>
  filter(term %in% c("econ_index", "pre_dem", "econ_index:pre_dem"))

cat("  Interaction model (HC2 robust SEs):\n")
for (i in seq_len(nrow(int_tidy))) {
  cat(sprintf("    %-25s  b = %7.4f (SE = %.4f), p = %.4f %s\n",
              int_tidy$term[i], int_tidy$estimate[i], int_tidy$std.error[i],
              int_tidy$p.value[i], sig_stars(int_tidy$p.value[i])))
}

# Implied effects
b_econ <- int_tidy$estimate[int_tidy$term == "econ_index"]
b_int  <- int_tidy$estimate[int_tidy$term == "econ_index:pre_dem"]
cat(sprintf("\n  Implied econ effect for post-dem cohort: %.4f\n", b_econ))
cat(sprintf("  Implied econ effect for pre-dem cohort:  %.4f\n", b_econ + b_int))

# 1c: Also run on individual auth rejection items
cat("\n1c: Split-sample on individual auth rejection items (Korea)\n\n")

auth_items <- c("strongman_reject_n", "military_reject_n",
                "expert_reject_n", "singleparty_reject_n")
auth_labels <- c("Reject strongman", "Reject military", "Reject expert", "Reject single-party")

item_cohort_results <- list()

for (i in seq_along(auth_items)) {
  cat(sprintf("  %s:\n", auth_labels[i]))
  for (cohort in c("Pre-democratization", "Post-democratization")) {
    sub <- kr |> filter(age_cohort == cohort, !is.na(.data[[auth_items[i]]]))
    if (nrow(sub) < 100) next

    f <- as.formula(paste(auth_items[i], "~ econ_index + factor(wave) +", controls_str))
    m <- lm(f, data = sub)
    ec <- extract_coef(m, "econ_index")

    item_cohort_results[[paste(auth_items[i], cohort)]] <- ec |>
      mutate(dv = auth_items[i], dv_label = auth_labels[i], cohort = cohort, n = nobs(m))

    cat(sprintf("    %-22s  b = %7.4f (SE = %.4f), p = %.4f %s, n = %d\n",
                cohort, ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value), nobs(m)))
  }
}

item_cohort_df <- bind_rows(item_cohort_results)

# 1d: Formal interaction on individual items
cat("\n1d: Formal econ x pre_dem interaction on individual items:\n\n")

item_int_results <- list()

for (i in seq_along(auth_items)) {
  sub <- kr |> filter(!is.na(.data[[auth_items[i]]]))
  f <- as.formula(paste(auth_items[i], "~ econ_index * pre_dem + factor(wave) +", controls_str))
  m <- lm(f, data = sub)

  int_row <- extract_coef(m, "econ_index:pre_dem")
  item_int_results[[auth_items[i]]] <- int_row |>
    mutate(dv = auth_items[i], dv_label = auth_labels[i], n = nobs(m))

  cat(sprintf("  %-20s  interaction b = %7.4f (SE = %.4f), p = %.4f %s\n",
              auth_labels[i], int_row$estimate, int_row$std.error,
              int_row$p.value, sig_stars(int_row$p.value)))
}

item_int_df <- bind_rows(item_int_results)

cat("\n")


# =============================================================================
# TASK 2: Political interest moderation on auth rejection index
# =============================================================================
cat("━━━ TASK 2: Political Interest x Econ on Auth Rejection ━━━\n\n")

# Controls without polint_n (since it's the moderator)
controls_no_polint <- "age_n + gender + edu_n + urban_rural"

# 2a: Continuous interaction
f_polint_int <- as.formula(paste(
  "auth_reject_index ~ econ_index * polint_n + factor(wave) +", controls_no_polint
))
m_polint_int <- lm(f_polint_int, data = kr)

polint_tidy <- tidy_hc2(m_polint_int) |>
  filter(term %in% c("econ_index", "polint_n", "econ_index:polint_n"))

cat("2a: Continuous interaction — auth_reject_index ~ econ x polint (Korea)\n")
cat("    HC2 robust SEs, wave FE, n =", nobs(m_polint_int), "\n\n")

for (i in seq_len(nrow(polint_tidy))) {
  cat(sprintf("    %-25s  b = %7.4f (SE = %.4f), p = %.4f %s\n",
              polint_tidy$term[i], polint_tidy$estimate[i], polint_tidy$std.error[i],
              polint_tidy$p.value[i], sig_stars(polint_tidy$p.value[i])))
}

# 2b: Subgroup split (high vs low political interest)
cat("\n2b: Subgroup split — auth_reject_index by polint_group (Korea)\n\n")

polint_split_results <- list()

for (grp in c("High interest", "Low interest")) {
  sub <- kr |> filter(polint_group == grp)
  if (nrow(sub) < 200) next

  f <- as.formula(paste("auth_reject_index ~ econ_index + factor(wave) +", controls_str))
  m <- lm(f, data = sub)
  ec <- extract_coef(m, "econ_index")

  polint_split_results[[grp]] <- ec |>
    mutate(group = grp, n = nobs(m), r_sq = summary(m)$r.squared)

  cat(sprintf("  %s (n = %d):\n", grp, nobs(m)))
  cat(sprintf("    b = %.4f (SE = %.4f), p = %.4f %s\n",
              ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value)))
}

polint_split_df <- bind_rows(polint_split_results)

# 2c: For comparison, replicate the abstract preference interaction
cat("\n2c: For comparison — polint x econ on qual_pref_dem_n (Korea)\n")

f_pref_polint <- as.formula(paste(
  "qual_pref_dem_n ~ econ_index * polint_n + factor(wave) +", controls_no_polint
))
m_pref_polint <- lm(f_pref_polint, data = kr)

pref_polint_tidy <- tidy_hc2(m_pref_polint) |>
  filter(term == "econ_index:polint_n")

cat(sprintf("    econ x polint interaction on dem_pref: b = %.4f (SE = %.4f), p = %.4f %s\n",
            pref_polint_tidy$estimate, pref_polint_tidy$std.error,
            pref_polint_tidy$p.value, sig_stars(pref_polint_tidy$p.value)))

# 2d: Individual auth rejection items
cat("\n2d: Polint x econ interaction on individual auth rejection items (Korea):\n\n")

polint_item_results <- list()

for (i in seq_along(auth_items)) {
  sub <- kr |> filter(!is.na(.data[[auth_items[i]]]))
  f <- as.formula(paste(auth_items[i], "~ econ_index * polint_n + factor(wave) +", controls_no_polint))
  m <- lm(f, data = sub)

  int_row <- extract_coef(m, "econ_index:polint_n")
  polint_item_results[[auth_items[i]]] <- int_row |>
    mutate(dv = auth_items[i], dv_label = auth_labels[i], n = nobs(m))

  cat(sprintf("  %-20s  interaction b = %7.4f (SE = %.4f), p = %.4f %s\n",
              auth_labels[i], int_row$estimate, int_row$std.error,
              int_row$p.value, sig_stars(int_row$p.value)))
}

polint_item_df <- bind_rows(polint_item_results)

cat("\n")


# =============================================================================
# TASK 3: Wave-specific age x econ interactions on auth rejection
# =============================================================================
cat("━━━ TASK 3: Wave-Specific Age x Econ on Auth Rejection ━━━\n\n")

# 3a: Wave-by-wave interaction models
cat("3a: econ x pre_dem interaction by wave (Korea)\n\n")

wave_age_results <- list()

cat(sprintf("  %-6s  %5s  %12s  %12s  %12s  %12s\n",
            "Wave", "Year", "b(econ)", "b(int)", "p(int)", "sig"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (w in sort(unique(kr$wave))) {
  sub <- kr |> filter(wave == w, !is.na(auth_reject_index), !is.na(pre_dem))
  if (nrow(sub) < 100) next

  # Drop wave FE for single-wave model; keep other controls
  f <- as.formula(paste("auth_reject_index ~ econ_index * pre_dem +", controls_str))
  m <- tryCatch(lm(f, data = sub), error = function(e) NULL)
  if (is.null(m)) next

  int_coef <- tryCatch(extract_coef(m, "econ_index:pre_dem"), error = function(e) NULL)
  econ_coef <- tryCatch(extract_coef(m, "econ_index"), error = function(e) NULL)
  if (is.null(int_coef) || is.null(econ_coef)) next

  yr <- yr_kr[as.character(w)]

  wave_age_results[[as.character(w)]] <- bind_rows(
    econ_coef |> mutate(term = "econ_index"),
    int_coef  |> mutate(term = "econ_index:pre_dem")
  ) |> mutate(wave = w, year = yr, n = nobs(m))

  cat(sprintf("  W%-4d  %5d  %12.4f  %12.4f  %12.4f  %s   (n=%d)\n",
              w, yr, econ_coef$estimate, int_coef$estimate,
              int_coef$p.value, sig_stars(int_coef$p.value), nobs(m)))
}

wave_age_df <- bind_rows(wave_age_results)

# 3b: Three-way interaction — econ x pre_dem x wave_c (pooled, Korea)
cat("\n3b: Three-way interaction — econ x pre_dem x wave_c (Korea pooled)\n\n")

kr <- kr |> mutate(wave_c = wave - mean(wave, na.rm = TRUE))

f_3way <- as.formula(paste(
  "auth_reject_index ~ econ_index * pre_dem * wave_c +", controls_str
))
m_3way <- lm(f_3way, data = kr)

threeway_tidy <- tidy_hc2(m_3way) |>
  filter(term %in% c("econ_index", "pre_dem", "wave_c",
                      "econ_index:pre_dem", "econ_index:wave_c",
                      "pre_dem:wave_c", "econ_index:pre_dem:wave_c"))

cat(sprintf("  Three-way model (HC2 robust SEs, n = %d):\n\n", nobs(m_3way)))

for (i in seq_len(nrow(threeway_tidy))) {
  cat(sprintf("    %-35s  b = %7.4f (SE = %.4f), p = %.4f %s\n",
              threeway_tidy$term[i], threeway_tidy$estimate[i],
              threeway_tidy$std.error[i], threeway_tidy$p.value[i],
              sig_stars(threeway_tidy$p.value[i])))
}

cat("\n  Interpretation:\n")
cat("  - econ_index:pre_dem = cohort difference in econ -> auth rejection at mean wave\n")
cat("  - econ_index:pre_dem:wave_c = whether that cohort difference changes over time\n")
cat("  - If three-way is negative and significant: cohort moderation weakens over time\n")
cat("    (consistent with nostalgia mechanism aging out)\n")

# 3c: Cohort share over time
cat("\n\n3c: Pre-democratization cohort share by wave:\n\n")

cohort_shares <- kr |>
  group_by(wave) |>
  summarise(
    year = first(survey_year),
    n = n(),
    n_pre = sum(age_cohort == "Pre-democratization", na.rm = TRUE),
    pct_pre = round(100 * n_pre / n, 1),
    mean_age_pre = mean(age[age_cohort == "Pre-democratization"], na.rm = TRUE),
    mean_age_post = mean(age[age_cohort == "Post-democratization"], na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("  %-6s  %5s  %5s  %8s  %10s  %10s\n",
            "Wave", "Year", "N", "% pre", "Age(pre)", "Age(post)"))
cat(paste(rep("-", 55), collapse = ""), "\n")
for (i in seq_len(nrow(cohort_shares))) {
  cat(sprintf("  W%-4d  %5d  %5d  %7.1f%%  %10.1f  %10.1f\n",
              cohort_shares$wave[i], cohort_shares$year[i], cohort_shares$n[i],
              cohort_shares$pct_pre[i], cohort_shares$mean_age_pre[i],
              cohort_shares$mean_age_post[i]))
}

cat("\n")


# =============================================================================
# SAVE ALL RESULTS
# =============================================================================
cat("━━━ Saving Results ━━━\n")

r2_1_results <- list(
  # Task 1: Age cohort moderation
  cohort_split       = cohort_split_df,
  cohort_interaction = int_tidy,
  cohort_model       = m_int,
  cohort_items_split = item_cohort_df,
  cohort_items_int   = item_int_df,
  cohort_models      = cohort_models,

  # Task 2: Political interest moderation
  polint_interaction      = polint_tidy,
  polint_model            = m_polint_int,
  polint_split            = polint_split_df,
  polint_pref_comparison  = pref_polint_tidy,
  polint_items            = polint_item_df,

  # Task 3: Wave-specific age interactions
  wave_age_interactions   = wave_age_df,
  threeway_interaction    = threeway_tidy,
  threeway_model          = m_3way,
  cohort_shares           = cohort_shares
)

saveRDS(r2_1_results, file.path(results_dir, "r2_1_results.rds"))
cat("Saved r2_1_results.rds\n")
cat("\n=== All R2.1 analyses complete ===\n")
