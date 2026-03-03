#!/usr/bin/env Rscript
# 06_reviewer2_response.R — Statistical analyses for Reviewer 2 response
# Tasks: 1a (construct validity), 1b (factor analysis), 2a (marginal effects),
#        3 (W1 vs W2 composition), 4 (measurement invariance)
#
# Input:  analysis/results/analysis_data.rds
# Output: analysis/results/reviewer2_results.rds (all results bundled)

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(psych)
library(lavaan)
library(semTools)
library(modelsummary)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/03b_sk_satisfaction_paradox")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

source(file.path(paper_dir, "R", "helpers.R"))

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))
dat <- dat |> mutate(is_korea = as.integer(country_label == "Korea"))

controls_str <- "age_n + gender + edu_n + urban_rural + polint_n"

cat("=== Reviewer 2 Response Analyses ===\n")
cat("Loaded:", nrow(dat), "observations\n")
cat("Korea:", sum(dat$country_label == "Korea"), "| Taiwan:", sum(dat$country_label == "Taiwan"), "\n\n")

# ═════════════════════════════════════════════════════════════════════════════
# TASK 1a: Construct validity — partial out democratic satisfaction
# ═════════════════════════════════════════════════════════════════════════════
cat("━━━ TASK 1a: Construct Validity — Partialing Out Satisfaction ━━━\n\n")

auth_dvs <- c("auth_reject_index", "strongman_reject_n", "military_reject_n",
               "expert_reject_n", "singleparty_reject_n")
auth_dv_labels <- c("Auth rejection index", "Reject: strongman",
                     "Reject: military", "Reject: expert", "Reject: single-party")
names(auth_dv_labels) <- auth_dvs
countries <- c("Korea", "Taiwan")

task1a_results <- map_dfr(countries, function(cntry) {
  df <- dat |> filter(country_label == cntry)
  map_dfr(auth_dvs, function(dv) {
    # Model A: baseline (no satisfaction control)
    f_base <- as.formula(paste(dv, "~ econ_index + factor(wave) +", controls_str))
    m_base <- lm(f_base, data = df)
    coef_base <- extract_coef(m_base, "econ_index")

    # Model B: add sat_democracy_n as control
    f_sat <- as.formula(paste(dv, "~ econ_index + sat_democracy_n + factor(wave) +", controls_str))
    m_sat <- lm(f_sat, data = df)
    coef_sat <- extract_coef(m_sat, "econ_index")

    # R-squared values
    r2_base <- summary(m_base)$r.squared
    r2_sat  <- summary(m_sat)$r.squared

    bind_rows(
      coef_base |> mutate(model = "Baseline", r_squared = r2_base),
      coef_sat  |> mutate(model = "+ Satisfaction", r_squared = r2_sat)
    ) |>
      mutate(
        country  = cntry,
        dv       = dv,
        dv_label = auth_dv_labels[dv],
        stars    = sig_stars(p.value)
      )
  })
})

# Print Task 1a results
cat("Table 1a: β on econ_index WITH and WITHOUT democratic satisfaction control\n")
cat("         (Pooled OLS, wave FE, HC2 robust SEs)\n\n")

task1a_print <- task1a_results |>
  select(country, dv_label, model, estimate, std.error, p.value, stars, r_squared) |>
  mutate(across(c(estimate, std.error, p.value, r_squared), \(x) round(x, 4)))

for (cntry in countries) {
  cat(sprintf("\n--- %s ---\n", cntry))
  sub <- task1a_print |> filter(country == cntry)
  wide <- sub |>
    pivot_wider(
      id_cols = dv_label,
      names_from = model,
      values_from = c(estimate, std.error, p.value, stars, r_squared),
      names_glue = "{model}_{.value}"
    )
  for (i in seq_len(nrow(wide))) {
    cat(sprintf("  %s\n", wide$dv_label[i]))
    cat(sprintf("    Baseline:         β = %7.4f (SE = %.4f), p = %.4f %s, R² = %.4f\n",
                wide$`Baseline_estimate`[i], wide$`Baseline_std.error`[i],
                wide$`Baseline_p.value`[i], wide$`Baseline_stars`[i],
                wide$`Baseline_r_squared`[i]))
    cat(sprintf("    + Satisfaction:   β = %7.4f (SE = %.4f), p = %.4f %s, R² = %.4f\n",
                wide$`+ Satisfaction_estimate`[i], wide$`+ Satisfaction_std.error`[i],
                wide$`+ Satisfaction_p.value`[i], wide$`+ Satisfaction_stars`[i],
                wide$`+ Satisfaction_r_squared`[i]))
    # Percent attenuation
    b0 <- wide$`Baseline_estimate`[i]
    b1 <- wide$`+ Satisfaction_estimate`[i]
    if (abs(b0) > 1e-6) {
      atten <- (1 - b1/b0) * 100
      cat(sprintf("    Attenuation:      %.1f%%\n", atten))
    }
  }
}

# Also store the full model objects for modelsummary tables
task1a_models <- list()
for (cntry in countries) {
  df <- dat |> filter(country_label == cntry)
  for (dv in auth_dvs) {
    f_base <- as.formula(paste(dv, "~ econ_index + factor(wave) +", controls_str))
    f_sat  <- as.formula(paste(dv, "~ econ_index + sat_democracy_n + factor(wave) +", controls_str))
    m_base <- lm(f_base, data = df)
    m_sat  <- lm(f_sat, data = df)
    label <- paste0(cntry, "_", dv)
    task1a_models[[paste0(label, "_base")]] <- m_base
    task1a_models[[paste0(label, "_sat")]]  <- m_sat
  }
}

cat("\n")

# ═════════════════════════════════════════════════════════════════════════════
# TASK 1b: Factor analysis — satisfaction vs. auth rejection items
# ═════════════════════════════════════════════════════════════════════════════
cat("━━━ TASK 1b: Factor Analysis — Satisfaction vs. Auth Rejection ━━━\n\n")

# Items for EFA
efa_items <- c("sat_democracy_n", "sat_govt_n", "qual_pref_dem_n",
               "strongman_reject_n", "military_reject_n",
               "expert_reject_n", "singleparty_reject_n")
efa_labels <- c("Democracy satisfaction", "Gov't satisfaction", "Dem always preferable",
                "Reject: strongman", "Reject: military", "Reject: expert", "Reject: single-party")

# Korea pooled across waves
kr_efa <- dat |>
  filter(country_label == "Korea") |>
  select(all_of(efa_items)) |>
  drop_na()

cat("Korea pooled EFA: n =", nrow(kr_efa), "\n\n")

# Parallel analysis to determine number of factors
cat("Parallel analysis (Korea pooled):\n")
pa <- fa.parallel(kr_efa, fm = "ml", fa = "fa", plot = FALSE, n.iter = 100)
cat(sprintf("  Suggested factors: %d\n\n", pa$nfact))

# 2-factor solution (our theoretical expectation)
efa_2f <- fa(kr_efa, nfactors = 2, rotate = "oblimin", fm = "ml")

cat("2-Factor Solution (oblimin rotation, ML extraction):\n")
cat(sprintf("  RMSEA = %.3f, TLI = %.3f, BIC = %.1f\n",
            efa_2f$RMSEA[1], efa_2f$TLI, efa_2f$BIC))
cat(sprintf("  Factor correlation: %.3f\n\n", efa_2f$Phi[1, 2]))

# Loadings table
loadings_mat <- as.data.frame(unclass(efa_2f$loadings))
loadings_mat$item <- efa_labels
loadings_mat$communality <- efa_2f$communalities

cat("Factor Loadings:\n")
cat(sprintf("  %-28s  %8s  %8s  %8s\n", "Item", "Factor 1", "Factor 2", "h²"))
cat(paste(rep("-", 60), collapse = ""), "\n")
for (i in seq_len(nrow(loadings_mat))) {
  cat(sprintf("  %-28s  %8.3f  %8.3f  %8.3f\n",
              loadings_mat$item[i],
              loadings_mat$ML1[i], loadings_mat$ML2[i],
              loadings_mat$communality[i]))
}

# 3-factor solution for comparison
efa_3f <- fa(kr_efa, nfactors = 3, rotate = "oblimin", fm = "ml")

cat(sprintf("\n3-Factor Solution (for comparison):\n"))
cat(sprintf("  RMSEA = %.3f, TLI = %.3f, BIC = %.1f\n",
            efa_3f$RMSEA[1], efa_3f$TLI, efa_3f$BIC))

loadings_3f <- as.data.frame(unclass(efa_3f$loadings))
loadings_3f$item <- efa_labels

cat("\n3-Factor Loadings:\n")
cat(sprintf("  %-28s  %8s  %8s  %8s\n", "Item", "Factor 1", "Factor 2", "Factor 3"))
cat(paste(rep("-", 65), collapse = ""), "\n")
for (i in seq_len(nrow(loadings_3f))) {
  cat(sprintf("  %-28s  %8.3f  %8.3f  %8.3f\n",
              loadings_3f$item[i],
              loadings_3f$ML1[i], loadings_3f$ML2[i], loadings_3f$ML3[i]))
}

# Wave-by-wave EFA for Korea (2-factor) to show stability
cat("\n\nWave-by-wave 2-factor EFA (Korea):\n")
cat(sprintf("  %-6s  %5s  %8s  %8s  %8s\n", "Wave", "N", "RMSEA", "TLI", "r(F1,F2)"))
cat(paste(rep("-", 50), collapse = ""), "\n")

efa_by_wave <- list()
for (w in sort(unique(dat$wave[dat$country_label == "Korea"]))) {
  kr_w <- dat |>
    filter(country_label == "Korea", wave == w) |>
    select(all_of(efa_items)) |>
    drop_na()
  if (nrow(kr_w) > 50) {
    efa_w <- tryCatch(
      fa(kr_w, nfactors = 2, rotate = "oblimin", fm = "ml"),
      error = function(e) NULL
    )
    if (!is.null(efa_w)) {
      efa_by_wave[[as.character(w)]] <- efa_w
      cat(sprintf("  W%-4d  %5d  %8.3f  %8.3f  %8.3f\n",
                  w, nrow(kr_w), efa_w$RMSEA[1], efa_w$TLI, efa_w$Phi[1, 2]))
    }
  }
}

task1b_results <- list(
  parallel_analysis = pa$nfact,
  efa_2f = efa_2f,
  efa_3f = efa_3f,
  efa_by_wave = efa_by_wave,
  n_obs = nrow(kr_efa)
)

cat("\n")

# ═════════════════════════════════════════════════════════════════════════════
# TASK 2a: Marginal effects at meaningful values
# ═════════════════════════════════════════════════════════════════════════════
cat("━━━ TASK 2a: Marginal Effects at 25th/75th Percentile ━━━\n\n")

kr_dat <- dat |> filter(country_label == "Korea")

# Percentiles of econ_index for Korea
econ_q <- quantile(kr_dat$econ_index, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
cat(sprintf("Korea econ_index percentiles: P25=%.4f, P50=%.4f, P75=%.4f\n\n",
            econ_q[1], econ_q[2], econ_q[3]))

# Function to compute predicted values at econ percentiles
predict_at_percentiles <- function(dv, data, econ_quantiles) {
  f <- as.formula(paste(dv, "~ econ_index + factor(wave) +", controls_str))
  m <- lm(f, data = data)

  # Create prediction data at 25th and 75th percentile, controls at means/modes
  newdata_base <- data |>
    summarise(
      age_n      = mean(age_n, na.rm = TRUE),
      edu_n      = mean(edu_n, na.rm = TRUE),
      polint_n   = mean(polint_n, na.rm = TRUE),
      gender     = as.numeric(names(sort(table(gender), decreasing = TRUE))[1]),
      urban_rural = as.numeric(names(sort(table(urban_rural), decreasing = TRUE))[1]),
      wave       = as.numeric(names(sort(table(wave), decreasing = TRUE))[1])
    )
  # Make wave a factor to match model
  newdata_base$wave <- factor(newdata_base$wave, levels = levels(factor(data$wave)))

  nd_p25 <- newdata_base |> mutate(econ_index = econ_quantiles[1])
  nd_p75 <- newdata_base |> mutate(econ_index = econ_quantiles[3])

  pred_p25 <- predict(m, newdata = nd_p25)
  pred_p75 <- predict(m, newdata = nd_p75)

  # R-squared
  r2 <- summary(m)$r.squared

  tibble(
    dv = dv,
    pred_p25 = pred_p25,
    pred_p75 = pred_p75,
    diff_01  = pred_p75 - pred_p25,
    diff_4pt = (pred_p75 - pred_p25) * 3,  # 0-1 scale spans 3 units on 4-pt scale
    r_squared = r2
  )
}

marginal_dvs <- c("auth_reject_index", "strongman_reject_n", "military_reject_n",
                   "expert_reject_n", "singleparty_reject_n")
marginal_labels <- c("Auth rejection index", "Reject: strongman", "Reject: military",
                      "Reject: expert", "Reject: single-party")

task2a_results <- map_dfr(marginal_dvs, \(dv) predict_at_percentiles(dv, kr_dat, econ_q))
task2a_results$dv_label <- marginal_labels

cat("Predicted values at 25th vs 75th percentile of econ_index (Korea pooled)\n")
cat("Controls at means/modes, modal wave\n\n")
cat(sprintf("  %-25s  %8s  %8s  %10s  %10s  %8s\n",
            "DV", "Pred P25", "Pred P75", "Diff (0-1)", "Diff (4pt)", "R²"))
cat(paste(rep("-", 80), collapse = ""), "\n")
for (i in seq_len(nrow(task2a_results))) {
  cat(sprintf("  %-25s  %8.4f  %8.4f  %10.4f  %10.4f  %8.4f\n",
              task2a_results$dv_label[i],
              task2a_results$pred_p25[i], task2a_results$pred_p75[i],
              task2a_results$diff_01[i], task2a_results$diff_4pt[i],
              task2a_results$r_squared[i]))
}

# IQR of econ_index for context
econ_iqr <- econ_q[3] - econ_q[1]
cat(sprintf("\nEcon_index IQR (P75 - P25): %.4f on 0-1 scale\n", econ_iqr))

# Also compute the strongman item specifically with SE via delta method / bootstrap
cat("\nStrongman rejection — interpretive translation:\n")
sm_row <- task2a_results |> filter(dv == "strongman_reject_n")
cat(sprintf("  A Korean citizen at P75 of economic comfort scores %.4f points higher\n", sm_row$diff_01))
cat(sprintf("  on rejection of strongman rule (0-1 scale) than one at P25.\n"))
cat(sprintf("  On the original 4-point scale, this equals %.2f points — roughly %.1f%%\n",
            sm_row$diff_4pt, abs(sm_row$diff_4pt) / 3 * 100))
cat(sprintf("  of the distance between adjacent response categories.\n"))

cat("\n")

# ═════════════════════════════════════════════════════════════════════════════
# TASK 3: Wave 1 vs Wave 2 Korea comparison
# ═════════════════════════════════════════════════════════════════════════════
cat("━━━ TASK 3: Korea Wave 1 (2003) vs Wave 2 (2006) Comparison ━━━\n\n")

kr_w1w2 <- dat |>
  filter(country_label == "Korea", wave %in% c(1, 2))

# 3a: Demographic profile comparison
cat("3a: Demographic Composition\n\n")

demo_vars <- c("age_n", "edu_n", "polint_n", "gender", "urban_rural")
demo_labels <- c("Age (0-1)", "Education (0-1)", "Political interest (0-1)",
                  "Gender (prop male)", "Urban (prop urban)")

demo_comparison <- kr_w1w2 |>
  group_by(wave) |>
  summarise(
    age_mean       = mean(age_n, na.rm = TRUE),
    age_sd         = sd(age_n, na.rm = TRUE),
    edu_mean       = mean(edu_n, na.rm = TRUE),
    edu_sd         = sd(edu_n, na.rm = TRUE),
    polint_mean    = mean(polint_n, na.rm = TRUE),
    polint_sd      = sd(polint_n, na.rm = TRUE),
    prop_male      = mean(gender == 1, na.rm = TRUE),
    prop_urban     = mean(urban_rural == 1, na.rm = TRUE),
    n              = n(),
    .groups = "drop"
  )

cat(sprintf("  %-25s  %12s  %12s\n", "Variable", "W1 (2003)", "W2 (2006)"))
cat(paste(rep("-", 55), collapse = ""), "\n")
w1 <- demo_comparison |> filter(wave == 1)
w2 <- demo_comparison |> filter(wave == 2)
cat(sprintf("  %-25s  %5.3f (%.3f)  %5.3f (%.3f)\n", "Age (0-1)",
            w1$age_mean, w1$age_sd, w2$age_mean, w2$age_sd))
cat(sprintf("  %-25s  %5.3f (%.3f)  %5.3f (%.3f)\n", "Education (0-1)",
            w1$edu_mean, w1$edu_sd, w2$edu_mean, w2$edu_sd))
cat(sprintf("  %-25s  %5.3f (%.3f)  %5.3f (%.3f)\n", "Political interest (0-1)",
            w1$polint_mean, w1$polint_sd, w2$polint_mean, w2$polint_sd))
cat(sprintf("  %-25s  %12.3f  %12.3f\n", "Prop. male", w1$prop_male, w2$prop_male))
cat(sprintf("  %-25s  %12.3f  %12.3f\n", "Prop. urban", w1$prop_urban, w2$prop_urban))
cat(sprintf("  %-25s  %12d  %12d\n", "N", w1$n, w2$n))

# T-tests for demographic differences
cat("\n  Difference tests (Welch t-tests):\n")
for (v in c("age_n", "edu_n", "polint_n")) {
  v1 <- kr_w1w2 |> filter(wave == 1) |> pull(!!sym(v))
  v2 <- kr_w1w2 |> filter(wave == 2) |> pull(!!sym(v))
  tt <- t.test(v1, v2)
  cat(sprintf("    %s: diff = %.3f, t = %.2f, p = %.4f\n",
              v, tt$estimate[1] - tt$estimate[2], tt$statistic, tt$p.value))
}

# 3b: Economic conditions comparison
cat("\n3b: Economic Evaluations Distribution\n\n")

econ_comparison <- kr_w1w2 |>
  group_by(wave) |>
  summarise(
    econ_index_mean = mean(econ_index, na.rm = TRUE),
    econ_index_sd   = sd(econ_index, na.rm = TRUE),
    econ_index_p25  = quantile(econ_index, 0.25, na.rm = TRUE),
    econ_index_p50  = quantile(econ_index, 0.50, na.rm = TRUE),
    econ_index_p75  = quantile(econ_index, 0.75, na.rm = TRUE),
    econ_national_mean = mean(econ_national_n, na.rm = TRUE),
    econ_family_mean   = mean(econ_family_n, na.rm = TRUE),
    econ_outlook_mean  = mean(econ_outlook_n, na.rm = TRUE),
    n_econ = sum(!is.na(econ_index)),
    .groups = "drop"
  )

cat(sprintf("  %-28s  %12s  %12s\n", "Measure", "W1 (2003)", "W2 (2006)"))
cat(paste(rep("-", 58), collapse = ""), "\n")
e1 <- econ_comparison |> filter(wave == 1)
e2 <- econ_comparison |> filter(wave == 2)
cat(sprintf("  %-28s  %5.3f (%.3f)  %5.3f (%.3f)\n", "Econ index: mean (SD)",
            e1$econ_index_mean, e1$econ_index_sd, e2$econ_index_mean, e2$econ_index_sd))
cat(sprintf("  %-28s  %12.3f  %12.3f\n", "Econ index: P25", e1$econ_index_p25, e2$econ_index_p25))
cat(sprintf("  %-28s  %12.3f  %12.3f\n", "Econ index: P50", e1$econ_index_p50, e2$econ_index_p50))
cat(sprintf("  %-28s  %12.3f  %12.3f\n", "Econ index: P75", e1$econ_index_p75, e2$econ_index_p75))
cat(sprintf("  %-28s  %12.3f  %12.3f\n", "Econ national (mean)", e1$econ_national_mean, e2$econ_national_mean))
cat(sprintf("  %-28s  %12.3f  %12.3f\n", "Econ family (mean)", e1$econ_family_mean, e2$econ_family_mean))
cat(sprintf("  %-28s  %12.3f  %12.3f\n", "Econ outlook (mean)", e1$econ_outlook_mean, e2$econ_outlook_mean))

# T-test on econ_index
e_v1 <- kr_w1w2 |> filter(wave == 1) |> pull(econ_index)
e_v2 <- kr_w1w2 |> filter(wave == 2) |> pull(econ_index)
e_tt <- t.test(e_v1, e_v2)
cat(sprintf("\n  Econ index difference: diff = %.3f, t = %.2f, p = %.4f\n",
            e_tt$estimate[1] - e_tt$estimate[2], e_tt$statistic, e_tt$p.value))

# Variance ratio (F-test)
var_test <- var.test(e_v1, e_v2)
cat(sprintf("  Variance ratio (W1/W2): %.3f, F-test p = %.4f\n",
            var_test$statistic, var_test$p.value))

# DV comparison
cat("\n  Key DV comparison:\n")
dv_comp_vars <- c("qual_pref_dem_n", "auth_reject_index", "sat_democracy_n", "sat_govt_n")
dv_comp_labels <- c("Dem always preferable", "Auth rejection index", "Democracy satisfaction", "Gov't satisfaction")
for (i in seq_along(dv_comp_vars)) {
  v1 <- kr_w1w2 |> filter(wave == 1) |> pull(!!sym(dv_comp_vars[i]))
  v2 <- kr_w1w2 |> filter(wave == 2) |> pull(!!sym(dv_comp_vars[i]))
  cat(sprintf("    %s: W1 = %.3f (SD %.3f), W2 = %.3f (SD %.3f)\n",
              dv_comp_labels[i],
              mean(v1, na.rm = TRUE), sd(v1, na.rm = TRUE),
              mean(v2, na.rm = TRUE), sd(v2, na.rm = TRUE)))
}

# The critical citizens effect: re-run the W1 model to confirm the negative beta
cat("\n  Re-running W1 and W2 models for dem_always_preferable:\n")
for (w in 1:2) {
  df_w <- kr_w1w2 |> filter(wave == w)
  f <- as.formula(paste("qual_pref_dem_n ~ econ_index +", controls_str))
  m <- lm(f, data = df_w)
  ec <- extract_coef(m, "econ_index")
  cat(sprintf("    Wave %d: β = %.4f (SE = %.4f), p = %.4f %s, n = %d\n",
              w, ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value),
              sum(!is.na(df_w$qual_pref_dem_n) & !is.na(df_w$econ_index))))
}

task3_results <- list(
  demo_comparison = demo_comparison,
  econ_comparison = econ_comparison
)

cat("\n")

# ═════════════════════════════════════════════════════════════════════════════
# TASK 4: Measurement invariance — auth rejection items
# ═════════════════════════════════════════════════════════════════════════════
cat("━━━ TASK 4: Measurement Invariance — Auth Rejection Items ━━━\n\n")

# Prepare data for lavaan
mi_dat <- dat |>
  select(country_label, strongman_reject_n, military_reject_n,
         expert_reject_n, singleparty_reject_n) |>
  drop_na() |>
  mutate(group = as.character(country_label))

cat("Measurement invariance sample: Korea =", sum(mi_dat$group == "Korea"),
    ", Taiwan =", sum(mi_dat$group == "Taiwan"), "\n\n")

# 1-factor CFA model
mi_model <- '
  auth_reject =~ strongman_reject_n + military_reject_n + expert_reject_n + singleparty_reject_n
'

# Configural invariance (same structure, free parameters)
fit_config <- cfa(mi_model, data = mi_dat, group = "group",
                  estimator = "MLR")

# Metric invariance (equal loadings)
fit_metric <- cfa(mi_model, data = mi_dat, group = "group",
                  group.equal = c("loadings"), estimator = "MLR")

# Scalar invariance (equal loadings + intercepts)
fit_scalar <- cfa(mi_model, data = mi_dat, group = "group",
                  group.equal = c("loadings", "intercepts"), estimator = "MLR")

# Comparison table
cat("Fit Statistics:\n")
cat(sprintf("  %-12s  %8s  %6s  %8s  %6s  %8s  %6s\n",
            "Model", "CFI", "RMSEA", "SRMR", "TLI", "Chi-sq", "df"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (label_fit in list(
  list("Configural", fit_config),
  list("Metric", fit_metric),
  list("Scalar", fit_scalar)
)) {
  nm <- label_fit[[1]]
  ft <- label_fit[[2]]
  fm <- fitMeasures(ft, c("cfi.robust", "rmsea.robust", "srmr", "tli.robust",
                           "chisq.scaled", "df.scaled"))
  cat(sprintf("  %-12s  %8.3f  %6.3f  %8.3f  %6.3f  %8.1f  %6.0f\n",
              nm, fm["cfi.robust"], fm["rmsea.robust"], fm["srmr"],
              fm["tli.robust"], fm["chisq.scaled"], fm["df.scaled"]))
}

# Model comparison tests
cat("\nModel Comparisons (scaled chi-square difference test):\n")
comp_cm <- lavTestLRT(fit_config, fit_metric)
comp_ms <- lavTestLRT(fit_metric, fit_scalar)

cat(sprintf("  Configural → Metric:  Δχ² = %.2f, Δdf = %d, p = %.4f\n",
            comp_cm[["Chisq diff"]][2], comp_cm[["Df diff"]][2], comp_cm[["Pr(>Chisq)"]][2]))
cat(sprintf("  Metric → Scalar:      Δχ² = %.2f, Δdf = %d, p = %.4f\n",
            comp_ms[["Chisq diff"]][2], comp_ms[["Df diff"]][2], comp_ms[["Pr(>Chisq)"]][2]))

# CFI difference (Chen 2007 criterion: ΔCFI < .010)
cfi_config <- fitMeasures(fit_config, "cfi.robust")
cfi_metric <- fitMeasures(fit_metric, "cfi.robust")
cfi_scalar <- fitMeasures(fit_scalar, "cfi.robust")

cat(sprintf("\n  ΔCFI (Configural → Metric): %.4f  [threshold: < .010]\n",
            cfi_config - cfi_metric))
cat(sprintf("  ΔCFI (Metric → Scalar):     %.4f  [threshold: < .010]\n",
            cfi_metric - cfi_scalar))

task4_results <- list(
  fit_configural = fit_config,
  fit_metric     = fit_metric,
  fit_scalar     = fit_scalar,
  comparison_cm  = comp_cm,
  comparison_ms  = comp_ms
)

cat("\n")

# ═════════════════════════════════════════════════════════════════════════════
# SAVE ALL RESULTS
# ═════════════════════════════════════════════════════════════════════════════
cat("━━━ Saving Results ━━━\n")

reviewer2_results <- list(
  task1a = task1a_results,
  task1a_models = task1a_models,
  task1b = task1b_results,
  task2a = task2a_results,
  task3  = task3_results,
  task4  = task4_results
)

saveRDS(reviewer2_results, file.path(results_dir, "reviewer2_results.rds"))
cat("Saved reviewer2_results.rds\n")
cat("\n=== All Reviewer 2 analyses complete ===\n")
