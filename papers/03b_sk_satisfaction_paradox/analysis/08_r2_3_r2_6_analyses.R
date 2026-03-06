#!/usr/bin/env Rscript
# 08_r2_3_r2_6_analyses.R — R2.3 (Engaged Minority) + R2.6 (Measurement) analyses
#
# Tasks 4-8: Engaged minority operationalization and robustness
# Tasks 9-13: Measurement evidence for the abstract item
# Task 14: Taiwan identity (conditional — skipped if data unavailable)
#
# Input:  analysis/results/analysis_data.rds, abs_harmonized.rds (for extra vars)
# Output: Multiple .rds files in analysis/results/

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(psych)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/03b_sk_satisfaction_paradox")
results_dir  <- file.path(paper_dir, "analysis", "results")

source(file.path(project_root, "_data_config.R"))
source(file.path(paper_dir, "R", "helpers.R"))

dat <- readRDS(file.path(results_dir, "analysis_data.rds"))

# Merge pol_discuss and pol_news_follow from harmonized data
abs_all <- readRDS(abs_harmonized_path)
abs_merge <- abs_all |>
  filter(country %in% c(3, 7)) |>
  select(pol_discuss, pol_news_follow)

# Row-position merge: analysis_data.rds was built from the same filter in 00_data_preparation
stopifnot(nrow(abs_merge) == nrow(dat))
dat$pol_discuss <- abs_merge$pol_discuss
dat$pol_news_follow <- abs_merge$pol_news_follow

# Normalize new variables within country
dat <- dat |>
  group_by(country_label) |>
  mutate(
    discuss_n = normalize_01(pol_discuss),
    news_follow_n = normalize_01(pol_news_follow)
  ) |>
  ungroup()

controls_str <- "age_n + gender + edu_n + urban_rural + polint_n"
controls_no_polint <- "age_n + gender + edu_n + urban_rural"

kr <- dat |> filter(country_label == "Korea")
tw <- dat |> filter(country_label == "Taiwan")

cat("=== R2.3 + R2.6 Analyses ===\n")
cat("Korea:", nrow(kr), "| Taiwan:", nrow(tw), "\n")
cat("pol_discuss coverage (Korea):", sum(!is.na(kr$pol_discuss)), "\n")
cat("pol_news_follow coverage (Korea):", sum(!is.na(kr$pol_news_follow)), "\n\n")


# =============================================================================
# TASK 4: Alternative Cutpoints for Political Interest
# =============================================================================
cat("━━━ TASK 4: Alternative Cutpoints for Political Interest ━━━\n\n")

# 4a: Tertile split
kr <- kr |>
  mutate(
    polint_tertile = ntile(political_interest, 3),
    polint_quartile = ntile(political_interest, 4),
    polint_top_tertile = if_else(polint_tertile == 3, "Top tertile", "Rest"),
    polint_top_quartile = if_else(polint_quartile == 4, "Top quartile", "Rest")
  )

cat("4a: Tertile split on dem_always_pref (Korea)\n")
tertile_results <- list()
for (grp in c("Top tertile", "Rest")) {
  sub <- kr |> filter(polint_top_tertile == grp)
  f <- as.formula(paste("qual_pref_dem_n ~ econ_index + factor(wave) +", controls_no_polint))
  m <- lm(f, data = sub)
  ec <- extract_coef(m, "econ_index")
  tertile_results[[grp]] <- ec |> mutate(group = grp, n = nobs(m))
  cat(sprintf("  %s (n=%d): b=%.4f (SE=%.4f), p=%.4f %s\n",
              grp, nobs(m), ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value)))
}

# 4b: Quartile split
cat("\n4b: Quartile split on dem_always_pref (Korea)\n")
quartile_results <- list()
for (grp in c("Top quartile", "Rest")) {
  sub <- kr |> filter(polint_top_quartile == grp)
  f <- as.formula(paste("qual_pref_dem_n ~ econ_index + factor(wave) +", controls_no_polint))
  m <- lm(f, data = sub)
  ec <- extract_coef(m, "econ_index")
  quartile_results[[grp]] <- ec |> mutate(group = grp, n = nobs(m))
  cat(sprintf("  %s (n=%d): b=%.4f (SE=%.4f), p=%.4f %s\n",
              grp, nobs(m), ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value)))
}

# 4c: Continuous interaction — dem_always_pref
cat("\n4c: Continuous interaction — dem_always_pref ~ econ x polint (Korea)\n")
f_cont_pref <- as.formula(paste("qual_pref_dem_n ~ econ_index * polint_n + factor(wave) +", controls_no_polint))
m_cont_pref <- lm(f_cont_pref, data = kr)
pref_tidy <- tidy_hc2(m_cont_pref) |>
  filter(term %in% c("econ_index", "polint_n", "econ_index:polint_n"))
for (i in seq_len(nrow(pref_tidy))) {
  cat(sprintf("  %-25s b=%.4f (SE=%.4f), p=%.4f %s\n",
              pref_tidy$term[i], pref_tidy$estimate[i], pref_tidy$std.error[i],
              pref_tidy$p.value[i], sig_stars(pref_tidy$p.value[i])))
}

# 4d: Continuous interaction — auth_reject_index
cat("\n4d: Continuous interaction — auth_reject_index ~ econ x polint (Korea)\n")
f_cont_auth <- as.formula(paste("auth_reject_index ~ econ_index * polint_n + factor(wave) +", controls_no_polint))
m_cont_auth <- lm(f_cont_auth, data = kr)
auth_tidy <- tidy_hc2(m_cont_auth) |>
  filter(term %in% c("econ_index", "polint_n", "econ_index:polint_n"))
for (i in seq_len(nrow(auth_tidy))) {
  cat(sprintf("  %-25s b=%.4f (SE=%.4f), p=%.4f %s\n",
              auth_tidy$term[i], auth_tidy$estimate[i], auth_tidy$std.error[i],
              auth_tidy$p.value[i], sig_stars(auth_tidy$p.value[i])))
}

task4_results <- list(
  tertile = bind_rows(tertile_results),
  quartile = bind_rows(quartile_results),
  continuous_pref = pref_tidy,
  continuous_auth = auth_tidy,
  model_pref = m_cont_pref,
  model_auth = m_cont_auth
)
saveRDS(task4_results, file.path(results_dir, "r2_3_cutpoints.rds"))
cat("\nSaved r2_3_cutpoints.rds\n\n")


# =============================================================================
# TASK 5: Political Discussion Frequency
# =============================================================================
cat("━━━ TASK 5: Political Discussion Frequency ━━━\n\n")

cat("pol_discuss coverage by wave (Korea):\n")
kr |> group_by(wave) |>
  summarise(n = n(), discuss_ok = sum(!is.na(discuss_n)), .groups = "drop") |>
  print()

# 5a: Continuous interaction — dem_pref
cat("\n5a: dem_always_pref ~ econ x discuss (Korea)\n")
f_disc_pref <- as.formula(paste("qual_pref_dem_n ~ econ_index * discuss_n + factor(wave) +", controls_str))
m_disc_pref <- lm(f_disc_pref, data = kr)
disc_pref_tidy <- tidy_hc2(m_disc_pref) |>
  filter(term %in% c("econ_index", "discuss_n", "econ_index:discuss_n"))
for (i in seq_len(nrow(disc_pref_tidy))) {
  cat(sprintf("  %-25s b=%.4f (SE=%.4f), p=%.4f %s\n",
              disc_pref_tidy$term[i], disc_pref_tidy$estimate[i], disc_pref_tidy$std.error[i],
              disc_pref_tidy$p.value[i], sig_stars(disc_pref_tidy$p.value[i])))
}

# 5b: Continuous interaction — auth_reject
cat("\n5b: auth_reject_index ~ econ x discuss (Korea)\n")
f_disc_auth <- as.formula(paste("auth_reject_index ~ econ_index * discuss_n + factor(wave) +", controls_str))
m_disc_auth <- lm(f_disc_auth, data = kr)
disc_auth_tidy <- tidy_hc2(m_disc_auth) |>
  filter(term %in% c("econ_index", "discuss_n", "econ_index:discuss_n"))
for (i in seq_len(nrow(disc_auth_tidy))) {
  cat(sprintf("  %-25s b=%.4f (SE=%.4f), p=%.4f %s\n",
              disc_auth_tidy$term[i], disc_auth_tidy$estimate[i], disc_auth_tidy$std.error[i],
              disc_auth_tidy$p.value[i], sig_stars(disc_auth_tidy$p.value[i])))
}

# 5c: Binary split
cat("\n5c: Binary split — high vs low discussion (Korea)\n")
kr <- kr |>
  mutate(discuss_group = case_when(
    discuss_n > median(discuss_n, na.rm = TRUE) ~ "High discussion",
    discuss_n <= median(discuss_n, na.rm = TRUE) ~ "Low discussion"
  ))

discuss_split <- list()
for (grp in c("High discussion", "Low discussion")) {
  sub <- kr |> filter(discuss_group == grp)
  if (nrow(sub) < 200) { cat(sprintf("  %s: n=%d (too small)\n", grp, nrow(sub))); next }
  f <- as.formula(paste("qual_pref_dem_n ~ econ_index + factor(wave) +", controls_str))
  m <- lm(f, data = sub)
  ec <- extract_coef(m, "econ_index")
  discuss_split[[grp]] <- ec |> mutate(group = grp, n = nobs(m))
  cat(sprintf("  %s (n=%d): b=%.4f (SE=%.4f), p=%.4f %s\n",
              grp, nobs(m), ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value)))
}

task5_results <- list(
  continuous_pref = disc_pref_tidy,
  continuous_auth = disc_auth_tidy,
  binary_split = bind_rows(discuss_split),
  model_pref = m_disc_pref,
  model_auth = m_disc_auth
)
saveRDS(task5_results, file.path(results_dir, "r2_3_discussion.rds"))
cat("\nSaved r2_3_discussion.rds\n\n")


# =============================================================================
# TASK 6: Composite Political Engagement Index
# =============================================================================
cat("━━━ TASK 6: Composite Political Engagement Index ━━━\n\n")

# Construct composite: mean of polint_n, discuss_n, news_follow_n
kr <- kr |>
  rowwise() |>
  mutate(
    engagement_index = mean(c_across(c(polint_n, discuss_n, news_follow_n)), na.rm = TRUE)
  ) |>
  ungroup()

# Also for full dataset
dat <- dat |>
  rowwise() |>
  mutate(
    engagement_index = mean(c_across(c(polint_n, discuss_n, news_follow_n)), na.rm = TRUE)
  ) |>
  ungroup()

cat("Engagement index (Korea): mean=%.3f, SD=%.3f, n=%d\n" |>
      sprintf(mean(kr$engagement_index, na.rm = TRUE),
              sd(kr$engagement_index, na.rm = TRUE),
              sum(!is.na(kr$engagement_index))))
cat("Components: polint_n, discuss_n, news_follow_n\n")
cat("Pairwise correlations (Korea):\n")
kr_eng <- kr |> select(polint_n, discuss_n, news_follow_n) |> drop_na()
print(round(cor(kr_eng), 3))

# 6a: Continuous interaction — dem_pref
cat("\n6a: dem_always_pref ~ econ x engagement (Korea)\n")
f_eng_pref <- as.formula(paste("qual_pref_dem_n ~ econ_index * engagement_index + factor(wave) +", controls_no_polint))
m_eng_pref <- lm(f_eng_pref, data = kr)
eng_pref_tidy <- tidy_hc2(m_eng_pref) |>
  filter(term %in% c("econ_index", "engagement_index", "econ_index:engagement_index"))
for (i in seq_len(nrow(eng_pref_tidy))) {
  cat(sprintf("  %-35s b=%.4f (SE=%.4f), p=%.4f %s\n",
              eng_pref_tidy$term[i], eng_pref_tidy$estimate[i], eng_pref_tidy$std.error[i],
              eng_pref_tidy$p.value[i], sig_stars(eng_pref_tidy$p.value[i])))
}

# 6b: Continuous interaction — auth_reject
cat("\n6b: auth_reject_index ~ econ x engagement (Korea)\n")
f_eng_auth <- as.formula(paste("auth_reject_index ~ econ_index * engagement_index + factor(wave) +", controls_no_polint))
m_eng_auth <- lm(f_eng_auth, data = kr)
eng_auth_tidy <- tidy_hc2(m_eng_auth) |>
  filter(term %in% c("econ_index", "engagement_index", "econ_index:engagement_index"))
for (i in seq_len(nrow(eng_auth_tidy))) {
  cat(sprintf("  %-35s b=%.4f (SE=%.4f), p=%.4f %s\n",
              eng_auth_tidy$term[i], eng_auth_tidy$estimate[i], eng_auth_tidy$std.error[i],
              eng_auth_tidy$p.value[i], sig_stars(eng_auth_tidy$p.value[i])))
}

# 6c: Binary split at median
cat("\n6c: Binary split at engagement median (Korea)\n")
kr <- kr |>
  mutate(engage_group = case_when(
    engagement_index > median(engagement_index, na.rm = TRUE) ~ "High engagement",
    engagement_index <= median(engagement_index, na.rm = TRUE) ~ "Low engagement"
  ))

engage_split <- list()
for (grp in c("High engagement", "Low engagement")) {
  sub <- kr |> filter(engage_group == grp)
  if (nrow(sub) < 200) next

  for (dv_pair in list(
    c("qual_pref_dem_n", "dem_always_pref"),
    c("auth_reject_index", "auth_reject")
  )) {
    f <- as.formula(paste(dv_pair[1], "~ econ_index + factor(wave) +", controls_no_polint))
    m <- lm(f, data = sub)
    ec <- extract_coef(m, "econ_index")
    engage_split[[paste(grp, dv_pair[2])]] <- ec |>
      mutate(group = grp, dv = dv_pair[2], n = nobs(m))
    cat(sprintf("  %s | %s (n=%d): b=%.4f (SE=%.4f), p=%.4f %s\n",
                grp, dv_pair[2], nobs(m), ec$estimate, ec$std.error, ec$p.value, sig_stars(ec$p.value)))
  }
}

task6_results <- list(
  continuous_pref = eng_pref_tidy,
  continuous_auth = eng_auth_tidy,
  binary_split = bind_rows(engage_split),
  component_cors = cor(kr_eng),
  model_pref = m_eng_pref,
  model_auth = m_eng_auth
)
saveRDS(task6_results, file.path(results_dir, "r2_3_composite.rds"))
cat("\nSaved r2_3_composite.rds\n\n")


# =============================================================================
# TASK 7: Engaged Minority Proportions by Wave
# =============================================================================
cat("━━━ TASK 7: Engaged Minority Proportions by Wave ━━━\n\n")

yr_kr <- c(`1`=2003, `2`=2006, `3`=2011, `4`=2015, `5`=2019, `6`=2022)
yr_tw <- c(`1`=2001, `2`=2006, `3`=2010, `4`=2014, `5`=2019, `6`=2022)

# Compute for Korea
kr_props <- kr |>
  group_by(wave) |>
  summarise(
    year = yr_kr[as.character(first(wave))],
    n_total = n(),
    # (i) Original binary polint split
    n_high_polint = sum(polint_group == "High interest", na.rm = TRUE),
    pct_high_polint = round(100 * n_high_polint / n_total, 1),
    # (ii) Top tertile
    n_top_tert = sum(polint_top_tertile == "Top tertile", na.rm = TRUE),
    pct_top_tert = round(100 * n_top_tert / n_total, 1),
    # (iii) Top quartile
    n_top_quart = sum(polint_top_quartile == "Top quartile", na.rm = TRUE),
    pct_top_quart = round(100 * n_top_quart / n_total, 1),
    # (iv) Composite above median
    n_high_engage = sum(engage_group == "High engagement", na.rm = TRUE),
    pct_high_engage = round(100 * n_high_engage / n_total, 1),
    .groups = "drop"
  ) |>
  mutate(country = "Korea")

cat("Korea — Engaged minority proportions by wave:\n")
cat(sprintf("  %-6s %5s %6s  %10s  %10s  %10s  %10s\n",
            "Wave", "Year", "N", "High PI%", "Top tert%", "Top Q%", "High eng%"))
cat(paste(rep("-", 75), collapse = ""), "\n")
for (i in seq_len(nrow(kr_props))) {
  cat(sprintf("  W%-4d %5d %6d  %9.1f%%  %9.1f%%  %9.1f%%  %9.1f%%\n",
              kr_props$wave[i], kr_props$year[i], kr_props$n_total[i],
              kr_props$pct_high_polint[i], kr_props$pct_top_tert[i],
              kr_props$pct_top_quart[i], kr_props$pct_high_engage[i]))
}

# Taiwan comparison (polint binary only)
tw <- dat |> filter(country_label == "Taiwan") |>
  mutate(polint_group_tw = case_when(
    political_interest > median(political_interest, na.rm = TRUE) ~ "High interest",
    political_interest <= median(political_interest, na.rm = TRUE) ~ "Low interest"
  ))

tw_props <- tw |>
  group_by(wave) |>
  summarise(
    year = yr_tw[as.character(first(wave))],
    n_total = n(),
    n_high_polint = sum(polint_group_tw == "High interest", na.rm = TRUE),
    pct_high_polint = round(100 * n_high_polint / n_total, 1),
    .groups = "drop"
  ) |>
  mutate(country = "Taiwan")

cat("\nTaiwan — High political interest by wave:\n")
for (i in seq_len(nrow(tw_props))) {
  cat(sprintf("  W%-4d %5d  n=%5d  High PI: %5.1f%%\n",
              tw_props$wave[i], tw_props$year[i], tw_props$n_total[i], tw_props$pct_high_polint[i]))
}

task7_results <- list(korea = kr_props, taiwan = tw_props)
saveRDS(task7_results, file.path(results_dir, "r2_3_proportions.rds"))
cat("\nSaved r2_3_proportions.rds\n\n")


# =============================================================================
# TASK 8: Engaged vs. Disengaged Demographic Profile
# =============================================================================
cat("━━━ TASK 8: Engaged vs. Disengaged Demographic Profile ━━━\n\n")

demo_compare <- function(data, group_var) {
  data |>
    group_by(!!sym(group_var)) |>
    summarise(
      n = n(),
      mean_age = mean(age_n, na.rm = TRUE),
      pct_female = mean(gender == 0, na.rm = TRUE) * 100,
      mean_edu = mean(edu_n, na.rm = TRUE),
      pct_urban = mean(urban_rural == 1, na.rm = TRUE) * 100,
      mean_econ = mean(econ_index, na.rm = TRUE),
      mean_dem_pref = mean(qual_pref_dem_n, na.rm = TRUE),
      mean_auth_reject = mean(auth_reject_index, na.rm = TRUE),
      mean_dem_sat = mean(sat_democracy_n, na.rm = TRUE),
      .groups = "drop"
    )
}

# Pooled
kr_demo <- demo_compare(kr, "polint_group")
cat("Pooled demographics (Korea, by polint_group):\n")
print(kr_demo)

# T-tests
cat("\nDifference tests (Welch t-tests):\n")
hi <- kr |> filter(polint_group == "High interest")
lo <- kr |> filter(polint_group == "Low interest")

test_vars <- c("age_n", "edu_n", "econ_index", "qual_pref_dem_n",
               "auth_reject_index", "sat_democracy_n")
test_labels <- c("Age (0-1)", "Education (0-1)", "Econ index",
                 "Dem always pref", "Auth reject index", "Dem satisfaction")

demo_tests <- list()
for (i in seq_along(test_vars)) {
  tt <- t.test(hi[[test_vars[i]]], lo[[test_vars[i]]])
  demo_tests[[test_vars[i]]] <- tibble(
    variable = test_labels[i],
    mean_high = tt$estimate[1], mean_low = tt$estimate[2],
    diff = tt$estimate[1] - tt$estimate[2],
    t_stat = tt$statistic, p_value = tt$p.value
  )
  cat(sprintf("  %-20s  High=%.3f  Low=%.3f  diff=%.3f  t=%.2f  p=%.4f\n",
              test_labels[i], tt$estimate[1], tt$estimate[2],
              tt$estimate[1] - tt$estimate[2], tt$statistic, tt$p.value))
}

# Gender (chi-square)
gender_tab <- table(kr$polint_group, kr$gender)
chi <- chisq.test(gender_tab)
cat(sprintf("  %-20s  High=%.1f%%  Low=%.1f%%  chi2=%.2f  p=%.4f\n",
            "% female",
            100 * mean(hi$gender == 0, na.rm = TRUE),
            100 * mean(lo$gender == 0, na.rm = TRUE),
            chi$statistic, chi$p.value))

# Urban (chi-square)
urban_tab <- table(kr$polint_group, kr$urban_rural)
chi_u <- chisq.test(urban_tab)
cat(sprintf("  %-20s  High=%.1f%%  Low=%.1f%%  chi2=%.2f  p=%.4f\n",
            "% urban",
            100 * mean(hi$urban_rural == 1, na.rm = TRUE),
            100 * mean(lo$urban_rural == 1, na.rm = TRUE),
            chi_u$statistic, chi_u$p.value))

# Wave 1 vs Wave 6 comparison
cat("\nWave 1 vs Wave 6 breakdown:\n")
for (w in c(1, 6)) {
  cat(sprintf("\n  Wave %d:\n", w))
  sub_w <- kr |> filter(wave == w)
  demo_w <- demo_compare(sub_w, "polint_group")
  print(demo_w)
}

task8_results <- list(
  pooled = kr_demo,
  tests = bind_rows(demo_tests),
  gender_chi = chi,
  urban_chi = chi_u
)
saveRDS(task8_results, file.path(results_dir, "r2_3_demographics.rds"))
cat("\nSaved r2_3_demographics.rds\n\n")


# =============================================================================
# TASK 9: Item-Total Correlations
# =============================================================================
cat("━━━ TASK 9: Item-Total Correlations ━━━\n\n")

# dem_always_pref × auth_reject_index by country and wave
cat("dem_always_pref x auth_reject_index:\n\n")
cat(sprintf("  %-8s  %8s  %8s  %8s  %8s\n", "Wave", "Korea r", "Korea p", "Taiwan r", "Taiwan p"))
cat(paste(rep("-", 50), collapse = ""), "\n")

corr_results <- list()
for (w in c(1:6, 0)) {  # 0 = pooled
  for (cntry in c("Korea", "Taiwan")) {
    sub <- dat |> filter(country_label == cntry)
    if (w > 0) sub <- sub |> filter(wave == w)
    sub <- sub |> filter(!is.na(qual_pref_dem_n), !is.na(auth_reject_index))
    if (nrow(sub) < 30) next
    ct <- cor.test(sub$qual_pref_dem_n, sub$auth_reject_index)
    corr_results[[paste(cntry, w)]] <- tibble(
      country = cntry, wave = if (w == 0) "Pooled" else as.character(w),
      r = ct$estimate, p = ct$p.value, n = nrow(sub)
    )
  }
  if (w > 0) {
    kr_r <- corr_results[[paste("Korea", w)]]
    tw_r <- corr_results[[paste("Taiwan", w)]]
    if (!is.null(kr_r) && !is.null(tw_r)) {
      cat(sprintf("  W%-6d  %8.3f  %8.4f  %8.3f  %8.4f\n",
                  w, kr_r$r, kr_r$p, tw_r$r, tw_r$p))
    }
  }
}
kr_pool <- corr_results[["Korea 0"]]
tw_pool <- corr_results[["Taiwan 0"]]
cat(sprintf("  %-8s  %8.3f  %8.4f  %8.3f  %8.4f\n",
            "Pooled", kr_pool$r, kr_pool$p, tw_pool$r, tw_pool$p))

# dem_always_pref × satisfaction items
cat("\ndem_always_pref x satisfaction items (pooled):\n")
sat_corrs <- list()
for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |> filter(country_label == cntry)
  for (sat_var in c("sat_democracy_n", "sat_govt_n")) {
    sub_ok <- sub |> filter(!is.na(qual_pref_dem_n), !is.na(.data[[sat_var]]))
    ct <- cor.test(sub_ok$qual_pref_dem_n, sub_ok[[sat_var]])
    sat_corrs[[paste(cntry, sat_var)]] <- tibble(
      country = cntry, variable = sat_var,
      r = ct$estimate, p = ct$p.value, n = nrow(sub_ok)
    )
    cat(sprintf("  %s | %s: r=%.3f, p=%.4f, n=%d\n",
                cntry, sat_var, ct$estimate, ct$p.value, nrow(sub_ok)))
  }
}

task9_results <- list(
  pref_auth_corr = bind_rows(corr_results),
  pref_sat_corr = bind_rows(sat_corrs)
)
saveRDS(task9_results, file.path(results_dir, "r2_6_correlations.rds"))
cat("\nSaved r2_6_correlations.rds\n\n")


# =============================================================================
# TASK 10: Factor Loading Comparison by Country
# =============================================================================
cat("━━━ TASK 10: Factor Loadings by Country ━━━\n\n")

efa_items <- c("sat_democracy_n", "sat_govt_n", "qual_pref_dem_n",
               "strongman_reject_n", "military_reject_n",
               "expert_reject_n", "singleparty_reject_n")
efa_labels <- c("Dem satisfaction", "Gov't satisfaction", "Dem always pref",
                "Reject strongman", "Reject military", "Reject expert", "Reject single-party")

efa_by_country <- list()
for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |>
    filter(country_label == cntry) |>
    select(all_of(efa_items)) |>
    drop_na()

  cat(sprintf("%s: n=%d\n", cntry, nrow(sub)))

  efa <- fa(sub, nfactors = 2, rotate = "oblimin", fm = "ml")
  loadings_mat <- as.data.frame(unclass(efa$loadings))
  loadings_mat$item <- efa_labels
  loadings_mat$communality <- efa$communalities

  efa_by_country[[cntry]] <- list(
    efa = efa, loadings = loadings_mat, n = nrow(sub)
  )

  cat(sprintf("  RMSEA=%.3f, TLI=%.3f, Factor correlation=%.3f\n",
              efa$RMSEA[1], efa$TLI, efa$Phi[1, 2]))
  cat(sprintf("  %-22s  %8s  %8s  %8s\n", "Item", "Factor 1", "Factor 2", "h2"))
  cat(paste(rep("-", 55), collapse = ""), "\n")
  for (i in seq_len(nrow(loadings_mat))) {
    cat(sprintf("  %-22s  %8.3f  %8.3f  %8.3f\n",
                loadings_mat$item[i],
                loadings_mat$ML1[i], loadings_mat$ML2[i],
                loadings_mat$communality[i]))
  }
  cat("\n")
}

task10_results <- efa_by_country
saveRDS(task10_results, file.path(results_dir, "r2_6_factor_loadings.rds"))
cat("Saved r2_6_factor_loadings.rds\n\n")


# =============================================================================
# TASK 11: Response Distributions for "Always Preferable"
# =============================================================================
cat("━━━ TASK 11: Response Distributions — dem_always_preferable ━━━\n\n")

# dem_always_preferable: 1 = always, 2 = sometimes auth, 3 = doesn't matter
dist_results <- dat |>
  filter(!is.na(dem_always_preferable)) |>
  group_by(country_label, wave) |>
  summarise(
    n = n(),
    pct_always = round(100 * mean(dem_always_preferable == 1), 1),
    pct_sometimes = round(100 * mean(dem_always_preferable == 2), 1),
    pct_indifferent = round(100 * mean(dem_always_preferable == 3), 1),
    .groups = "drop"
  )

cat(sprintf("  %-8s  %-6s  %5s  %10s  %12s  %12s\n",
            "Country", "Wave", "N", "Always%", "Sometimes%", "Indifferent%"))
cat(paste(rep("-", 65), collapse = ""), "\n")
for (i in seq_len(nrow(dist_results))) {
  cat(sprintf("  %-8s  W%-5d  %5d  %9.1f%%  %11.1f%%  %11.1f%%\n",
              dist_results$country_label[i], dist_results$wave[i],
              dist_results$n[i], dist_results$pct_always[i],
              dist_results$pct_sometimes[i], dist_results$pct_indifferent[i]))
}

# Pooled
dist_pooled <- dat |>
  filter(!is.na(dem_always_preferable)) |>
  group_by(country_label) |>
  summarise(
    n = n(),
    pct_always = round(100 * mean(dem_always_preferable == 1), 1),
    pct_sometimes = round(100 * mean(dem_always_preferable == 2), 1),
    pct_indifferent = round(100 * mean(dem_always_preferable == 3), 1),
    .groups = "drop"
  )
cat("\nPooled:\n")
print(dist_pooled)

task11_results <- list(by_wave = dist_results, pooled = dist_pooled)
saveRDS(task11_results, file.path(results_dir, "r2_6_distributions.rds"))
cat("\nSaved r2_6_distributions.rds\n\n")


# =============================================================================
# TASK 12: Conditional Correlation Matrix
# =============================================================================
cat("━━━ TASK 12: Correlation Matrices by Country ━━━\n\n")

corr_items <- c("qual_pref_dem_n", "strongman_reject_n", "military_reject_n",
                "expert_reject_n", "singleparty_reject_n",
                "sat_democracy_n", "sat_govt_n")
corr_labels <- c("Dem always pref", "Rej strongman", "Rej military",
                  "Rej expert", "Rej single-party",
                  "Sat democracy", "Sat gov't")

corr_matrices <- list()
for (cntry in c("Korea", "Taiwan")) {
  sub <- dat |>
    filter(country_label == cntry) |>
    select(all_of(corr_items)) |>
    drop_na()

  cm <- cor(sub)
  rownames(cm) <- corr_labels
  colnames(cm) <- corr_labels
  corr_matrices[[cntry]] <- cm

  cat(sprintf("%s (n=%d):\n", cntry, nrow(sub)))
  print(round(cm, 3))
  cat("\n")
}

# Differences
cat("Cross-country differences (Korea - Taiwan):\n")
diff_mat <- corr_matrices[["Korea"]] - corr_matrices[["Taiwan"]]
print(round(diff_mat, 3))

task12_results <- list(
  korea = corr_matrices[["Korea"]],
  taiwan = corr_matrices[["Taiwan"]],
  difference = diff_mat
)
saveRDS(task12_results, file.path(results_dir, "r2_6_corr_matrix.rds"))
cat("\nSaved r2_6_corr_matrix.rds\n\n")


# =============================================================================
# TASK 13: Survey Weights Verification
# =============================================================================
cat("━━━ TASK 13: Survey Weights Verification ━━━\n\n")

cat("Sample demographics by wave (Korea):\n\n")
weight_check <- dat |>
  group_by(country_label, wave) |>
  summarise(
    n = n(),
    mean_age_raw = mean(age, na.rm = TRUE),
    pct_female = round(100 * mean(gender == 0, na.rm = TRUE), 1),
    mean_edu_raw = mean(education_level, na.rm = TRUE),
    pct_urban = round(100 * mean(urban_rural == 1, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat(sprintf("  %-8s  %-5s  %5s  %8s  %8s  %8s  %8s\n",
            "Country", "Wave", "N", "MeanAge", "%Female", "MeanEdu", "%Urban"))
cat(paste(rep("-", 60), collapse = ""), "\n")
for (i in seq_len(nrow(weight_check))) {
  cat(sprintf("  %-8s  W%-4d  %5d  %8.1f  %7.1f%%  %8.2f  %7.1f%%\n",
              weight_check$country_label[i], weight_check$wave[i],
              weight_check$n[i], weight_check$mean_age_raw[i],
              weight_check$pct_female[i], weight_check$mean_edu_raw[i],
              weight_check$pct_urban[i]))
}

cat("\nNote: ABS uses multi-stage stratified probability sampling with quotas on")
cat("\ngender, age, and geographic region. Technical reports for each wave document")
cat("\nsampling frame and post-stratification procedures. Formal census-margin")
cat("\ncomparisons require external census data for each survey year.\n")

task13_results <- weight_check
saveRDS(task13_results, file.path(results_dir, "r2_8_weights.rds"))
cat("\nSaved r2_8_weights.rds\n\n")


# =============================================================================
# TASK 14: Taiwan Identity Variable (CONDITIONAL)
# =============================================================================
cat("━━━ TASK 14: Taiwan Identity Variable (CONDITIONAL) ━━━\n\n")

# Check if Taiwan country-specific module data exists
tw_module_paths <- c(
  file.path(project_root, "data/external/abs_taiwan_module.rds"),
  file.path(project_root, "data/processed/abs_taiwan_identity.rds")
)

tw_identity_found <- FALSE
for (p in tw_module_paths) {
  if (file.exists(p)) {
    cat("Found Taiwan module data at:", p, "\n")
    tw_identity_found <- TRUE
    break
  }
}

# Also check harmonized data for any identity variable
tw_ident_vars <- names(abs_all)[grepl("taiwan|self_id|ethnic_id|identity_excl", names(abs_all), ignore.case = TRUE)]
if (length(tw_ident_vars) > 0) {
  cat("Potential identity variables in harmonized data:", paste(tw_ident_vars, collapse = ", "), "\n")
} else {
  cat("No Taiwan identity variable found in harmonized data.\n")
}

if (!tw_identity_found && length(tw_ident_vars) == 0) {
  cat("Taiwan country-specific identity data not available. Skipping Task 14.\n")
  cat("To run this analysis, the Taiwan module data with the Taiwanese/Chinese identity\n")
  cat("spectrum item needs to be added to survey-data-prep.\n")
} else {
  cat("Identity variable available — analysis would go here.\n")
}

cat("\n=== All R2.3 + R2.6 analyses complete ===\n")
