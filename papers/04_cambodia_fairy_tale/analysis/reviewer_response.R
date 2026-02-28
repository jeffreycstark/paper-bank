#!/usr/bin/env Rscript
# reviewer_response.R — Task 9: Statistical upgrades and adjudication checks
# Output dir: analysis/reviewer_response/
# Cambodia only, Waves 2/3/4/6

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/04_cambodia_fairy_tale")
results_dir  <- file.path(paper_dir, "analysis/results")
fig_dir      <- file.path(paper_dir, "analysis/figures")
rr_dir       <- file.path(paper_dir, "analysis/reviewer_response")

dir.create(rr_dir, showWarnings = FALSE, recursive = TRUE)

source(file.path(project_root, "_data_config.R"))
source(file.path(paper_dir, "R", "helpers.R"))

cat("Loading ABS...\n")
abs_all <- readRDS(abs_harmonized_path)

gate_vars <- c("gate_contact_elected", "gate_contact_civil_servant",
               "gate_contact_influential", "gate_petition",
               "gate_demonstration", "gate_contact_media")

dat <- abs_all |>
  filter(country == 12, wave %in% c(2, 3, 4, 6)) |>
  mutate(
    year = case_when(
      wave == 2 ~ 2008, wave == 3 ~ 2012,
      wave == 4 ~ 2015, wave == 6 ~ 2021   # W6 field year 2021
    ),
    across(all_of(gate_vars), ~ as.numeric(as.character(.x))),
    # Subgroup variables
    urban_label = if_else(urban_rural == 1, "Urban", "Rural"),
    age_group   = case_when(
      age < 30  ~ "Under 30",
      age < 50  ~ "30-49",
      age >= 50 ~ "50+",
      TRUE      ~ NA_character_
    ),
    edu_group   = case_when(
      education_level %in% 1:3 ~ "Primary or below",
      education_level %in% 4:7 ~ "Secondary",
      education_level %in% 8:10 ~ "Tertiary",
      TRUE ~ NA_character_
    )
  )

cat("N per wave:\n"); print(table(dat$wave))

# ── CI helpers ─────────────────────────────────────────────────────────────────

# Wilson interval for binary proportions (x is 0/1 vector)
wilson_ci <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, valid_n = 0L))
  k   <- sum(x)
  p   <- k / n
  se  <- sqrt(p * (1 - p) / n)
  res <- suppressWarnings(prop.test(k, n, conf.level = conf))
  list(estimate = p, se = se,
       ci_lower = res$conf.int[1], ci_upper = res$conf.int[2],
       valid_n = as.integer(n))
}

# t-based CI for scale means
mean_ci <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA, valid_n = as.integer(n)))
  m      <- mean(x)
  se     <- sd(x) / sqrt(n)
  t_crit <- qt((1 + conf) / 2, df = n - 1)
  list(estimate = m, se = se,
       ci_lower = m - t_crit * se, ci_upper = m + t_crit * se,
       valid_n  = as.integer(n))
}

# ─────────────────────────────────────────────────────────────────────────────
# 9a.  Uncertainty estimates for all Table 1-3 / inline variables
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n=== 9a. Uncertainty estimates ===\n")

# Variable definitions: type = "prop" (binary 0/1) or "mean" (scale)
# scale_mult applies to proportions stored as raw 0/1 where we want to
# keep estimates on the 0-1 scale here (rounding in prose is caller's job).

var_meta <- tribble(
  ~variable,                    ~label,                          ~type,
  "gate_contact_elected",       "Contacted elected official",    "prop",
  "gate_contact_civil_servant", "Contacted civil servant",       "prop",
  "gate_contact_influential",   "Contacted influential person",  "prop",
  "gate_petition",              "Signed petition",               "prop",
  "gate_demonstration",         "Attended demonstration",        "prop",
  "gate_contact_media",         "Contacted media",               "prop",
  "community_leader_contact",   "Community leader contact",      "mean",
  "voted_last_election",        "Voted in last election",        "prop",
  "expert_rule",                "Expert rule",                   "mean",
  "single_party_rule",          "Single-party rule",             "mean",
  "strongman_rule",             "Strongman rule",                "mean",
  "military_rule",              "Military rule",                 "mean",
  "dem_country_future",         "Democratic future (0-10)",      "mean",
  "dem_country_past",           "Democratic past (0-10)",        "mean",
  "dem_country_present_govt",   "Democratic present (0-10)",     "mean",
  "corrupt_witnessed",          "Witnessed corruption",          "prop",
  "corrupt_national_govt",      "National govt corruption",      "mean",
  "corrupt_local_govt",         "Local govt corruption",         "mean",
  "pol_news_follow",            "Follows political news",        "mean",
  "news_internet",              "Internet news (harmonized)",    "mean",
  "political_interest",         "Political interest",            "mean",
  "pol_discuss",                "Discusses politics",            "mean"
)

ue_rows <- list()
for (w in c(2, 3, 4, 6)) {
  dw <- dat |> filter(wave == w)
  for (i in seq_len(nrow(var_meta))) {
    v    <- var_meta$variable[i]
    lbl  <- var_meta$label[i]
    typ  <- var_meta$type[i]
    if (!v %in% names(dw)) next
    vals <- dw[[v]]
    ci   <- if (typ == "prop") wilson_ci(vals) else mean_ci(vals)
    ue_rows[[length(ue_rows) + 1]] <- tibble(
      variable  = v,
      label     = lbl,
      type      = typ,
      wave      = w,
      estimate  = ci$estimate,
      se        = ci$se,
      ci_lower  = ci$ci_lower,
      ci_upper  = ci$ci_upper,
      valid_n   = ci$valid_n
    )
  }
}

uncertainty_estimates <- bind_rows(ue_rows)
saveRDS(uncertainty_estimates, file.path(rr_dir, "uncertainty_estimates.rds"))
write.csv(uncertainty_estimates, file.path(rr_dir, "uncertainty_estimates.csv"), row.names = FALSE)
cat("Saved uncertainty_estimates (.rds + .csv)\n")
cat(sprintf("  Rows: %d  (variables x waves)\n", nrow(uncertainty_estimates)))

# ── 9a. Headline difference tests ─────────────────────────────────────────────
cat("\n=== 9a. Headline difference tests ===\n")

headline_vars <- tribble(
  ~variable,                ~label,             ~type,
  "dem_country_future",     "Dem future",       "mean",
  "gate_contact_influential","Gate influential", "prop",
  "corrupt_witnessed",      "Corrupt witnessed","prop",
  "voted_last_election",    "Voted",            "prop"
)

diff_rows <- list()
wave_pairs <- list(c(3, 4), c(4, 6))

for (wp in wave_pairs) {
  wa <- wp[1]; wb <- wp[2]
  da <- dat |> filter(wave == wa)
  db <- dat |> filter(wave == wb)
  for (i in seq_len(nrow(headline_vars))) {
    v   <- headline_vars$variable[i]
    lbl <- headline_vars$label[i]
    typ <- headline_vars$type[i]
    xa  <- da[[v]]; xa <- xa[!is.na(xa)]
    xb  <- db[[v]]; xb <- xb[!is.na(xb)]
    if (length(xa) < 5 || length(xb) < 5) next

    if (typ == "mean") {
      res      <- t.test(xb, xa)
      estimate_a <- mean(xa)
      estimate_b <- mean(xb)
      test_type  <- "two-sample t-test"
    } else {
      na_  <- sum(xa); nb_  <- sum(xb)
      res  <- prop.test(c(nb_, na_), c(length(xb), length(xa)))
      estimate_a <- mean(xa)
      estimate_b <- mean(xb)
      test_type  <- "two-sample proportion test"
    }

    diff_rows[[length(diff_rows) + 1]] <- tibble(
      variable      = v,
      label         = lbl,
      wave_from     = wa,
      wave_to       = wb,
      estimate_from = estimate_a,
      estimate_to   = estimate_b,
      difference    = estimate_b - estimate_a,
      statistic     = res$statistic,
      p_value       = res$p.value,
      test_type     = test_type,
      n_from        = length(xa),
      n_to          = length(xb)
    )
  }
}

difference_tests <- bind_rows(diff_rows)
write.csv(difference_tests, file.path(rr_dir, "difference_tests.csv"), row.names = FALSE)
cat("Saved difference_tests.csv\n")
print(difference_tests |> select(label, wave_from, wave_to, estimate_from, estimate_to, difference, p_value))

# ─────────────────────────────────────────────────────────────────────────────
# 9b. Response-style and missingness diagnostics
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n=== 9b. Response-style diagnostics ===\n")

# In the harmonized ABS data, DK/refused are coded as NA.
# NA rates therefore proxy item nonresponse (DK + refused + legitimately skipped).

political_items <- c(
  "gate_contact_elected", "gate_contact_civil_servant",
  "gate_contact_influential", "gate_petition",
  "gate_demonstration", "gate_contact_media",
  "community_leader_contact", "voted_last_election",
  "expert_rule", "single_party_rule", "strongman_rule", "military_rule",
  "dem_country_future", "dem_country_past", "dem_country_present_govt",
  "corrupt_witnessed", "corrupt_national_govt", "corrupt_local_govt",
  "pol_news_follow", "news_internet", "political_interest", "pol_discuss"
)

dem_expect_items <- c("dem_country_future", "dem_country_past", "dem_country_present_govt")
auth_items_all   <- c("expert_rule", "single_party_rule", "strongman_rule", "military_rule")
auth_items_w2    <- c("single_party_rule", "strongman_rule", "military_rule")  # expert_rule NA in W2

rs_rows <- list()

for (w in c(2, 3, 4, 6)) {
  dw      <- dat |> filter(wave == w)
  wave_n  <- nrow(dw)

  # Per-variable NA rates across all political items available in this wave
  available <- intersect(political_items, names(dw))
  na_rates  <- sapply(available, function(v) mean(is.na(dw[[v]])))
  mean_na   <- mean(na_rates)

  # DK rate on dem expectations items specifically
  dem_available <- intersect(dem_expect_items, names(dw))
  dem_na_rates  <- sapply(dem_available, function(v) mean(is.na(dw[[v]])))
  dem_mean_na   <- if (length(dem_na_rates) > 0) mean(dem_na_rates) else NA_real_

  # Straightlining: respondents who gave the identical response to all available auth items
  auth_use <- if (w == 2) auth_items_w2 else auth_items_all
  auth_avail <- intersect(auth_use, names(dw))
  auth_df  <- dw |> select(all_of(auth_avail)) |> drop_na()
  n_auth   <- nrow(auth_df)
  if (n_auth > 0) {
    n_straight <- sum(apply(auth_df, 1, function(r) length(unique(r)) == 1))
    straight_rate <- n_straight / n_auth
  } else {
    straight_rate <- NA_real_; n_straight <- NA_integer_; n_auth <- 0L
  }

  # Acquiescence: proportion at the positive ceiling of scale
  # Auth prefs (1-4): ceiling = 4
  auth_acq_vars  <- intersect(auth_items_all, names(dw))
  auth_acq_vals  <- unlist(lapply(auth_acq_vars, function(v) {
    x <- dw[[v]]; x[!is.na(x)]
  }))
  auth_acq_rate  <- if (length(auth_acq_vals) > 0) mean(auth_acq_vals == 4) else NA_real_

  # Dem expectations (0-10): ceiling = 9 or 10
  dem_acq_vals <- unlist(lapply(dem_available, function(v) {
    x <- dw[[v]]; x[!is.na(x)]
  }))
  dem_acq_rate <- if (length(dem_acq_vals) > 0) mean(dem_acq_vals >= 9) else NA_real_

  rs_rows[[length(rs_rows) + 1]] <- tibble(
    wave                         = w,
    wave_n                       = wave_n,
    auth_items_used              = paste(auth_use, collapse = ", "),
    mean_na_all_political        = round(mean_na, 4),
    mean_na_dem_expectations     = round(dem_mean_na, 4),
    na_dem_future                = round(mean(is.na(dw$dem_country_future)), 4),
    na_dem_past                  = round(mean(is.na(dw$dem_country_past)), 4),
    na_dem_present               = round(mean(is.na(dw$dem_country_present_govt)), 4),
    n_auth_complete              = as.integer(n_auth),
    n_straightliners             = as.integer(n_straight),
    straightline_rate            = round(straight_rate, 4),
    auth_acquiescence_rate       = round(auth_acq_rate, 4),
    dem_ceiling_rate_9_10        = round(dem_acq_rate, 4)
  )
}

response_style <- bind_rows(rs_rows)

# Also produce per-variable NA table
na_per_var <- dat |>
  group_by(wave) |>
  summarise(
    wave_n = n(),
    across(all_of(political_items), ~ round(mean(is.na(.x)), 4), .names = "{.col}"),
    .groups = "drop"
  ) |>
  pivot_longer(-c(wave, wave_n), names_to = "variable", values_to = "na_rate") |>
  mutate(na_pct = round(na_rate * 100, 1)) |>
  arrange(variable, wave)

response_style_full <- list(
  wave_level     = response_style,
  per_variable   = na_per_var
)

# Save wave-level summary as CSV; per-variable appended below
write.csv(
  bind_rows(
    response_style |> mutate(table = "wave_summary"),
    na_per_var |> rename(na_rate_value = na_rate) |> mutate(table = "per_variable_na")
  ),
  file.path(rr_dir, "response_style_diagnostics.csv"), row.names = FALSE
)
cat("Saved response_style_diagnostics.csv\n")
cat("\nWave-level summary:\n")
print(response_style |> select(wave, mean_na_all_political, mean_na_dem_expectations,
                                 straightline_rate, auth_acquiescence_rate, dem_ceiling_rate_9_10))

# ─────────────────────────────────────────────────────────────────────────────
# 9c. Subgroup splits
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n=== 9c. Subgroup splits ===\n")

key_vars_sg <- tribble(
  ~variable,                   ~label,             ~type,
  "gate_contact_influential",  "Gate influential", "prop",
  "dem_country_future",        "Dem future",       "mean",
  "corrupt_witnessed",         "Corrupt witnessed","prop",
  "single_party_rule",         "Single-party rule","mean",
  "voted_last_election",       "Voted",            "prop",
  "political_interest",        "Political interest","mean"
)

subgroup_specs <- list(
  list(col = "urban_label",  name = "urban_rural",
       levels = c("Urban", "Rural")),
  list(col = "age_group",    name = "age",
       levels = c("Under 30", "30-49", "50+")),
  list(col = "edu_group",    name = "education",
       levels = c("Primary or below", "Secondary", "Tertiary"))
)

sg_rows <- list()

for (w in c(2, 3, 4, 6)) {
  dw <- dat |> filter(wave == w)
  for (vi in seq_len(nrow(key_vars_sg))) {
    v   <- key_vars_sg$variable[vi]
    lbl <- key_vars_sg$label[vi]
    typ <- key_vars_sg$type[vi]
    if (!v %in% names(dw)) next
    for (sg in subgroup_specs) {
      sg_col <- sg$col
      sg_name <- sg$name
      for (sg_val in sg$levels) {
        vals <- dw |> filter(.data[[sg_col]] == sg_val) |> pull(all_of(v))
        if (sum(!is.na(vals)) < 5) next
        ci <- if (typ == "prop") wilson_ci(vals) else mean_ci(vals)
        sg_rows[[length(sg_rows) + 1]] <- tibble(
          variable       = v,
          label          = lbl,
          type           = typ,
          wave           = w,
          subgroup_type  = sg_name,
          subgroup_value = sg_val,
          estimate       = ci$estimate,
          se             = ci$se,
          ci_lower       = ci$ci_lower,
          ci_upper       = ci$ci_upper,
          valid_n        = ci$valid_n
        )
      }
    }
  }
}

subgroup_splits <- bind_rows(sg_rows)
saveRDS(subgroup_splits, file.path(rr_dir, "subgroup_splits.rds"))
write.csv(subgroup_splits, file.path(rr_dir, "subgroup_splits.csv"), row.names = FALSE)
cat("Saved subgroup_splits (.rds + .csv)\n")
cat(sprintf("  Rows: %d\n", nrow(subgroup_splits)))

# Quick check: gate_influential by urban_rural
cat("\ngate_influential by urban_rural:\n")
subgroup_splits |>
  filter(variable == "gate_contact_influential", subgroup_type == "urban_rural") |>
  select(wave, subgroup_value, estimate, ci_lower, ci_upper, valid_n) |>
  mutate(across(c(estimate, ci_lower, ci_upper), ~ round(.x * 100, 1))) |>
  print()

# ─────────────────────────────────────────────────────────────────────────────
# 9d. Placebo battery
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n=== 9d. Placebo battery ===\n")

placebo_meta <- tribble(
  ~variable,                   ~label,                          ~type,
  "trust_generalized_binary",  "Interpersonal trust (binary)",  "prop",
  "trust_generalized_ordinal", "Interpersonal trust (ordinal)", "mean",
  "nat_proud_citizen",         "National pride",                "mean",
  "system_proud",              "Pride in political system",     "mean",
  "hh_income_sat",             "HH income satisfaction",        "mean",
  "econ_family_now",           "Family econ situation (now)",   "mean",
  "econ_family_change",        "Family econ change (1yr)",      "mean",
  "econ_national_now",         "National econ situation",       "mean",
  "democracy_satisfaction",    "Democracy satisfaction",        "mean"
)

pb_rows <- list()
for (w in c(2, 3, 4, 6)) {
  dw <- dat |> filter(wave == w)
  for (i in seq_len(nrow(placebo_meta))) {
    v   <- placebo_meta$variable[i]
    lbl <- placebo_meta$label[i]
    typ <- placebo_meta$type[i]
    if (!v %in% names(dw)) next
    vals <- dw[[v]]
    if (sum(!is.na(vals)) < 5) next
    ci <- if (typ == "prop") wilson_ci(vals) else mean_ci(vals)
    pb_rows[[length(pb_rows) + 1]] <- tibble(
      variable  = v,
      label     = lbl,
      type      = typ,
      wave      = w,
      estimate  = ci$estimate,
      se        = ci$se,
      ci_lower  = ci$ci_lower,
      ci_upper  = ci$ci_upper,
      valid_n   = ci$valid_n
    )
  }
}

placebo_battery <- bind_rows(pb_rows)
write.csv(placebo_battery, file.path(rr_dir, "placebo_battery.csv"), row.names = FALSE)
cat("Saved placebo_battery.csv\n")
cat("\nPlacebo battery wave means:\n")
placebo_battery |>
  select(label, wave, estimate, se, valid_n) |>
  mutate(estimate = round(estimate, 3), se = round(se, 4)) |>
  print(n = 40)

# ─────────────────────────────────────────────────────────────────────────────
# 9e. Comparability matrix
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n=== 9e. Comparability matrix ===\n")

# Based on ABS questionnaire wording audit and editorial notes.
# Status: Full / Partial / Excluded
# Notes explain which wave pairs are affected.

comparability_matrix <- tribble(
  ~variable,                    ~label,                          ~w2_wording,                                        ~w3_wording,                                        ~w4_wording,                                       ~w6_wording,                                       ~status,   ~notes,
  "gate_contact_elected",       "Contacted elected official",    "Have you ever contacted an elected official to express your views? (broad)",  "Have you ever contacted an elected official to complain about or seek help with a problem? (narrow)",  "Have you ever contacted an elected official to express your views? (broad)",  "Have you ever contacted an elected official to express your views? (broad)",  "Partial",  "W3 uses problem-motivated framing; W2↔W3 and W3↔W4 comparisons invalid. W2, W4, W6 comparable.",
  "gate_contact_civil_servant", "Contacted civil servant",       "Have you ever contacted a civil servant to express your views? (broad)",      "Have you ever contacted a government official to complain about or seek help with a problem? (narrow)", "Have you ever contacted a civil servant to express your views? (broad)",      "Have you ever contacted a civil servant to express your views? (broad)",      "Partial",  "W3 uses problem-motivated framing; W2↔W3 and W3↔W4 comparisons invalid. W2, W4, W6 comparable.",
  "gate_contact_influential",   "Contacted influential person",  "Have you ever contacted a person of influence? (broad)",                      "Have you ever contacted a person of influence? (broad)",                                              "Have you ever contacted a person of influence? (broad)",                     "Have you ever contacted a person of influence? (broad)",                     "Full",     "Consistent broad framing across all waves. Recommended for cross-wave participation comparisons.",
  "gate_petition",              "Signed petition",               "Not collected in W2",                                                          "Have you ever signed a petition? (consistent)",                                                        "Have you ever signed a petition? (consistent)",                              "Have you ever signed a petition? (consistent)",                              "Partial",  "W2 not collected. W3/W4/W6 comparable.",
  "gate_demonstration",         "Attended demonstration",        "Have you ever attended a demonstration or protest march? (consistent)",        "Have you ever attended a demonstration or protest march? (consistent)",                               "Have you ever attended a demonstration or protest march? (consistent)",      "Have you ever attended a demonstration or protest march? (consistent)",      "Full",     "Consistent wording across all four waves.",
  "gate_contact_media",         "Contacted media",               "Have you ever contacted the media to express your views? (consistent)",        "Have you ever contacted the media to express your views? (consistent)",                               "Have you ever contacted the media to express your views? (consistent)",      "Have you ever contacted the media to express your views? (consistent)",      "Full",     "Consistent wording across all four waves.",
  "community_leader_contact",   "Community leader contact",      "How often do you contact community leaders? (1–5 frequency)",                  "How often do you contact community leaders? (1–5 frequency)",                                         "How often do you contact community leaders? (1–5 frequency)",                "How often do you contact community leaders? (1–5 frequency)",                "Full",     "Consistent scale and wording across all waves.",
  "voted_last_election",        "Voted in last election",        "Did you vote in the last national election? (binary)",                          "Did you vote in the last national election? (binary)",                                                 "Did you vote in the last national election? (binary)",                        "Did you vote in the last national election? (binary)",                        "Full",     "Consistent across all waves. Note: reference election differs by wave (2003/2008/2013/2018).",
  "expert_rule",                "Expert rule",                   "Not collected in W2",                                                          "Should experts rather than government make decisions? (1–4)",                                          "Should experts rather than government make decisions? (1–4)",                 "Should experts rather than government make decisions? (1–4)",                 "Partial",  "W2 not collected. W3/W4/W6 comparable on identical 1–4 scale.",
  "single_party_rule",          "Single-party rule",             "Should only one political party be allowed to contest elections? (1–4)",        "Should only one political party be allowed to contest elections? (1–4)",                               "Should only one political party be allowed to contest elections? (1–4)",      "Should only one political party be allowed to contest elections? (1–4)",      "Full",     "Consistent 1–4 scale ('very bad' to 'very good') across all waves.",
  "strongman_rule",             "Strongman rule",                "Should we get rid of parliament and elections for a strong leader? (1–4)",      "Should we get rid of parliament and elections for a strong leader? (1–4)",                            "Should we get rid of parliament and elections for a strong leader? (1–4)",    "Should we get rid of parliament and elections for a strong leader? (1–4)",    "Full",     "Consistent 1–4 scale across all waves.",
  "military_rule",              "Military rule",                 "Should the military rule the country? (1–4)",                                  "Should the military rule the country? (1–4)",                                                          "Should the military rule the country? (1–4)",                                 "Should the military rule the country? (1–4)",                                 "Full",     "Consistent 1–4 scale across all waves.",
  "dem_country_future",         "Democratic future (0-10)",      "Not collected in W2",                                                          "How democratic will this country be in 10 years? (0–10)",                                              "How democratic will this country be in 10 years? (0–10)",                    "How democratic will this country be in 10 years? (0–10)",                    "Partial",  "W2 not collected. W3/W4/W6 comparable on identical 0–10 scale.",
  "dem_country_past",           "Democratic past (0-10)",        "Not collected in W2",                                                          "How democratic was this country 10 years ago? (0–10)",                                                 "How democratic was this country 10 years ago? (0–10)",                        "How democratic was this country 10 years ago? (0–10)",                        "Partial",  "W2 not collected. W3/W4/W6 comparable.",
  "dem_country_present_govt",   "Democratic present (0-10)",     "How democratic is this country today? (partial coverage, W2)",                  "How democratic is this country today? (0–10 scale, government performance framing)",                    "How democratic is this country today? (0–10 scale, government performance framing)", "How democratic is this country today? (0–10 scale, government performance framing)", "Partial",  "W2 has ~53% nonresponse; low confidence on W2 estimate. W3/W4/W6 comparable.",
  "corrupt_witnessed",          "Witnessed corruption (binary)", "Has any government official asked you for a bribe? (binary W2)",               "Have you or anyone in your family personally experienced or witnessed government corruption? (broader)",  "Have you or anyone in your family personally experienced or witnessed government corruption?",     "Have you or anyone in your family personally experienced or witnessed government corruption?",     "Partial",  "W2 wording is narrower (bribe solicitation only); W3–W6 use broader witnessed/experienced framing. W2↔W3 comparison should be treated with caution.",
  "corrupt_national_govt",      "National govt corruption",      "How widespread is corruption in national government? (1–4)",                    "How widespread is corruption in national government? (1–4)",                                           "How widespread is corruption in national government? (1–4)",                  "How widespread is corruption in national government? (1–4)",                  "Full",     "Consistent 1–4 scale across all waves.",
  "corrupt_local_govt",         "Local govt corruption",         "How widespread is corruption in local government? (1–4)",                      "How widespread is corruption in local government? (1–4)",                                              "How widespread is corruption in local government? (1–4)",                     "How widespread is corruption in local government? (1–4)",                     "Full",     "Consistent 1–4 scale across all waves.",
  "pol_news_follow",            "Follows political news",        "How often do you follow political news? (1–5)",                                 "How often do you follow political news? (1–5)",                                                        "How often do you follow political news? (1–5)",                               "How often do you follow political news? (1–5)",                               "Full",     "Consistent 1–5 scale across all waves.",
  "news_internet",              "Internet news (harmonized)",    "How often do you get news from internet? (6-point scale)",                      "How often do you get news from internet? (6-point scale)",                                             "How often do you get news from internet? (8-point scale — harmonized to 6pt)", "How often do you get news from internet? (9-point scale — harmonized to 6pt)", "Partial",  "Response scale differs across waves (6pt W2/W3, 8pt W4, 9pt W6). Harmonized to common 1–6 scale. Within-frequency comparisons (never vs. daily) valid; fine distinctions within daily use not comparable.",
  "political_interest",         "Political interest",            "How interested are you in politics? (1–4)",                                    "How interested are you in politics? (1–4)",                                                            "How interested are you in politics? (1–4)",                                   "How interested are you in politics? (1–4)",                                   "Full",     "Consistent 1–4 scale across all waves.",
  "pol_discuss",                "Discusses politics",            "How often do you discuss politics with friends or family? (1–3)",               "How often do you discuss politics with friends or family? (1–3)",                                      "How often do you discuss politics with friends or family? (1–3)",             "How often do you discuss politics with friends or family? (1–3)",             "Full",     "Consistent 1–3 scale across all waves."
)

write.csv(comparability_matrix, file.path(rr_dir, "comparability_matrix.csv"), row.names = FALSE)
cat("Saved comparability_matrix.csv\n")
cat(sprintf("  %d variables documented\n", nrow(comparability_matrix)))
cat("\nStatus summary:\n")
print(table(comparability_matrix$status))

# ─────────────────────────────────────────────────────────────────────────────
# 9f. Figure 1 with uncertainty bands
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n=== 9f. Figure 1 with uncertainty ===\n")

fig_vars <- tribble(
  ~domain,                    ~variable,                   ~label,                  ~type,
  "Political Participation",  "gate_contact_influential",  "Contacted influential", "prop",
  "Political Participation",  "gate_demonstration",        "Attended demonstration","prop",
  "Political Participation",  "voted_last_election",       "Voted (last election)", "prop",
  "Authoritarian Preferences","single_party_rule",         "Single-party rule",     "mean",
  "Authoritarian Preferences","strongman_rule",            "Strongman rule",        "mean",
  "Democratic Expectations",  "dem_country_future",        "Democratic future",     "mean",
  "Democratic Expectations",  "dem_country_present_govt",  "Democratic present",   "mean",
  "Corruption",               "corrupt_witnessed",         "Witnessed corruption",  "prop",
  "Corruption",               "corrupt_national_govt",     "Nat'l govt corruption", "mean",
  "Media & Political Interest","pol_news_follow",          "Follows pol. news",     "mean",
  "Media & Political Interest","political_interest",       "Political interest",    "mean"
)

fig_ci_rows <- list()
for (w in c(2, 3, 4, 6)) {
  yr <- c(`2`=2008, `3`=2012, `4`=2015, `6`=2021)[as.character(w)]
  dw <- dat |> filter(wave == w)
  for (i in seq_len(nrow(fig_vars))) {
    v   <- fig_vars$variable[i]
    typ <- fig_vars$type[i]
    if (!v %in% names(dw)) next
    vals <- dw[[v]]
    ci   <- if (typ == "prop") wilson_ci(vals) else mean_ci(vals)
    if (is.na(ci$estimate)) next
    scale_mult <- if (typ == "prop") 100 else 1
    fig_ci_rows[[length(fig_ci_rows) + 1]] <- tibble(
      variable  = v,
      wave      = w,
      year      = yr,
      mean_val  = ci$estimate * scale_mult,
      ci_lower  = ci$ci_lower * scale_mult,
      ci_upper  = ci$ci_upper * scale_mult,
      valid_n   = ci$valid_n
    )
  }
}

fig_ci_data <- bind_rows(fig_ci_rows) |>
  left_join(fig_vars, by = "variable") |>
  mutate(
    domain = factor(domain, levels = c(
      "Political Participation", "Authoritarian Preferences",
      "Democratic Expectations", "Corruption", "Media & Political Interest"
    ))
  )

fig1_ci <- ggplot(fig_ci_data, aes(x = year, y = mean_val, color = label, group = label)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = label), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  annotate("text", x = 2017, y = -Inf, label = "CNRP\ndissolved",
           hjust = -0.1, vjust = -0.3, size = 2.5, color = "grey50") +
  facet_wrap(~domain, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = c(2008, 2012, 2015, 2021)) +
  scale_color_manual(values = c(
    "#2166AC","#B2182B","#4DAF4A","#F4A582","#D6604D",
    "#92C5DE","#4393C3","#762A83","#9970AB","#1B7837","#5AAE61"
  )) +
  scale_fill_manual(values = c(
    "#2166AC","#B2182B","#4DAF4A","#F4A582","#D6604D",
    "#92C5DE","#4393C3","#762A83","#9970AB","#1B7837","#5AAE61"
  )) +
  labs(
    title    = "Political Orientations in Cambodia, 2008–2021",
    subtitle = "Participation panel: % who ever engaged (gate). Other panels: wave means. Shading = 95% CI.",
    x = NULL, y = NULL,
    caption = paste0("Source: Asian Barometer Survey, Waves 2, 3, 4, 6. Cambodia (N = 1,000–1,242 per wave).\n",
                     "CI: Wilson interval for proportions; t-based interval for means.")
  ) +
  guides(fill = "none") +
  theme_pub +
  theme(legend.position = "right", legend.text = element_text(size = 7.5))

ggsave(file.path(rr_dir, "fig1_with_uncertainty.pdf"), fig1_ci, width = 10, height = 8)
ggsave(file.path(rr_dir, "fig1_with_uncertainty.png"), fig1_ci, width = 10, height = 8, dpi = 300)
cat("Saved fig1_with_uncertainty (.pdf + .png)\n")

# ─────────────────────────────────────────────────────────────────────────────
# Summary checklist
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n=== OUTPUT CHECKLIST ===\n")
outputs <- c(
  "uncertainty_estimates.rds",
  "uncertainty_estimates.csv",
  "difference_tests.csv",
  "response_style_diagnostics.csv",
  "subgroup_splits.rds",
  "subgroup_splits.csv",
  "placebo_battery.csv",
  "comparability_matrix.csv",
  "fig1_with_uncertainty.pdf",
  "fig1_with_uncertainty.png"
)
for (f in outputs) {
  path <- file.path(rr_dir, f)
  status <- if (file.exists(path)) sprintf("✓  (%s KB)", round(file.size(path)/1024, 1)) else "MISSING"
  cat(sprintf("  %-40s %s\n", f, status))
}

cat("\nDONE.\n")
