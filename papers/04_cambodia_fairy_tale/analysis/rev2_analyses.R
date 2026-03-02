#!/usr/bin/env Rscript
# rev2_analyses.R — Reviewer 2 response analyses
# Tasks 1 (W3→W4 partisan decomp), 2 (W6 DK/Refuse profile), 4 (bounds analysis)
# Outputs: analysis/tables/tableA4_partisan_w3w4.rds
#          analysis/tables/tableA5_dk_profile.rds
#          analysis/tables/tableA9_bounds.rds

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
})

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/04_cambodia_fairy_tale")
results_dir  <- file.path(paper_dir, "analysis/results")
tbl_dir      <- file.path(paper_dir, "analysis/tables")
rr_dir       <- file.path(paper_dir, "analysis/reviewer_response")

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
      wave == 2 ~ 2008L, wave == 3 ~ 2012L,
      wave == 4 ~ 2015L, wave == 6 ~ 2021L
    ),
    across(all_of(gate_vars), ~ as.numeric(as.character(.x))),
    # Partisan category: based on voted_winning_losing + voted_last_election
    # voted_winning_losing: 1 = CPP (winner), 2 = CNRP/SRP (loser)
    # voted_last_election:  1 = voted, 0 = nonvoter
    partisan = case_when(
      voted_winning_losing == 1 ~ "CPP voter",
      voted_winning_losing == 2 ~ "CNRP/SRP voter",
      voted_last_election  == 0 ~ "Nonvoter",
      TRUE                      ~ "DK/Refuse"
    ),
    partisan = factor(partisan,
                      levels = c("CPP voter", "CNRP/SRP voter",
                                 "Nonvoter", "DK/Refuse"))
  )

cat("N per wave:\n"); print(table(dat$wave))
cat("\nPartisan distribution W3:\n")
print(table(dat$partisan[dat$wave == 3]))
cat("\nPartisan distribution W4:\n")
print(table(dat$partisan[dat$wave == 4]))
cat("\nPartisan distribution W6:\n")
print(table(dat$partisan[dat$wave == 6]))

# ── CI helpers ─────────────────────────────────────────────────────────────────
wilson_ci <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(list(estimate = NA_real_, se = NA_real_,
                          ci_lower = NA_real_, ci_upper = NA_real_, valid_n = 0L))
  k   <- sum(x)
  p   <- k / n
  se  <- sqrt(p * (1 - p) / n)
  res <- suppressWarnings(prop.test(k, n, conf.level = conf))
  list(estimate = p, se = se,
       ci_lower = res$conf.int[1], ci_upper = res$conf.int[2],
       valid_n  = as.integer(n))
}

mean_ci <- function(x, conf = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(list(estimate = mean(x), se = NA_real_,
                         ci_lower = NA_real_, ci_upper = NA_real_,
                         valid_n  = as.integer(n)))
  m      <- mean(x)
  se     <- sd(x) / sqrt(n)
  t_crit <- qt((1 + conf) / 2, df = n - 1)
  list(estimate = m, se = se,
       ci_lower = m - t_crit * se, ci_upper = m + t_crit * se,
       valid_n  = as.integer(n))
}

fmt_pct_ci <- function(est, lo, hi) {
  if (is.na(est)) return("---")
  sprintf("%.1f\\%% (%.1f, %.1f)", est * 100, lo * 100, hi * 100)
}
fmt_mean_ci <- function(est, lo, hi) {
  if (is.na(est)) return("---")
  sprintf("%.2f (%.2f, %.2f)", est, lo, hi)
}
fmt_delta_pct <- function(d) {
  if (is.na(d)) return("---")
  if (d > 0) sprintf("+%.1f pp", d * 100) else sprintf("%.1f pp", d * 100)
}
fmt_delta_mean <- function(d) {
  if (is.na(d)) return("---")
  if (d > 0) sprintf("+%.2f", d) else sprintf("%.2f", d)
}
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("")
}

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 1: W3→W4 Partisan Decomposition Table
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== TASK 1: W3→W4 Partisan Decomposition ===\n")

# Variables to report (matching W4→W6 decomp in manuscript)
decomp_vars <- tribble(
  ~variable,                  ~label,                              ~type,
  "gate_contact_influential", "Contacted influential person (\\%)", "prop",
  "dem_country_future",       "Democratic future (0--10 mean)",     "mean",
  "single_party_rule",        "Single-party rule acceptance (1--4)","mean",
  "political_interest",       "Political interest (1--4)",          "mean",
  "corrupt_witnessed",        "Witnessed corruption (\\%)",         "prop",
  "democracy_satisfaction",   "Democracy satisfaction (1--4)",      "mean"
)

# Compute group × wave estimates for W3 and W4
compute_group_est <- function(data, wave_num, var, type) {
  groups <- levels(data$partisan)
  res <- list()
  for (g in groups) {
    x <- data |> filter(wave == wave_num, partisan == g) |> pull(all_of(var))
    ci <- if (type == "prop") wilson_ci(x) else mean_ci(x)
    res[[g]] <- tibble(
      wave = wave_num, partisan = g,
      estimate = ci$estimate, ci_lower = ci$ci_lower, ci_upper = ci$ci_upper,
      valid_n  = ci$valid_n
    )
  }
  bind_rows(res)
}

# Between-group difference in change: does CNRP delta differ from CPP delta?
# Model: y ~ wave_f * partisan_group, CPP as reference, W3 as reference wave
# For W3→W4 period. Report interaction term for CNRP × W4.
test_interaction <- function(data, var, type) {
  df <- data |>
    filter(wave %in% c(3, 4), partisan %in% c("CPP voter", "CNRP/SRP voter")) |>
    filter(!is.na(.data[[var]])) |>
    mutate(
      wave_f = factor(wave, levels = c(3, 4)),
      part_f = relevel(factor(as.character(partisan)), ref = "CPP voter")
    )
  if (nrow(df) < 10) return(tibble(interaction_est = NA_real_, p_value = NA_real_))
  f   <- as.formula(paste(var, "~ wave_f * part_f"))
  m   <- lm(f, data = df)
  res <- tidy(m)
  # Interaction term: wave_f4:part_fCNRP/SRP voter
  int <- res |> filter(grepl("wave_f4.*CNRP|CNRP.*wave_f4", term))
  if (nrow(int) == 0) return(tibble(interaction_est = NA_real_, p_value = NA_real_))
  tibble(interaction_est = int$estimate[1], p_value = int$p.value[1])
}

partisan_decomp_rows <- list()

for (i in seq_len(nrow(decomp_vars))) {
  v   <- decomp_vars$variable[i]
  lbl <- decomp_vars$label[i]
  typ <- decomp_vars$type[i]

  e3 <- compute_group_est(dat, 3, v, typ)
  e4 <- compute_group_est(dat, 4, v, typ)
  int_test <- test_interaction(dat, v, typ)

  for (g in levels(dat$partisan)) {
    w3_row <- e3 |> filter(partisan == g)
    w4_row <- e4 |> filter(partisan == g)

    if (typ == "prop") {
      w3_fmt  <- fmt_pct_ci(w3_row$estimate, w3_row$ci_lower, w3_row$ci_upper)
      w4_fmt  <- fmt_pct_ci(w4_row$estimate, w4_row$ci_lower, w4_row$ci_upper)
      delta   <- if (!is.na(w3_row$estimate) && !is.na(w4_row$estimate))
                   w4_row$estimate - w3_row$estimate else NA_real_
      d_fmt   <- fmt_delta_pct(delta)
    } else {
      w3_fmt  <- fmt_mean_ci(w3_row$estimate, w3_row$ci_lower, w3_row$ci_upper)
      w4_fmt  <- fmt_mean_ci(w4_row$estimate, w4_row$ci_lower, w4_row$ci_upper)
      delta   <- if (!is.na(w3_row$estimate) && !is.na(w4_row$estimate))
                   w4_row$estimate - w3_row$estimate else NA_real_
      d_fmt   <- fmt_delta_mean(delta)
    }

    # Add significance flag (for CNRP/SRP row only, from interaction test)
    sig_flag <- if (g == "CNRP/SRP voter") fmt_p(int_test$p_value) else ""

    partisan_decomp_rows[[length(partisan_decomp_rows) + 1]] <- tibble(
      variable    = v,
      label       = lbl,
      group       = g,
      n_w3        = w3_row$valid_n,
      n_w4        = w4_row$valid_n,
      w3_fmt      = w3_fmt,
      w4_fmt      = w4_fmt,
      delta_fmt   = paste0(d_fmt, sig_flag),
      int_est     = int_test$interaction_est,
      int_p       = int_test$p_value
    )
  }
}

tableA4 <- bind_rows(partisan_decomp_rows)

cat("\nPartisan decomp summary:\n")
tableA4 |>
  select(label, group, w3_fmt, w4_fmt, delta_fmt) |>
  print(n = 40)

# Print interaction test results separately
cat("\n=== Between-group interaction tests (CNRP vs CPP) ===\n")
tableA4 |>
  filter(group == "CNRP/SRP voter") |>
  select(label, int_est, int_p) |>
  mutate(across(c(int_est, int_p), ~ round(.x, 4))) |>
  print()

saveRDS(tableA4, file.path(tbl_dir, "tableA4_partisan_w3w4.rds"))
write.csv(tableA4, file.path(tbl_dir, "tableA4_partisan_w3w4.csv"), row.names = FALSE)
cat("Saved tableA4_partisan_w3w4\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 2: W6 DK/Refuse Group Profile
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== TASK 2: W6 DK/Refuse Group Profile ===\n")

# Variables for W6 profile
profile_vars <- tribble(
  ~domain,                    ~variable,                  ~label,                              ~type,
  "Democratic Orientation",   "dem_always_preferable",    "Democracy always preferable (\\% = 1)", "prop_val1",
  "Democratic Orientation",   "dem_best_form",            "Democracy is best form (1--4 mean)",    "mean",
  "Democratic Orientation",   "dem_vs_equality",          "Democracy vs. equality (1--5 mean)",    "mean",
  "Democratic Orientation",   "democracy_satisfaction",   "Democracy satisfaction (1--4 mean)",    "mean",
  "Democratic Expectations",  "dem_country_future",       "Democratic future (0--10 mean)",        "mean",
  "Democratic Expectations",  "dem_country_present_govt", "Democratic present (0--10 mean)",       "mean",
  "Democratic Expectations",  "dem_country_past",         "Democratic past (0--10 mean)",          "mean",
  "Auth. Preferences",        "expert_rule",              "Expert rule (1--4 mean)",               "mean",
  "Auth. Preferences",        "single_party_rule",        "Single-party rule (1--4 mean)",         "mean",
  "Auth. Preferences",        "strongman_rule",           "Strongman rule (1--4 mean)",            "mean",
  "Auth. Preferences",        "military_rule",            "Military rule (1--4 mean)",             "mean",
  "Civic Engagement",         "gate_contact_influential", "Contacted influential person (\\%)",    "prop",
  "Civic Engagement",         "political_interest",       "Political interest (1--4 mean)",        "mean",
  "Civic Engagement",         "pol_news_follow",          "Follows political news (mean)",         "mean"
)

# In W6, conditional authoritarianism = voting CPP but not fully endorsing regime
# dem_always_preferable: 1=democracy always pref, 2=sometimes, 3=sometimes non-dem better
# prop_val1: proportion with value == 1

w6 <- dat |> filter(wave == 6)
groups_w6 <- c("CPP voter", "CNRP/SRP voter", "DK/Refuse")

cat(sprintf("W6 group sizes:\n"))
print(table(w6$partisan))

# Nonresponse rates on democratic expectation items
cat("\n=== W6 Nonresponse rates on democratic future item by group ===\n")
nr_rates <- w6 |>
  group_by(partisan) |>
  summarise(
    n_total          = n(),
    na_dem_future    = round(mean(is.na(dem_country_future)) * 100, 1),
    na_dem_past      = round(mean(is.na(dem_country_past)) * 100, 1),
    na_dem_present   = round(mean(is.na(dem_country_present_govt)) * 100, 1),
    .groups = "drop"
  )
print(nr_rates)

profile_rows <- list()

for (i in seq_len(nrow(profile_vars))) {
  v   <- profile_vars$variable[i]
  lbl <- profile_vars$label[i]
  dom <- profile_vars$domain[i]
  typ <- profile_vars$type[i]

  row_cells <- list(domain = dom, variable = v, label = lbl)

  for (g in groups_w6) {
    gdat <- w6 |> filter(partisan == g) |> pull(all_of(v))
    if (typ == "prop_val1") {
      # Proportion with value == 1
      gbin <- as.integer(gdat == 1)
      gbin[is.na(gdat)] <- NA_integer_
      ci <- wilson_ci(gbin)
      row_cells[[paste0("fmt_", gsub("/| ", "_", g))]] <-
        fmt_pct_ci(ci$estimate, ci$ci_lower, ci$ci_upper)
      row_cells[[paste0("n_", gsub("/| ", "_", g))]] <- ci$valid_n
    } else if (typ == "prop") {
      ci <- wilson_ci(gdat)
      row_cells[[paste0("fmt_", gsub("/| ", "_", g))]] <-
        fmt_pct_ci(ci$estimate, ci$ci_lower, ci$ci_upper)
      row_cells[[paste0("n_", gsub("/| ", "_", g))]] <- ci$valid_n
    } else {
      ci <- mean_ci(gdat)
      row_cells[[paste0("fmt_", gsub("/| ", "_", g))]] <-
        fmt_mean_ci(ci$estimate, ci$ci_lower, ci$ci_upper)
      row_cells[[paste0("n_", gsub("/| ", "_", g))]] <- ci$valid_n
    }
  }

  profile_rows[[length(profile_rows) + 1]] <- as_tibble(row_cells)
}

tableA5 <- bind_rows(profile_rows)

# Also add nonresponse rates as rows
nr_rows <- nr_rates |>
  filter(partisan %in% groups_w6) |>
  select(partisan, na_dem_future) |>
  pivot_wider(names_from = partisan,
              values_from = na_dem_future,
              names_prefix = "nr_")

cat("\nW6 Group Profile (first 8 rows):\n")
tableA5 |> select(domain, label, starts_with("fmt_")) |> print(n = 8)

# Save nonresponse rates alongside profile
tableA5_full <- list(profile = tableA5, nonresponse = nr_rates)
saveRDS(tableA5_full, file.path(tbl_dir, "tableA5_dk_profile.rds"))
write.csv(tableA5, file.path(tbl_dir, "tableA5_dk_profile.csv"), row.names = FALSE)
cat("Saved tableA5_dk_profile\n")

# ═══════════════════════════════════════════════════════════════════════════════
# TASK 4: Bounds Analysis Table
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== TASK 4: Bounds Analysis Table ===\n")

# dem_country_future (0-10 scale) W3 and W6
w3_future <- dat |> filter(wave == 3) |> pull(dem_country_future)
w6_future <- dat |> filter(wave == 6) |> pull(dem_country_future)

n_w3_total     <- length(w3_future)
n_w3_resp      <- sum(!is.na(w3_future))
mean_w3_obs    <- mean(w3_future, na.rm = TRUE)

n_w6_total     <- length(w6_future)
n_w6_resp      <- sum(!is.na(w6_future))
n_w6_nonresp   <- n_w6_total - n_w6_resp
mean_w6_obs    <- mean(w6_future, na.rm = TRUE)

# Imputation scenarios
sum_w6_resp    <- sum(w6_future, na.rm = TRUE)

# Scenario 1: nonrespondents imputed at scale max (10)
mean_w6_max    <- (sum_w6_resp + n_w6_nonresp * 10) / n_w6_total
# Scenario 2: nonrespondents imputed at scale min (0)
mean_w6_min    <- (sum_w6_resp + n_w6_nonresp * 0)  / n_w6_total
# Scenario 3: nonrespondents imputed at W3 population mean
mean_w6_w3mean <- (sum_w6_resp + n_w6_nonresp * mean_w3_obs) / n_w6_total

cat(sprintf("W3: N=%d, respondents=%d (%.1f%%), mean=%.2f\n",
            n_w3_total, n_w3_resp, n_w3_resp/n_w3_total*100, mean_w3_obs))
cat(sprintf("W6: N=%d, respondents=%d (%.1f%%), non-respondents=%d (%.1f%%), mean_obs=%.2f\n",
            n_w6_total, n_w6_resp, n_w6_resp/n_w6_total*100,
            n_w6_nonresp, n_w6_nonresp/n_w6_total*100, mean_w6_obs))

cat("\nBounds under imputation scenarios:\n")
cat(sprintf("  Observed W6 mean (respondents only):     %.2f  Δ from W3: %+.2f\n",
            mean_w6_obs,    mean_w6_obs    - mean_w3_obs))
cat(sprintf("  W6 mean (nonrespondents → max = 10):    %.2f  Δ from W3: %+.2f\n",
            mean_w6_max,    mean_w6_max    - mean_w3_obs))
cat(sprintf("  W6 mean (nonrespondents → min = 0):     %.2f  Δ from W3: %+.2f\n",
            mean_w6_min,    mean_w6_min    - mean_w3_obs))
cat(sprintf("  W6 mean (nonrespondents → W3 mean):     %.2f  Δ from W3: %+.2f\n",
            mean_w6_w3mean, mean_w6_w3mean - mean_w3_obs))

tableA9 <- tribble(
  ~Scenario,                                         ~Wave,   ~N,            ~Respondents, ~Mean,             ~Delta_from_W3,
  "Observed (W3, 2012)",                             "W3",    n_w3_total,    n_w3_resp,    round(mean_w3_obs, 2),          "—",
  "Observed (W6, respondents only)",                 "W6",    n_w6_total,    n_w6_resp,    round(mean_w6_obs, 2),
    sprintf("%+.2f", mean_w6_obs - mean_w3_obs),
  "Bound: nonrespondents imputed at max (10)",       "W6",    n_w6_total,    n_w6_total,   round(mean_w6_max, 2),
    sprintf("%+.2f", mean_w6_max - mean_w3_obs),
  "Bound: nonrespondents imputed at min (0)",        "W6",    n_w6_total,    n_w6_total,   round(mean_w6_min, 2),
    sprintf("%+.2f", mean_w6_min - mean_w3_obs),
  "Conservative: nonrespondents imputed at W3 mean","W6",    n_w6_total,    n_w6_total,   round(mean_w6_w3mean, 2),
    sprintf("%+.2f", mean_w6_w3mean - mean_w3_obs)
)

saveRDS(tableA9, file.path(tbl_dir, "tableA9_bounds.rds"))
write.csv(tableA9, file.path(tbl_dir, "tableA9_bounds.csv"), row.names = FALSE)
cat("\nSaved tableA9_bounds\n")

# ═══════════════════════════════════════════════════════════════════════════════
# OUTPUT CHECKLIST
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== OUTPUT CHECKLIST ===\n")
files_out <- c(
  "analysis/tables/tableA4_partisan_w3w4.rds",
  "analysis/tables/tableA4_partisan_w3w4.csv",
  "analysis/tables/tableA5_dk_profile.rds",
  "analysis/tables/tableA5_dk_profile.csv",
  "analysis/tables/tableA9_bounds.rds",
  "analysis/tables/tableA9_bounds.csv"
)
for (f in files_out) {
  path   <- file.path(paper_dir, f)
  status <- if (file.exists(path)) sprintf("OK  (%.1f KB)", file.size(path)/1024) else "MISSING"
  cat(sprintf("  %-50s %s\n", f, status))
}
cat("\nDONE.\n")
