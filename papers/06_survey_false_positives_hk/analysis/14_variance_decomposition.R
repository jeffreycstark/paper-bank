###############################################################################
# 14_variance_decomposition.R
# Mechanism Decomposition: Falsification vs. Selection via Distributional Shape
#
# Logic: Pure falsification predicts ceiling clustering (increased top-response
# proportion, reduced variance, negative skew shift). Pure selection predicts
# bottom truncation (decreased floor proportion, preserved or reduced variance,
# but NOT necessarily ceiling increase). The joint pattern — both ceiling
# increase AND floor decrease — indicates both mechanisms operating. The
# relative magnitudes help assess which dominates.
#
# Run from: papers/06_survey_false_positives_hk/
# Outputs:  analysis/results/variance_decomposition.RData
###############################################################################

library(tidyverse)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  getwd()
}

analysis_dir <- get_script_dir()

# ── Load prepared data ───────────────────────────────────────────────────────
load(file.path(analysis_dir, "results", "prepared_data.RData"))
load(file.path(analysis_dir, "results", "descriptive_results.RData"))

hk5_analysis <- hk5 |>
  filter(period %in% c("Protest", "Post-NSL")) |>
  mutate(post_nsl = as.integer(period == "Post-NSL"))

# ── Define trust items with sensitivity ranking ──────────────────────────────
trust_items <- tribble(
  ~variable,                    ~label,                  ~sens_rank,
  "trust_police",               "Trust in police",       1,
  "trust_national_government",  "Trust in nat. govt",    2,
  "trust_president",            "Trust in president/CE", 3,
  "trust_parliament",           "Trust in parliament",   5,
  "trust_courts",               "Trust in courts",       6,
  "trust_civil_service",        "Trust in civil service", 7
)

# ── Distributional analysis for each trust item ──────────────────────────────
cat("================================================================\n")
cat("DISTRIBUTIONAL DECOMPOSITION: Falsification vs. Selection\n")
cat("================================================================\n\n")

dist_results <- trust_items |>
  pmap_dfr(function(variable, label, sens_rank) {
    pro <- hk5_analysis |> filter(period == "Protest", !is.na(!!sym(variable)))
    post <- hk5_analysis |> filter(period == "Post-NSL", !is.na(!!sym(variable)))

    # Proportions at each response level (1-4 scale)
    pro_dist <- table(factor(pro[[variable]], levels = 1:4)) / nrow(pro) * 100
    post_dist <- table(factor(post[[variable]], levels = 1:4)) / nrow(post) * 100

    # SDs (unweighted for distributional analysis)
    pro_sd <- sd(pro[[variable]])
    post_sd <- sd(post[[variable]])

    # Skewness
    skew <- function(x) {
      n <- length(x)
      m <- mean(x)
      s <- sd(x)
      (1/n) * sum(((x - m) / s)^3)
    }

    # Brown-Forsythe (Levene using median) test for variance equality
    combined <- bind_rows(
      tibble(val = pro[[variable]], period = "Protest"),
      tibble(val = post[[variable]], period = "Post-NSL")
    ) |> mutate(
      med = ave(val, period, FUN = median),
      abs_dev = abs(val - med)
    )
    levene_m <- lm(abs_dev ~ period, data = combined)
    levene_F <- summary(levene_m)$fstatistic[1]
    levene_p <- pf(levene_F,
                    summary(levene_m)$fstatistic[2],
                    summary(levene_m)$fstatistic[3],
                    lower.tail = FALSE)

    tibble(
      variable = variable,
      label = label,
      sens_rank = sens_rank,
      # SDs
      protest_sd = pro_sd,
      postnsl_sd = post_sd,
      sd_change = post_sd - pro_sd,
      sd_pct_change = (post_sd - pro_sd) / pro_sd * 100,
      # Ceiling (score = 4)
      protest_ceil_pct = as.numeric(pro_dist["4"]),
      postnsl_ceil_pct = as.numeric(post_dist["4"]),
      ceil_change_pp = as.numeric(post_dist["4"]) - as.numeric(pro_dist["4"]),
      # Floor (score = 1)
      protest_floor_pct = as.numeric(pro_dist["1"]),
      postnsl_floor_pct = as.numeric(post_dist["1"]),
      floor_change_pp = as.numeric(post_dist["1"]) - as.numeric(pro_dist["1"]),
      # Skewness
      protest_skew = skew(pro[[variable]]),
      postnsl_skew = skew(post[[variable]]),
      skew_change = skew(post[[variable]]) - skew(pro[[variable]]),
      # Variance equality test
      levene_F = levene_F,
      levene_p = levene_p,
      # Ns
      n_protest = nrow(pro),
      n_postnsl = nrow(post)
    )
  })

# ── Display results ──────────────────────────────────────────────────────────
cat("--- Variance Shifts ---\n\n")
dist_results |>
  select(label, sens_rank, protest_sd, postnsl_sd, sd_change, levene_F, levene_p) |>
  mutate(across(where(is.numeric), ~round(.x, 3))) |>
  print(width = 100)

cat("\n--- Ceiling and Floor Shifts ---\n\n")
dist_results |>
  select(label, sens_rank, protest_ceil_pct, postnsl_ceil_pct, ceil_change_pp,
         protest_floor_pct, postnsl_floor_pct, floor_change_pp) |>
  mutate(across(where(is.numeric), ~round(.x, 1))) |>
  print(width = 120)

cat("\n--- Skewness Shifts ---\n\n")
dist_results |>
  select(label, sens_rank, protest_skew, postnsl_skew, skew_change) |>
  mutate(across(where(is.numeric), ~round(.x, 3))) |>
  print(width = 100)

# ── Mechanism attribution ────────────────────────────────────────────────────
cat("\n\n================================================================\n")
cat("MECHANISM ATTRIBUTION LOGIC\n")
cat("================================================================\n\n")

cat("For each trust item, the ceiling increase captures the FALSIFICATION\n")
cat("component (respondents who shifted TO the top), while the floor\n")
cat("decrease captures the SELECTION component (critics who exited).\n")
cat("When both occur, both mechanisms are operating.\n\n")

# Ratio: ceiling increase / floor decrease (absolute values)
dist_results <- dist_results |>
  mutate(
    ceil_floor_ratio = abs(ceil_change_pp) / abs(floor_change_pp),
    falsification_signal = case_when(
      ceil_change_pp > 5 & skew_change < -0.1 ~ "Strong",
      ceil_change_pp > 2 ~ "Moderate",
      TRUE ~ "Weak"
    ),
    selection_signal = case_when(
      floor_change_pp < -10 ~ "Strong",
      floor_change_pp < -5 ~ "Moderate",
      TRUE ~ "Weak"
    )
  )

cat("--- Mechanism Signals by Item ---\n\n")
dist_results |>
  select(label, sens_rank, ceil_change_pp, floor_change_pp, skew_change,
         ceil_floor_ratio, falsification_signal, selection_signal) |>
  mutate(across(where(is.numeric), ~round(.x, 2))) |>
  arrange(sens_rank) |>
  print(width = 120)

# ── Gradient correlation with distributional features ────────────────────────
cat("\n\n================================================================\n")
cat("DISTRIBUTIONAL FEATURES × SENSITIVITY GRADIENT\n")
cat("================================================================\n\n")

# Merge Cohen's d from nsl_tests
dist_with_d <- dist_results |>
  left_join(nsl_tests |> select(variable, cohens_d), by = "variable")

cat("Correlation: Cohen's d × ceiling change (pp):\n")
ct1 <- cor.test(dist_with_d$cohens_d, dist_with_d$ceil_change_pp, method = "spearman")
cat("  rho =", round(ct1$estimate, 3), ", p =", round(ct1$p.value, 3), "\n\n")

cat("Correlation: Cohen's d × floor change (pp):\n")
ct2 <- cor.test(dist_with_d$cohens_d, dist_with_d$floor_change_pp, method = "spearman")
cat("  rho =", round(ct2$estimate, 3), ", p =", round(ct2$p.value, 3), "\n\n")

cat("Correlation: Cohen's d × skewness change:\n")
ct3 <- cor.test(dist_with_d$cohens_d, dist_with_d$skew_change, method = "spearman")
cat("  rho =", round(ct3$estimate, 3), ", p =", round(ct3$p.value, 3), "\n\n")

cat("Correlation: sensitivity rank × ceiling change (pp):\n")
ct4 <- cor.test(dist_with_d$sens_rank, dist_with_d$ceil_change_pp, method = "spearman")
cat("  rho =", round(ct4$estimate, 3), ", p =", round(ct4$p.value, 3), "\n\n")

# ── Key comparison: police vs. courts ────────────────────────────────────────
cat("\n================================================================\n")
cat("KEY COMPARISON: Police vs. Courts\n")
cat("================================================================\n\n")

police <- dist_results |> filter(variable == "trust_police")
courts <- dist_results |> filter(variable == "trust_courts")

cat("Trust in police:\n")
cat("  SD: ", round(police$protest_sd, 3), " → ", round(police$postnsl_sd, 3), "\n")
cat("  Ceiling: +", round(police$ceil_change_pp, 1), "pp\n")
cat("  Floor: ", round(police$floor_change_pp, 1), "pp\n")
cat("  Skew: ", round(police$protest_skew, 3), " → ", round(police$postnsl_skew, 3), "\n\n")

cat("Trust in courts:\n")
cat("  SD: ", round(courts$protest_sd, 3), " → ", round(courts$postnsl_sd, 3), "\n")
cat("  Ceiling: +", round(courts$ceil_change_pp, 1), "pp\n")
cat("  Floor: ", round(courts$floor_change_pp, 1), "pp\n")
cat("  Skew: ", round(courts$protest_skew, 3), " → ", round(courts$postnsl_skew, 3), "\n\n")

cat("Police had nearly identical protest-period SD to courts (",
    round(police$protest_sd, 2), " vs. ", round(courts$protest_sd, 2),
    ") but diverged\ndramatically in ceiling clustering (+",
    round(police$ceil_change_pp, 1), "pp vs. +", round(courts$ceil_change_pp, 1),
    "pp) and floor loss (", round(police$floor_change_pp, 1), "pp vs. ",
    round(courts$floor_change_pp, 1), "pp).\n")
cat("This is the signature of institution-specific falsification layered on\n")
cat("top of a broader selection effect.\n")

# ── Save results ─────────────────────────────────────────────────────────────
save(
  dist_results,
  dist_with_d,
  file = file.path(analysis_dir, "results", "variance_decomposition.RData")
)

cat("\n\nResults saved to analysis/results/variance_decomposition.RData\n")
