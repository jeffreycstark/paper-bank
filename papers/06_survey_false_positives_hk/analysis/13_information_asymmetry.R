###############################################################################
# 13_information_asymmetry.R
# Information Environment Confound Test: Does the sensitivity gradient hold
# differentially for politically interested respondents?
#
# Logic: If media restructuring (not falsification) drives the trust paradox,
# respondents who maintain high political interest — and thus are more likely
# to seek alternative information — should show a FLATTER gradient than those
# whose interest has collapsed. If instead the gradient is STEEPER for the
# high-interest group, this supports strategic compliance/falsification over
# simple belief revision.
#
# Run from: papers/06_survey_false_positives_hk/
# Outputs:  analysis/results/information_asymmetry.RData
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

hk5_analysis <- hk5 |>
  filter(period %in% c("Protest", "Post-NSL")) |>
  mutate(post_nsl = as.integer(period == "Post-NSL"))

# ── Weighted Cohen's d ───────────────────────────────────────────────────────
weighted_cohens_d <- function(x1, x2, w1 = NULL, w2 = NULL) {
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w2)) w2 <- rep(1, length(x2))
  m1 <- weighted.mean(x1, w1, na.rm = TRUE)
  m2 <- weighted.mean(x2, w2, na.rm = TRUE)
  v1 <- sum(w1 * (x1 - m1)^2, na.rm = TRUE) / (sum(w1, na.rm = TRUE) - 1)
  v2 <- sum(w2 * (x2 - m2)^2, na.rm = TRUE) / (sum(w2, na.rm = TRUE) - 1)
  n1 <- sum(!is.na(x1))
  n2 <- sum(!is.na(x2))
  pooled_sd <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
  (m2 - m1) / pooled_sd
}

# ── Create interest groups ───────────────────────────────────────────────────
# political_interest: 1-4 scale (higher = more interest)
# Median split: 1-2 = low, 3-4 = high
hk5_analysis <- hk5_analysis |>
  mutate(
    high_interest = case_when(
      political_interest >= 3 ~ "High interest",
      political_interest <= 2 ~ "Low interest",
      TRUE ~ NA_character_
    ),
    high_interest = factor(high_interest, levels = c("Low interest", "High interest"))
  )

cat("=== SAMPLE SIZES BY PERIOD × INTEREST GROUP ===\n\n")
sample_counts <- hk5_analysis |>
  filter(!is.na(high_interest)) |>
  count(period, high_interest)
print(sample_counts)

# ── Define gradient items with sensitivity classification ────────────────────
gradient_items <- tribble(
  ~variable,                    ~label,                        ~sensitivity, ~sens_rank,
  "trust_police",               "Trust in police",             "High",       1,
  "trust_national_government",  "Trust in nat. government",    "High",       2,
  "trust_president",            "Trust in president/CE",       "High",       3,
  "dem_free_speech",            "Free to speak without fear",  "High",       4,
  "trust_parliament",           "Trust in parliament",         "Medium",     5,
  "trust_courts",               "Trust in courts",             "Medium",     6,
  "trust_civil_service",        "Trust in civil service",      "Medium",     7,
  "dem_always_preferable",      "Democracy always preferable", "Low",        8,
  "democracy_suitability",      "Democracy suitability",       "Low",        9,
  "rich_poor_treated_equally",  "Rich/poor treated equally",   "Low",       10,
  "system_needs_change",        "System needs major change",   "Low",       11
)

gradient_items <- gradient_items |> filter(variable %in% names(hk5_analysis))

# ── Compute gradient by interest group ───────────────────────────────────────
cat("\n\n=== SENSITIVITY GRADIENT BY POLITICAL INTEREST ===\n\n")

gradient_by_interest <- gradient_items |>
  pmap_dfr(function(variable, label, sensitivity, sens_rank) {
    map_dfr(c("Low interest", "High interest"), function(grp) {
      dat <- hk5_analysis |> filter(high_interest == grp, !is.na(!!sym(variable)))
      pro <- dat |> filter(period == "Protest")
      post <- dat |> filter(period == "Post-NSL")

      if (nrow(pro) < 10 | nrow(post) < 10) return(NULL)

      d_val <- weighted_cohens_d(
        pro[[variable]], post[[variable]],
        pro$weight, post$weight
      )

      # OLS for p-value and CI
      m <- lm(as.formula(paste(variable, "~ post_nsl")), data = dat, weights = weight)
      coefs <- summary(m)$coefficients
      ci <- confint(m)["post_nsl", ]

      tibble(
        variable = variable,
        label = label,
        sensitivity = sensitivity,
        sens_rank = sens_rank,
        interest_group = grp,
        d = round(d_val, 3),
        coef = coefs["post_nsl", "Estimate"],
        se = coefs["post_nsl", "Std. Error"],
        p_value = coefs["post_nsl", "Pr(>|t|)"],
        ci_lo = ci[1],
        ci_hi = ci[2],
        n_protest = nrow(pro),
        n_postnsl = nrow(post)
      )
    })
  })

# Wide format for display
gradient_wide <- gradient_by_interest |>
  select(label, sensitivity, sens_rank, interest_group, d, p_value) |>
  pivot_wider(
    names_from = interest_group,
    values_from = c(d, p_value),
    names_sep = "_"
  ) |>
  mutate(
    d_diff = `d_High interest` - `d_Low interest`,
    sensitivity = factor(sensitivity, levels = c("High", "Medium", "Low"))
  ) |>
  arrange(sens_rank)

gradient_wide |>
  mutate(across(starts_with("d_"), ~round(.x, 3)),
         across(starts_with("p_"), ~round(.x, 4))) |>
  print(width = 120)

# ── Gradient slopes by group ─────────────────────────────────────────────────
cat("\n\n=== GRADIENT SLOPE BY INTEREST GROUP ===\n")
cat("(Mean d for high-sensitivity items minus mean d for low-sensitivity items)\n\n")

gradient_slopes <- gradient_by_interest |>
  filter(sensitivity %in% c("High", "Low")) |>
  group_by(interest_group, sensitivity) |>
  summarise(mean_d = mean(d), .groups = "drop") |>
  pivot_wider(names_from = sensitivity, values_from = mean_d) |>
  mutate(gradient_slope = High - Low)

print(gradient_slopes)

# ── Gradient correlations by group ───────────────────────────────────────────
cat("\n\n=== GRADIENT CORRELATION (rank × d) BY INTEREST GROUP ===\n\n")

gradient_cors <- gradient_by_interest |>
  group_by(interest_group) |>
  summarise(
    r = cor(sens_rank, d, method = "spearman"),
    n_items = n(),
    .groups = "drop"
  )

print(gradient_cors)

# ── Formal test: three-way interaction ───────────────────────────────────────
cat("\n\n=== FORMAL TEST: Three-Way Interaction ===\n")
cat("Model: value ~ post_nsl × high_sensitivity × high_interest\n\n")

long_data <- gradient_items |>
  pmap_dfr(function(variable, label, sensitivity, sens_rank) {
    hk5_analysis |>
      filter(!is.na(!!sym(variable)), !is.na(high_interest)) |>
      transmute(
        value = !!sym(variable),
        item = label,
        sensitivity = sensitivity,
        sens_rank = sens_rank,
        post_nsl = post_nsl,
        high_interest = high_interest,
        weight = weight
      )
  }) |>
  mutate(
    high_sens = as.integer(sensitivity == "High"),
    high_int = as.integer(high_interest == "High interest")
  )

m_threeway <- lm(value ~ post_nsl * high_sens * high_int,
                  data = long_data, weights = weight)

cat("Coefficients:\n")
print(round(summary(m_threeway)$coefficients, 4))

threeway_coef <- summary(m_threeway)$coefficients["post_nsl:high_sens:high_int", ]
threeway_ci <- confint(m_threeway)["post_nsl:high_sens:high_int", ]

cat("\n\nThree-way interaction (post_nsl × high_sens × high_int):\n")
cat("  β =", round(threeway_coef["Estimate"], 3), "\n")
cat("  SE =", round(threeway_coef["Std. Error"], 3), "\n")
cat("  t =", round(threeway_coef["t value"], 2), "\n")
cat("  p =", round(threeway_coef["Pr(>|t|)"], 4), "\n")
cat("  95% CI: [", round(threeway_ci[1], 3), ",", round(threeway_ci[2], 3), "]\n")

# ── Key items: trust in police and democracy suitability by interest ─────────
cat("\n\n=== KEY ITEM COMPARISON ===\n\n")

key_items <- gradient_by_interest |>
  filter(variable %in% c("trust_police", "trust_national_government",
                          "trust_president", "democracy_suitability")) |>
  select(label, interest_group, d, p_value, n_protest, n_postnsl) |>
  arrange(label, interest_group)

print(key_items)

# ── dem_free_speech anomaly note ─────────────────────────────────────────────
cat("\n\n=== NOTE ON dem_free_speech ===\n")
fs_rows <- gradient_by_interest |> filter(variable == "dem_free_speech")
cat("Free to speak without fear:\n")
cat("  Low interest:  d =", fs_rows$d[fs_rows$interest_group == "Low interest"], "\n")
cat("  High interest: d =", fs_rows$d[fs_rows$interest_group == "High interest"], "\n")
cat("  Interpretation: Politically interested respondents find it harder to\n")
cat("  falsify on an item that directly contradicts their lived experience.\n")
cat("  This is consistent with 'experiential anchoring' — items closer to\n")
cat("  direct personal experience are harder to strategically inflate.\n")

# ── Save results ─────────────────────────────────────────────────────────────
save(
  gradient_by_interest,
  gradient_wide,
  gradient_slopes,
  gradient_cors,
  m_threeway,
  threeway_coef,
  threeway_ci,
  sample_counts,
  long_data,
  file = file.path(analysis_dir, "results", "information_asymmetry.RData")
)

cat("\n\nResults saved to analysis/results/information_asymmetry.RData\n")
