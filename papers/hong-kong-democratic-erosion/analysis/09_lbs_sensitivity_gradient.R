## ──────────────────────────────────────────────────────────────────────────────
## 09 — LBS Sensitivity Gradient: Venezuela & Nicaragua
##
## Purpose: Extend sensitivity gradient analysis to Latin America using
## Latinobarómetro data. Venezuela brackets the 2017 constituent assembly
## crisis; Nicaragua brackets the 2018 April crackdown. Three waves
## (2015, 2016, 2018) enable tracking gradient emergence across time.
##
## Data: Harmonized LBS from survey-data-prep pipeline.
##   Trust items: 1-4, higher = more trust/confidence.
##   Democracy items: 1-4, higher = more satisfied/supportive.
##   Missing values coded as NA.
##
## Output: results/venezuela_gradient.RData
##         results/nicaragua_gradient.RData
##         results/lbs_sensitivity_gradient.RData
##         figures/lbs_sensitivity_gradient_*.pdf
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

# ─── 1. Load data ────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))
lbs_path <- lbs_harmonized_path

stopifnot("Harmonized LBS RDS not found" = file.exists(lbs_path))

lbs <- readRDS(lbs_path)
cat("Harmonized LBS:", nrow(lbs), "obs,", n_distinct(lbs$country), "countries,",
    n_distinct(lbs$wave), "waves\n")

# ─── 2. Variable mapping ────────────────────────────────────────────────────

# 10 core items available across waves 1-3 (2015, 2016, 2018).
# Sensitivity ranking: 1 = most sensitive (coercive institutions),
# 10 = least sensitive (control).
# All items are 1-4 scale.

item_map <- tribble(
  ~label,                ~var,                      ~sensitivity_rank, ~category,
  "Conf. Police",        "trust_police",            1L,                "High",
  "Conf. Government",    "trust_government",        2L,                "High",
  "Conf. Armed Forces",  "trust_armed_forces",      3L,                "High",
  "Conf. Parliament",    "trust_parliament",        4L,                "Medium",
  "Conf. Courts",        "trust_courts",            5L,                "Medium",
  "Conf. Elections",     "trust_elections",          6L,                "Medium",
  "Conf. Pol. Parties",  "trust_political_parties", 7L,                "Medium",
  "Dem. Satisfaction",   "dem_satisfaction",         8L,                "Low",
  "Dem. Best System",    "dem_best_system",          9L,                "Low",
  "Conf. Churches",      "trust_churches",          10L,               "Control"
)

# ─── 3. Filter to VEN and NIC, waves 1-3 ────────────────────────────────────

countries <- c(Venezuela = "VEN", Nicaragua = "NIC")
target_waves <- 1:3
wave_years <- c(`1` = 2015L, `2` = 2016L, `3` = 2018L)

lbs_sub <- lbs |>
  filter(country %in% countries, wave %in% target_waves)

cat("\nSubset:", nrow(lbs_sub), "obs\n")
cat("Country-wave counts:\n")
print(table(lbs_sub$country, lbs_sub$wave))

# ─── 4. Per-item stats: N, NR rate, mean ────────────────────────────────────

compute_item_stats <- function(data, country_iso, country_name) {
  results_list <- list()
  for (w in target_waves) {
    w_data <- data |> filter(country == country_iso, wave == w)
    n_total <- nrow(w_data)

    w_results <- item_map |>
      rowwise() |>
      mutate(
        country = country_name,
        country_iso = !!country_iso,
        wave = !!w,
        wave_year = wave_years[as.character(!!w)],
        n_total = !!n_total,
        n_valid = sum(!is.na(w_data[[var]])),
        nr_rate = (n_total - n_valid) / n_total * 100,
        item_mean = mean(w_data[[var]], na.rm = TRUE)
      ) |>
      ungroup()

    results_list[[length(results_list) + 1]] <- w_results
  }
  bind_rows(results_list)
}

all_stats <- bind_rows(
  compute_item_stats(lbs_sub, "VEN", "Venezuela"),
  compute_item_stats(lbs_sub, "NIC", "Nicaragua")
)

cat("\n=== Item stats preview ===\n")
all_stats |>
  filter(country == "Venezuela", wave == 1) |>
  select(label, category, n_valid, nr_rate, item_mean) |>
  print(n = 10)

# ─── 5. Cohen's d for each wave pair ────────────────────────────────────────

# Compute Cohen's d (pooled SD) and two-sample t-test p-value
compute_cohens_d <- function(data, country_iso, w_before, w_after) {
  d_before <- data |> filter(country == country_iso, wave == w_before)
  d_after  <- data |> filter(country == country_iso, wave == w_after)

  item_map |>
    rowwise() |>
    mutate(
      x1 = list(d_before[[var]]),
      x2 = list(d_after[[var]]),
      x1_clean = list(x1[!is.na(x1)]),
      x2_clean = list(x2[!is.na(x2)]),
      w_before_n = length(x1_clean),
      w_after_n = length(x2_clean),
      w_before_mean = mean(x1_clean),
      w_after_mean = mean(x2_clean),
      delta = w_after_mean - w_before_mean,
      # Pooled SD
      sd1 = sd(x1_clean),
      sd2 = sd(x2_clean),
      pooled_sd = sqrt(((w_before_n - 1) * sd1^2 + (w_after_n - 1) * sd2^2) /
                         (w_before_n + w_after_n - 2)),
      cohens_d = delta / pooled_sd,
      # t-test p-value
      p_value = tryCatch(
        t.test(x1_clean, x2_clean)$p.value,
        error = function(e) NA_real_
      )
    ) |>
    select(label, var, sensitivity_rank, category,
           w_before_mean, w_after_mean, delta, cohens_d, p_value,
           w_before_n, w_after_n) |>
    ungroup()
}

# Wave pairs
wave_pairs <- list(
  list(before = 1, after = 2, label = "w1_w2"),
  list(before = 2, after = 3, label = "w2_w3"),
  list(before = 1, after = 3, label = "w1_w3")
)

gradient_results <- list()

for (cty_name in names(countries)) {
  cty_iso <- countries[[cty_name]]
  gradient_results[[cty_name]] <- list()

  for (wp in wave_pairs) {
    res <- compute_cohens_d(lbs_sub, cty_iso, wp$before, wp$after)
    gradient_results[[cty_name]][[wp$label]] <- res

    cat(sprintf("\n=== %s %s (w%d→w%d, %d→%d) ===\n",
                cty_name, wp$label, wp$before, wp$after,
                wave_years[as.character(wp$before)],
                wave_years[as.character(wp$after)]))
    res |>
      select(label, category, delta, cohens_d, p_value) |>
      mutate(across(where(is.numeric), ~round(.x, 4))) |>
      print(n = 10)
  }
}

# ─── 6. Gradient correlations ───────────────────────────────────────────────

# For each country × wave-pair, correlate sensitivity_rank with Cohen's d.
# Expect negative correlation: higher sensitivity → more distortion (positive d
# for trust = "recovery", or more suppressed responses).

gradient_cors <- list()

for (cty_name in names(countries)) {
  gradient_cors[[cty_name]] <- list()

  for (wp in wave_pairs) {
    res <- gradient_results[[cty_name]][[wp$label]]

    # All 10 items
    ct_all_p  <- cor.test(res$sensitivity_rank, res$cohens_d, method = "pearson")
    ct_all_s  <- cor.test(res$sensitivity_rank, res$cohens_d, method = "spearman",
                          exact = FALSE)

    # Trust items only (7 items, excluding dem + control)
    res_trust <- res |> filter(category %in% c("High", "Medium"))
    ct_trust_p  <- cor.test(res_trust$sensitivity_rank, res_trust$cohens_d, method = "pearson")
    ct_trust_s  <- cor.test(res_trust$sensitivity_rank, res_trust$cohens_d, method = "spearman",
                            exact = FALSE)

    # Extract as plain scalars
    r_ap  <- as.numeric(ct_all_p$estimate)
    r_app <- as.numeric(ct_all_p$p.value)
    r_as  <- as.numeric(ct_all_s$estimate)
    r_asp <- as.numeric(ct_all_s$p.value)
    r_tp  <- as.numeric(ct_trust_p$estimate)
    r_tpp <- as.numeric(ct_trust_p$p.value)
    r_ts  <- as.numeric(ct_trust_s$estimate)
    r_tsp <- as.numeric(ct_trust_s$p.value)

    gradient_cors[[cty_name]][[wp$label]] <- tibble(
      country = cty_name,
      wave_pair = wp$label,
      w_before = wp$before, w_after = wp$after,
      r_all_pearson = r_ap, r_all_pearson_p = r_app,
      r_all_spearman = r_as, r_all_spearman_p = r_asp,
      r_trust_pearson = r_tp, r_trust_pearson_p = r_tpp,
      r_trust_spearman = r_ts, r_trust_spearman_p = r_tsp
    )

    cat(sprintf("\n%s %s gradient correlation (all items): r = %.3f (p = %.3f)\n",
                cty_name, wp$label, r_ap, r_app))
    cat(sprintf("%s %s gradient correlation (trust only): r = %.3f (p = %.3f)\n",
                cty_name, wp$label, r_tp, r_tpp))
  }
}

gradient_cor_table <- bind_rows(unlist(gradient_cors, recursive = FALSE))

# ─── 7. Summary tables by sensitivity category ──────────────────────────────

category_summary <- all_stats |>
  group_by(country, category, wave) |>
  summarise(
    avg_nr = mean(nr_rate, na.rm = TRUE),
    avg_mean = mean(item_mean, na.rm = TRUE),
    avg_n = mean(n_valid, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(category = factor(category, levels = c("High", "Medium", "Low", "Control"))) |>
  arrange(country, wave, category)

cat("\n=== Category summary ===\n")
print(category_summary, n = 30)

# ─── 8. Statistical tests ───────────────────────────────────────────────────

# Respondent-level chi-squared on NR: pool item-responses within category,
# test whether sensitivity category predicts missingness.
# Also compute odds ratios for High vs Low.

stat_tests <- list()

for (cty_name in names(countries)) {
  cty_iso <- countries[[cty_name]]
  stat_tests[[cty_name]] <- list()

  for (w in target_waves) {
    w_data <- lbs_sub |> filter(country == cty_iso, wave == w)
    n_resp <- nrow(w_data)

    # Count responses and NR per category
    cat_counts <- item_map |>
      rowwise() |>
      mutate(
        n_responded = sum(!is.na(w_data[[var]])),
        n_missing = n_resp - n_responded
      ) |>
      ungroup() |>
      group_by(category) |>
      summarise(
        total_responded = sum(n_responded),
        total_missing = sum(n_missing),
        n_items = n(),
        .groups = "drop"
      )

    # Chi-squared test on the 4x2 contingency table
    cont_table <- matrix(
      c(cat_counts$total_responded, cat_counts$total_missing),
      nrow = 4, ncol = 2,
      dimnames = list(cat_counts$category, c("Responded", "Missing"))
    )

    chi_test <- chisq.test(cont_table)

    # Odds ratio: High vs Low NR
    high_row <- cat_counts |> filter(category == "High")
    low_row  <- cat_counts |> filter(category == "Low")
    or_val <- (high_row$total_missing / high_row$total_responded) /
              (low_row$total_missing / low_row$total_responded)
    log_or_se <- sqrt(1/high_row$total_responded + 1/high_row$total_missing +
                      1/low_row$total_responded + 1/low_row$total_missing)
    or_ci_lo <- exp(log(or_val) - 1.96 * log_or_se)
    or_ci_hi <- exp(log(or_val) + 1.96 * log_or_se)

    stat_tests[[cty_name]][[paste0("w", w)]] <- list(
      wave = w,
      chi_test = tibble(
        country = cty_name, wave = w,
        chi_sq = chi_test$statistic,
        df = chi_test$parameter,
        p_value = chi_test$p.value
      ),
      odds_ratio = tibble(
        country = cty_name, wave = w,
        comparison = "High vs Low",
        or = or_val, or_ci_lo = or_ci_lo, or_ci_hi = or_ci_hi
      ),
      contingency = cont_table
    )

    cat(sprintf("\n%s W%d: chi-sq = %.1f (df=%d, p=%.2e), OR(High/Low) = %.2f [%.2f, %.2f]\n",
                cty_name, w, chi_test$statistic, chi_test$parameter, chi_test$p.value,
                or_val, or_ci_lo, or_ci_hi))
  }
}

# ─── 9. Figures ──────────────────────────────────────────────────────────────

# Figure 1: Gradient dot-plots with OLS fit line (one per country, w1→w3)
# X-axis: sensitivity rank, Y-axis: Cohen's d

for (cty_name in names(countries)) {
  res <- gradient_results[[cty_name]][["w1_w3"]]

  p <- ggplot(res, aes(x = sensitivity_rank, y = cohens_d)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", se = TRUE, color = "steelblue", fill = "steelblue",
                alpha = 0.15, linewidth = 0.8) +
    geom_point(aes(color = category), size = 3.5) +
    geom_text(aes(label = label), hjust = -0.1, vjust = -0.5, size = 3) +
    scale_color_manual(
      values = c(High = "#CC3311", Medium = "#EE7733", Low = "#0077BB", Control = "#009988"),
      name = "Sensitivity"
    ) +
    scale_x_continuous(breaks = 1:10) +
    labs(
      x = "Sensitivity rank (1 = most sensitive)",
      y = "Cohen's d (2015 \u2192 2018)",
      title = sprintf("%s: Sensitivity gradient (LBS 2015\u20132018)", cty_name),
      subtitle = sprintf("Pearson r = %.3f",
                         gradient_cors[[cty_name]][["w1_w3"]]$r_all_pearson[[1]])
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  fname <- tolower(cty_name)
  ggsave(sprintf("figures/lbs_%s_gradient.pdf", fname), p, width = 9, height = 6)
  ggsave(sprintf("figures/lbs_%s_gradient.png", fname), p, width = 9, height = 6, dpi = 300)
  cat(sprintf("\u2713 Saved gradient figure for %s\n", cty_name))
}

# Figure 2: Combined NR rate comparison across waves
nr_fig_data <- all_stats |>
  mutate(
    category = factor(category, levels = c("High", "Medium", "Low", "Control")),
    wave_label = paste0("W", wave, " (", wave_year, ")")
  )

p_nr <- ggplot(nr_fig_data, aes(x = nr_rate, y = fct_reorder(label, -sensitivity_rank),
                                 fill = category)) +
  geom_col(width = 0.7, alpha = 0.85) +
  facet_grid(country ~ wave_label) +
  scale_fill_manual(
    values = c(High = "#CC3311", Medium = "#EE7733", Low = "#0077BB", Control = "#009988"),
    name = "Sensitivity"
  ) +
  labs(
    x = "Item non-response rate (%)",
    y = NULL,
    title = "LBS Sensitivity Gradient: Non-response rates",
    subtitle = "Venezuela and Nicaragua, 2015\u20132018"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/lbs_sensitivity_gradient_nr.pdf", p_nr, width = 12, height = 8)
ggsave("figures/lbs_sensitivity_gradient_nr.png", p_nr, width = 12, height = 8, dpi = 300)

# ─── 10. Save results ───────────────────────────────────────────────────────

# Venezuela gradient (turkey_gradient contract)
ven_gradient_results <- gradient_results[["Venezuela"]][["w1_w3"]]
ven_gradient_r_all   <- gradient_cors[["Venezuela"]][["w1_w3"]]$r_all_pearson
ven_gradient_r_trust <- gradient_cors[["Venezuela"]][["w1_w3"]]$r_trust_pearson

save(ven_gradient_results, ven_gradient_r_all, ven_gradient_r_trust,
     file = "results/venezuela_gradient.RData")
cat("\u2713 Saved results/venezuela_gradient.RData\n")

# Nicaragua gradient
nic_gradient_results <- gradient_results[["Nicaragua"]][["w1_w3"]]
nic_gradient_r_all   <- gradient_cors[["Nicaragua"]][["w1_w3"]]$r_all_pearson
nic_gradient_r_trust <- gradient_cors[["Nicaragua"]][["w1_w3"]]$r_trust_pearson

save(nic_gradient_results, nic_gradient_r_all, nic_gradient_r_trust,
     file = "results/nicaragua_gradient.RData")
cat("\u2713 Saved results/nicaragua_gradient.RData\n")

# Combined LBS results
lbs_gradient_all_stats     <- all_stats
lbs_gradient_results       <- gradient_results
lbs_gradient_cors          <- gradient_cor_table
lbs_gradient_category_summary <- category_summary
lbs_gradient_stat_tests    <- stat_tests

save(lbs_gradient_all_stats, lbs_gradient_results,
     lbs_gradient_cors, lbs_gradient_category_summary,
     lbs_gradient_stat_tests,
     file = "results/lbs_sensitivity_gradient.RData")
cat("\u2713 Saved results/lbs_sensitivity_gradient.RData\n")
cat("\u2713 Figures saved to figures/\n")

# ─── 11. Print key findings ─────────────────────────────────────────────────

cat("\n=== KEY FINDINGS ===\n")

for (cty_name in names(countries)) {
  cat(sprintf("\n--- %s ---\n", cty_name))

  for (wp in wave_pairs) {
    res <- gradient_results[[cty_name]][[wp$label]]
    cors <- gradient_cors[[cty_name]][[wp$label]]

    cat(sprintf("\n  %s (%d\u2192%d):\n",
                wp$label, wave_years[as.character(wp$before)],
                wave_years[as.character(wp$after)]))

    # Category means of Cohen's d
    cat_d <- res |>
      group_by(category) |>
      summarise(avg_d = mean(cohens_d, na.rm = TRUE), .groups = "drop")
    for (i in seq_len(nrow(cat_d))) {
      cat(sprintf("    %s avg d = %+.3f\n", cat_d$category[i], cat_d$avg_d[i]))
    }

    cat(sprintf("    Gradient r (all) = %.3f (p = %.3f)\n",
                cors$r_all_pearson, cors$r_all_pearson_p))
    cat(sprintf("    Gradient r (trust) = %.3f (p = %.3f)\n",
                cors$r_trust_pearson, cors$r_trust_pearson_p))
  }
}

cat("\n=== Gradient correlation summary ===\n")
gradient_cor_table |>
  select(country, wave_pair, r_all_pearson, r_all_pearson_p,
         r_trust_pearson, r_trust_pearson_p) |>
  mutate(across(where(is.numeric), ~round(.x, 4))) |>
  print(n = 20)
