## ──────────────────────────────────────────────────────────────────────────────
## 11 — Afrobarometer Sensitivity Gradient: Burkina Faso Coup (Sep 30, 2022)
##
## Purpose: Extend the sensitivity gradient framework to a third continent,
## fourth survey program, and distinct autocratization mechanism (military coup).
## Afrobarometer Round 9 fieldwork in Burkina Faso straddled the September 30
## coup, enabling a within-wave pre/post comparison analogous to the Hong Kong
## NSL analysis.
##
## Design: Following Brailey, Harding, and Isbell (2024), we drop the
## transition period (Sep 30 – Oct 2) and compare respondents interviewed
## Sep 20–29 (pre-coup) with those interviewed Oct 4–12 (post-coup).
##
## Tests:
##   (1) Sensitivity gradient: correlation between coercive salience rank
##       and Cohen's d (pre→post mean shift)
##   (2) Non-response gradient: differential item non-response by sensitivity
##   (3) Critical citizens correlation: dem_support × dem_satisfaction pre/post
##   (4) Trust–democracy divergence: trust increases vs. democratic evaluation
##
## Data: Afrobarometer R9 harmonized from survey-data-prep pipeline
## Output: results/afro_sensitivity_gradient.RData
##         figures/afro_sensitivity_gradient_*.pdf
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

# ─── 1. Load data ────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))

# afro_harmonized_path should be defined in _data_config.R
# If not, use direct path
if (!exists("afro_harmonized_path")) {
  afro_harmonized_path <- file.path(
    "/Users/jeffreystark/Development/Research/survey-data-prep",
    "data/processed/afro_harmonized.rds"
  )
}

afro <- readRDS(afro_harmonized_path)
cat("Afrobarometer R9:", nrow(afro), "obs,",
    n_distinct(afro$country), "countries\n")

# ─── 2. Extract Burkina Faso and create pre/post split ───────────────────────

bfa <- afro |> filter(country == "BFA")
cat("Burkina Faso: n =", nrow(bfa), "\n")
cat("Fieldwork dates:", as.character(range(bfa$int_date)), "\n\n")

# Coup date: September 30, 2022
coup_date <- as.Date("2022-09-30")

# Following Brailey et al. (2024): drop Sep 30 – Oct 2 transition period
bfa <- bfa |>
  mutate(
    period = case_when(
      int_date < coup_date ~ "Pre-coup",
      int_date > as.Date("2022-10-02") ~ "Post-coup",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period))

cat("Pre-coup (Sep 20–29):", sum(bfa$period == "Pre-coup"), "\n")
cat("Post-coup (Oct 4–12):", sum(bfa$period == "Post-coup"), "\n")
cat("Dropped (Sep 30–Oct 2):", nrow(filter(afro, country == "BFA")) - nrow(bfa), "\n\n")

pre  <- bfa |> filter(period == "Pre-coup")
post <- bfa |> filter(period == "Post-coup")

# ─── 3. Variable mapping ────────────────────────────────────────────────────
# Sensitivity ranking: 1 = most coercive/sensitive, higher = less sensitive
# Trust items: 1-4 scale, higher = more trust
# Dem items: various scales

item_map <- tribble(
  ~label,                ~var,                        ~sensitivity,  ~sensitivity_rank, ~scale,
  # High sensitivity: coercive institutions
  "Trust army",          "trust_armed_forces",        "Trust",       1,                 "1-4",
  "Trust president",     "trust_president",           "Trust",       2,                 "1-4",
  "Trust police",        "trust_police",              "Trust",       3,                 "1-4",
  # Medium sensitivity: political institutions
  "Trust courts",        "trust_courts",              "Trust",       4,                 "1-4",
  "Trust elections",     "trust_elections",            "Trust",       5,                 "1-4",
  "Trust churches",      "trust_churches",            "Trust",       6,                 "1-4",
  # Low sensitivity: democratic evaluations
  "Dem. satisfaction",   "dem_satisfaction",           "Democracy",   7,                 "1-4",
  "How democratic",      "dem_how_democratic_qual",    "Democracy",   8,                 "1-4",
  "Dem. preferable",     "dem_support_preferable",     "Democracy",   9,                 "1-3"
)

# ─── 4. Weighted statistics ─────────────────────────────────────────────────

weighted_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  if (sum(valid) < 10) return(NA_real_)
  sum(x[valid] * w[valid]) / sum(w[valid])
}

weighted_sd <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  if (sum(valid) < 10) return(NA_real_)
  xv <- x[valid]; wv <- w[valid]
  mu <- sum(xv * wv) / sum(wv)
  sqrt(sum(wv * (xv - mu)^2) / (sum(wv) - 1))
}

# ─── 5. Compute means, effect sizes, and non-response ───────────────────────

results <- item_map |>
  rowwise() |>
  mutate(
    # Pre-coup stats
    pre_n = sum(!is.na(pre[[var]]), na.rm = TRUE),
    pre_nr = (sum(is.na(pre[[var]])) / nrow(pre)) * 100,
    pre_mean = weighted_mean(pre[[var]], pre$weight),
    pre_sd = weighted_sd(pre[[var]], pre$weight),
    # Post-coup stats
    post_n = sum(!is.na(post[[var]]), na.rm = TRUE),
    post_nr = (sum(is.na(post[[var]])) / nrow(post)) * 100,
    post_mean = weighted_mean(post[[var]], post$weight),
    post_sd = weighted_sd(post[[var]], post$weight),
    # Changes
    nr_change = post_nr - pre_nr,
    mean_change = post_mean - pre_mean,
    # Cohen's d
    pooled_sd = sqrt(((pre_n - 1) * pre_sd^2 + (post_n - 1) * post_sd^2) /
                       (pre_n + post_n - 2)),
    cohens_d = if_else(pooled_sd > 0, mean_change / pooled_sd, NA_real_),
    # p-value
    p_value = tryCatch(
      t.test(post[[var]], pre[[var]])$p.value,
      error = function(e) NA_real_
    )
  ) |>
  ungroup()

# ─── 6. Print results ───────────────────────────────────────────────────────

cat("═══ BURKINA FASO: PRE/POST COUP COMPARISON ═══\n\n")

results |>
  select(label, sensitivity, pre_mean, post_mean, mean_change, cohens_d, p_value,
         pre_nr, post_nr, nr_change) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  print(n = 20, width = Inf)

# ─── 7. Sensitivity gradient correlation ────────────────────────────────────

cat("\n═══ SENSITIVITY GRADIENT ═══\n")

# All items
grad_all <- cor.test(results$sensitivity_rank, results$cohens_d, method = "pearson")
grad_all_rho <- cor.test(results$sensitivity_rank, results$cohens_d,
                         method = "spearman", exact = FALSE)

cat(sprintf("\nAll items (n=%d):\n  Pearson r = %.3f (p = %.3f)\n  Spearman rho = %.3f (p = %.3f)\n",
            nrow(results),
            grad_all$estimate, grad_all$p.value,
            grad_all_rho$estimate, grad_all_rho$p.value))

# Trust items only
trust_only <- results |> filter(sensitivity == "Trust")
if (nrow(trust_only) >= 3) {
  grad_trust <- cor.test(trust_only$sensitivity_rank, trust_only$cohens_d,
                         method = "pearson")
  cat(sprintf("\nTrust items only (n=%d):\n  Pearson r = %.3f (p = %.3f)\n",
              nrow(trust_only), grad_trust$estimate, grad_trust$p.value))
}

# ─── 8. Trust–democracy divergence ──────────────────────────────────────────

cat("\n═══ TRUST–DEMOCRACY DIVERGENCE ═══\n")

trust_avg_d <- results |> filter(sensitivity == "Trust") |>
  summarise(avg_d = mean(cohens_d, na.rm = TRUE)) |> pull(avg_d)
dem_avg_d <- results |> filter(sensitivity == "Democracy") |>
  summarise(avg_d = mean(cohens_d, na.rm = TRUE)) |> pull(avg_d)

cat(sprintf("  Avg trust Cohen's d: %+.3f\n", trust_avg_d))
cat(sprintf("  Avg democracy Cohen's d: %+.3f\n", dem_avg_d))
cat(sprintf("  Divergence (trust - dem): %+.3f\n", trust_avg_d - dem_avg_d))

# ─── 9. Critical citizens correlation ───────────────────────────────────────

cat("\n═══ CRITICAL CITIZENS CORRELATION ═══\n")
cat("(dem_support_preferable × dem_satisfaction)\n")

# Pre-coup
pre_valid <- pre |> filter(!is.na(dem_support_preferable) & !is.na(dem_satisfaction))
if (nrow(pre_valid) > 10) {
  cc_pre <- cor.test(pre_valid$dem_support_preferable, pre_valid$dem_satisfaction,
                     method = "pearson")
  cat(sprintf("\n  Pre-coup:  r = %.3f (p = %.3f, n = %d)\n",
              cc_pre$estimate, cc_pre$p.value, nrow(pre_valid)))
}

# Post-coup
post_valid <- post |> filter(!is.na(dem_support_preferable) & !is.na(dem_satisfaction))
if (nrow(post_valid) > 10) {
  cc_post <- cor.test(post_valid$dem_support_preferable, post_valid$dem_satisfaction,
                      method = "pearson")
  cat(sprintf("  Post-coup: r = %.3f (p = %.3f, n = %d)\n",
              cc_post$estimate, cc_post$p.value, nrow(post_valid)))
}

# ─── 10. Non-response gradient test ─────────────────────────────────────────

cat("\n═══ NON-RESPONSE GRADIENT ═══\n")

# Post-coup NR by sensitivity category
nr_by_cat <- results |>
  group_by(sensitivity) |>
  summarise(
    avg_post_nr = mean(post_nr, na.rm = TRUE),
    avg_pre_nr = mean(pre_nr, na.rm = TRUE),
    avg_nr_change = mean(nr_change, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nPost-coup non-response by category:\n")
print(nr_by_cat)

# Respondent-level chi-squared: pool item responses by category
n_post <- nrow(post)

cat_counts <- item_map |>
  rowwise() |>
  mutate(
    n_responded = sum(!is.na(post[[var]])),
    n_missing = n_post - n_responded
  ) |>
  ungroup() |>
  group_by(sensitivity) |>
  summarise(
    total_responded = sum(n_responded),
    total_missing = sum(n_missing),
    .groups = "drop"
  )

cont_table <- matrix(
  c(cat_counts$total_responded, cat_counts$total_missing),
  nrow = nrow(cat_counts), ncol = 2,
  dimnames = list(cat_counts$sensitivity, c("Responded", "Missing"))
)

cat("\nPost-coup contingency table (pooled):\n")
print(cont_table)

chi_test <- chisq.test(cont_table)
cat(sprintf("Chi-squared: %.1f, df=%d, p = %.4f\n",
            chi_test$statistic, chi_test$parameter, chi_test$p.value))

# Democracy vs Trust odds ratio
dem_row <- which(rownames(cont_table) == "Democracy")
trust_row <- which(rownames(cont_table) == "Trust")
if (length(dem_row) == 1 && length(trust_row) == 1) {
  sub <- cont_table[c(dem_row, trust_row), ]
  or_val <- (sub[1, 2] / sub[1, 1]) / (sub[2, 2] / sub[2, 1])
  log_or_se <- sqrt(1/sub[1,1] + 1/sub[1,2] + 1/sub[2,1] + 1/sub[2,2])
  cat(sprintf("OR (Democracy vs Trust NR): %.2f [%.2f, %.2f]\n",
              or_val, exp(log(or_val) - 1.96*log_or_se),
              exp(log(or_val) + 1.96*log_or_se)))
}

# ─── 11. Figures ─────────────────────────────────────────────────────────────

# Figure: Mean shifts (pre→post coup) by item
fig_data <- results |>
  mutate(
    sensitivity = factor(sensitivity, levels = c("Trust", "Democracy")),
    label = fct_reorder(label, cohens_d)
  )

p_shifts <- ggplot(fig_data, aes(x = cohens_d, y = label, fill = sensitivity)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(
    values = c(Trust = "#0077BB", Democracy = "#CC3311"),
    name = "Item category"
  ) +
  labs(
    x = "Cohen's d (pre → post coup)\n← Decreased | Increased →",
    y = NULL,
    title = "Burkina Faso: Institutional trust and democratic evaluations",
    subtitle = "Pre-coup (Sep 20–29) vs. Post-coup (Oct 4–12), 2022"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/afro_bfa_mean_shifts.pdf", p_shifts, width = 8, height = 5)
ggsave("figures/afro_bfa_mean_shifts.png", p_shifts, width = 8, height = 5, dpi = 300)

# Figure: Sensitivity gradient scatterplot
p_gradient <- ggplot(results,
                     aes(x = sensitivity_rank, y = cohens_d,
                         color = sensitivity, label = label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", linetype = "dashed") +
  geom_text(nudge_y = 0.02, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c(Trust = "#0077BB", Democracy = "#CC3311"),
    name = "Category"
  ) +
  labs(
    x = "Sensitivity rank (1 = most coercive)",
    y = "Cohen's d (pre → post coup)",
    title = "Sensitivity gradient: Burkina Faso",
    subtitle = sprintf("r = %.2f, ρ = %.2f",
                        grad_all$estimate, grad_all_rho$estimate)
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("figures/afro_bfa_gradient.pdf", p_gradient, width = 7, height = 5)
ggsave("figures/afro_bfa_gradient.png", p_gradient, width = 7, height = 5, dpi = 300)

# ─── 12. Save results ───────────────────────────────────────────────────────

afro_gradient_results <- list(
  item_results = results,
  gradient_pearson = grad_all,
  gradient_spearman = grad_all_rho,
  trust_avg_d = trust_avg_d,
  dem_avg_d = dem_avg_d,
  cc_pre = if (exists("cc_pre")) cc_pre else NULL,
  cc_post = if (exists("cc_post")) cc_post else NULL,
  nr_by_category = nr_by_cat,
  chi_test = chi_test,
  n_pre = nrow(pre),
  n_post = nrow(post),
  coup_date = coup_date,
  item_map = item_map
)

save(afro_gradient_results,
     file = "results/afro_sensitivity_gradient.RData")

cat("\n✓ Results saved to results/afro_sensitivity_gradient.RData\n")
cat("✓ Figures saved to figures/\n")
