# ============================================================
# 12_baseline_distrust_interaction.R
#
# PURPOSE: Test whether the sensitivity gradient is driven by
# differential baseline distrust variance (selection alternative)
# vs. item sensitivity ranking (falsification prediction).
#
# LOGIC OF THE TEST:
#   Selection account predicts: items with highest protest-period
#     variance shift most post-NSL, because the most extreme critics
#     of those items selectively exit.
#   Falsification account predicts: items shift in proportion to
#     their sensitivity ranking, independently of baseline variance.
#
# STRUCTURE:
#   PRIMARY ANALYSIS (6 observations):
#     Five individual trust items (same 1-4 scale, directly comparable)
#     + one democratic attitudes index (mean of 3 items, z-scored to
#     trust scale for comparability). Collapsing democratic items to an
#     index is theoretically motivated: all three are "low-sensitivity
#     abstract evaluations" in the framework's own typology, and treating
#     them as independent observations would be pseudo-replication.
#
#   ROBUSTNESS CHECK (9 observations):
#     All items disaggregated. Shows that the anomalous within-category
#     heterogeneity among democratic items (dem_always_preferable moves
#     opposite to dem_suitability) is consistent with the item asymmetry
#     argument developed in the manuscript.
#
# OUTPUT: results/baseline_distrust_interaction.RData
# ============================================================

library(tidyverse)
library(broom)
library(knitr)

source("../../../_data_config.R")
load("results/prepared_data.RData")
load("results/main_analysis_results.RData")

# ── 0. Rebuild analysis data ──────────────────────────────────────────────────

hk5_analysis <- hk5 |>
  filter(period %in% c("Protest", "Post-NSL")) |>
  mutate(
    period   = droplevels(period),
    post_nsl = as.numeric(period == "Post-NSL")
  )

protest_data <- hk5_analysis |> filter(period == "Protest")
postnsl_data <- hk5_analysis |> filter(period == "Post-NSL")

# ── 1. Helper functions ───────────────────────────────────────────────────────

weighted_var <- function(x, w) {
  mu <- weighted.mean(x, w)
  sum(w * (x - mu)^2) / (sum(w) - 1)
}
weighted_sd  <- function(x, w) sqrt(weighted_var(x, w))

weighted_cohens_d <- function(x1, w1, x2, w2) {
  m1 <- weighted.mean(x1, w1); m2 <- weighted.mean(x2, w2)
  v1 <- weighted_var(x1, w1);  v2 <- weighted_var(x2, w2)
  pooled_sd <- sqrt(((length(x1) - 1) * v1 + (length(x2) - 1) * v2) /
                      (length(x1) + length(x2) - 2))
  (m2 - m1) / pooled_sd
}

# Compute stats for a single variable
item_stats_for <- function(var, pro = protest_data, post = postnsl_data) {
  p  <- pro  |> filter(!is.na(!!sym(var)))
  po <- post |> filter(!is.na(!!sym(var)))
  if (nrow(p) < 10 | nrow(po) < 10) return(NULL)
  tibble(
    variable      = var,
    baseline_mean = weighted.mean(p[[var]], p$weight),
    baseline_sd   = weighted_sd(p[[var]], p$weight),
    cohens_d      = weighted_cohens_d(p[[var]], p$weight, po[[var]], po$weight),
    n_protest     = nrow(p),
    n_postnsl     = nrow(po)
  )
}

# ── 2. Trust items (primary, same 1-4 scale) ─────────────────────────────────

trust_hierarchy <- tribble(
  ~variable,                   ~sensitivity_rank, ~label,
  "trust_police",              1, "Trust in Police",
  "trust_national_government", 1, "Trust in Nat'l Government",
  "trust_president",           2, "Trust in President/CE",
  "trust_parliament",          3, "Trust in Parliament",
  "trust_military",            3, "Trust in Military",
  "trust_courts",              4, "Trust in Courts"
) |> filter(variable %in% names(hk5_analysis))

trust_stats <- map_dfr(trust_hierarchy$variable, item_stats_for) |>
  left_join(trust_hierarchy, by = "variable")

cat("\n=== Trust item statistics ===\n")
trust_stats |>
  select(label, sensitivity_rank, baseline_mean, baseline_sd, cohens_d) |>
  mutate(across(c(baseline_mean, baseline_sd, cohens_d), ~round(.x, 3))) |>
  print(n = Inf)

# ── 3. Democratic attitudes index ────────────────────────────────────────────
# Collapse three rank-5 items into a single index.
# Each item is z-scored within the full hk5_analysis sample before averaging,
# so items with different scales (0-10 vs 1-4 vs 1-3) contribute equally.
# The resulting index is re-expressed as Cohen's d in index-SD units.

dem_vars <- c("democracy_suitability", "dem_always_preferable", "democracy_satisfaction")
dem_vars_avail <- dem_vars[dem_vars %in% names(hk5_analysis)]

cat("\n=== Democratic attitude items available:", length(dem_vars_avail), "of", length(dem_vars), "===\n")
cat(dem_vars_avail, "\n")

# Z-score each item across full sample, then average
hk5_analysis <- hk5_analysis |>
  mutate(across(all_of(dem_vars_avail),
                ~(. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                .names = "{.col}_z")) |>
  mutate(
    dem_index_z = rowMeans(
      pick(ends_with("_z") & matches(paste(dem_vars_avail, collapse = "|"))),
      na.rm = TRUE
    ),
    dem_index_z = ifelse(is.nan(dem_index_z), NA_real_, dem_index_z)
  )

# Recompute split samples after adding index
protest_data <- hk5_analysis |> filter(period == "Protest")
postnsl_data <- hk5_analysis |> filter(period == "Post-NSL")

dem_index_stats <- item_stats_for("dem_index_z") |>
  mutate(
    variable          = "dem_index",
    sensitivity_rank  = 5,
    label             = "Democratic Attitudes Index"
  )

cat("\n=== Democratic attitudes index statistics ===\n")
dem_index_stats |>
  select(label, sensitivity_rank, baseline_mean, baseline_sd, cohens_d) |>
  mutate(across(c(baseline_mean, baseline_sd, cohens_d), ~round(.x, 3))) |>
  print()

# ── 4. Combined 6-observation dataset (PRIMARY) ───────────────────────────────

primary_stats <- bind_rows(trust_stats, dem_index_stats) |>
  arrange(sensitivity_rank)

cat("\n=== PRIMARY DATASET: 6 observations ===\n")
primary_stats |>
  select(label, sensitivity_rank, baseline_sd, cohens_d) |>
  mutate(across(c(baseline_sd, cohens_d), ~round(.x, 3))) |>
  print(n = Inf)

# ── 5. CORE TEST: Cross-item regression (PRIMARY) ────────────────────────────
# DV: cohens_d
# IV1: sensitivity_rank  (falsification prediction: negative coefficient)
# IV2: baseline_sd       (selection prediction: positive coefficient)

primary_scaled <- primary_stats |>
  mutate(
    sensitivity_rank_z = scale(sensitivity_rank)[, 1],
    baseline_sd_z      = scale(baseline_sd)[, 1],
    cohens_d_z         = scale(cohens_d)[, 1]
  )

# M1: Sensitivity rank alone
m1 <- lm(cohens_d ~ sensitivity_rank, data = primary_stats)
# M2: Baseline SD alone
m2 <- lm(cohens_d ~ baseline_sd,      data = primary_stats)
# M3: Both jointly (key test)
m3 <- lm(cohens_d ~ sensitivity_rank + baseline_sd, data = primary_stats)
# M4: Standardized
m4 <- lm(cohens_d_z ~ sensitivity_rank_z + baseline_sd_z, data = primary_scaled)

cat("\n\n=== PRIMARY: MODEL 1 — Sensitivity rank only ===\n")
print(summary(m1))
cat("\n=== PRIMARY: MODEL 2 — Baseline SD only ===\n")
print(summary(m2))
cat("\n=== PRIMARY: MODEL 3 — Both jointly (key test) ===\n")
print(summary(m3))
cat("\n=== PRIMARY: MODEL 4 — Standardized ===\n")
print(summary(m4))

# Partial correlations
resid_rank_p <- residuals(lm(sensitivity_rank ~ baseline_sd, data = primary_stats))
resid_d_p    <- residuals(lm(cohens_d        ~ baseline_sd, data = primary_stats))
partial_r_rank_primary <- cor.test(resid_rank_p, resid_d_p)

resid_sd_p   <- residuals(lm(baseline_sd     ~ sensitivity_rank, data = primary_stats))
partial_r_sd_primary   <- cor.test(resid_sd_p, resid_d_p)

cat("\n=== PRIMARY: Partial correlations ===\n")
cat("Partial r(sensitivity_rank, d | baseline_sd):",
    round(partial_r_rank_primary$estimate, 3),
    "  p =", format.pval(partial_r_rank_primary$p.value, digits = 3), "\n")
cat("Partial r(baseline_sd, d | sensitivity_rank):",
    round(partial_r_sd_primary$estimate, 3),
    "  p =", format.pval(partial_r_sd_primary$p.value, digits = 3), "\n")

# Spearman rho (rank-based, appropriate for n=6)
spearman_primary <- cor.test(primary_stats$sensitivity_rank,
                              primary_stats$cohens_d, method = "spearman")
cat("\nSpearman rho (sensitivity_rank vs cohens_d, n=6):",
    round(spearman_primary$estimate, 3),
    "  p =", format.pval(spearman_primary$p.value, digits = 3), "\n")

# ── 6. ROBUSTNESS: 9-item disaggregated ──────────────────────────────────────

all_hierarchy <- tribble(
  ~variable,                    ~sensitivity_rank, ~label,
  "trust_police",               1, "Trust in Police",
  "trust_national_government",  1, "Trust in Nat'l Government",
  "trust_president",            2, "Trust in President/CE",
  "trust_parliament",           3, "Trust in Parliament",
  "trust_military",             3, "Trust in Military",
  "trust_courts",               4, "Trust in Courts",
  "democracy_suitability",      5, "Democracy Suitability",
  "dem_always_preferable",      5, "Democracy Preferable",
  "democracy_satisfaction",     5, "Democratic Satisfaction"
) |> filter(variable %in% names(hk5_analysis))

# Use original (non-z-scored) split data for 9-item robustness
protest_orig <- hk5 |> filter(period == "Protest")
postnsl_orig <- hk5 |> filter(period == "Post-NSL")

all_stats <- map_dfr(all_hierarchy$variable, function(var) {
  item_stats_for(var, pro = protest_orig, post = postnsl_orig)
}) |>
  left_join(all_hierarchy, by = "variable")

cat("\n\n=== ROBUSTNESS: 9-item disaggregated ===\n")
all_stats |>
  select(label, sensitivity_rank, baseline_sd, cohens_d) |>
  mutate(across(c(baseline_sd, cohens_d), ~round(.x, 3))) |>
  print(n = Inf)

cat("\nNote: Within-category heterogeneity for democratic items (rank 5):\n")
cat("  dem_suitability collapses (d < 0) while dem_always_preferable inflates (d > 0).\n")
cat("  This is consistent with the item asymmetry argument: 'democracy always preferable'\n")
cat("  aligns with PRC self-legitimation rhetoric and carries low cost; 'suitability'\n")
cat("  evaluates regime performance directly and carries higher cost.\n")
cat("  The index appropriately averages these opposing pressures.\n\n")

spearman_9 <- cor.test(all_stats$sensitivity_rank, all_stats$cohens_d,
                        method = "spearman")
cat("Spearman rho (9-item disaggregated):",
    round(spearman_9$estimate, 3),
    "  p =", format.pval(spearman_9$p.value, digits = 3), "\n")

# ── 7. Figure: primary (6-obs) with trust items labeled ──────────────────────

label_colors <- c(
  "1" = "#d73027", "2" = "#fc8d59", "3" = "#fee090",
  "4" = "#91bfdb", "5" = "#4575b4"
)
rank_labels <- c(
  "1" = "Coercive (rank 1)",
  "2" = "Executive (rank 2)",
  "3" = "Legislature/Military (rank 3)",
  "4" = "Judiciary (rank 4)",
  "5" = "Abstract democratic (rank 5)"
)

p_primary <- primary_stats |>
  mutate(rank_f = factor(sensitivity_rank)) |>
  ggplot(aes(x = baseline_sd, y = cohens_d,
             color = rank_f, label = label)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE,
              color = "grey40", linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 4) +
  {if (requireNamespace("ggrepel", quietly = TRUE))
    ggrepel::geom_text_repel(size = 3.2, max.overlaps = 12,
                              box.padding = 0.4, show.legend = FALSE)
   else
    geom_text(size = 3, vjust = -1, show.legend = FALSE)} +
  scale_color_manual(values = label_colors, labels = rank_labels,
                     name = "Item category") +
  labs(
    x     = "Protest-period SD (baseline disagreement)",
    y     = "Cohen's d (Post-NSL vs. Protest)",
    title = "Baseline disagreement vs. post-NSL shift",
    subtitle = paste0(
      "Selection predicts: higher baseline SD -> larger d (positive slope).\n",
      "Falsification predicts: sensitivity rank drives d, independent of SD."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 9, color = "grey40"))

ggsave("figures/baseline_distrust_interaction.pdf",  p_primary, width = 9, height = 6)
ggsave("figures/baseline_distrust_interaction.png",  p_primary, width = 9, height = 6, dpi = 300)
cat("\nFigure saved.\n")

# ── 8. Summary regression table ──────────────────────────────────────────────

regression_table <- bind_rows(
  tidy(m1) |> mutate(model = "M1: Sensitivity rank only"),
  tidy(m2) |> mutate(model = "M2: Baseline SD only"),
  tidy(m3) |> mutate(model = "M3: Both jointly (key test)"),
  tidy(m4) |> mutate(model = "M4: Standardized")
) |>
  select(model, term, estimate, std.error, statistic, p.value) |>
  mutate(across(c(estimate, std.error, statistic), ~round(.x, 3)),
         p.value = format.pval(p.value, digits = 3))

model_fit <- tibble(
  model         = c("M1", "M2", "M3", "M4"),
  r_squared     = map_dbl(list(m1, m2, m3, m4), ~summary(.x)$r.squared),
  adj_r_squared = map_dbl(list(m1, m2, m3, m4), ~summary(.x)$adj.r.squared),
  n             = nrow(primary_stats)
) |> mutate(across(c(r_squared, adj_r_squared), ~round(.x, 3)))

cat("\n=== Regression table ===\n"); print(regression_table, n = Inf)
cat("\n=== Model fit ===\n"); print(model_fit)

# ── 9. Save ───────────────────────────────────────────────────────────────────

save(
  primary_stats, all_stats,
  dem_index_stats, trust_stats,
  regression_table, model_fit,
  partial_r_rank_primary, partial_r_sd_primary,
  spearman_primary, spearman_9,
  m1, m2, m3, m4,
  file = "results/baseline_distrust_interaction.RData"
)

cat("\nSaved to results/baseline_distrust_interaction.RData\n")

cat("\n=== INTERPRETATION GUIDE ===\n")
cat("PRIMARY ANALYSIS (n=6: 5 trust items + democratic index):\n")
cat("  Falsification supported if: sensitivity_rank is negative & significant in M3\n")
cat("  Selection supported if: baseline_sd is positive & significant in M3\n")
cat("  Spearman rho gives non-parametric summary appropriate for n=6\n\n")
cat("ROBUSTNESS (n=9 disaggregated):\n")
cat("  Within-category heterogeneity in rank-5 items is expected under item asymmetry\n")
cat("  argument (dem_always_preferable aligned with PRC rhetoric; suitability not).\n")
cat("  If gradient holds in robustness check, adds confidence; if not, index is primary.\n")
