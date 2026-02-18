## ──────────────────────────────────────────────────────────────────────────────
## 08 — WVS Cross-National Sensitivity Gradient: Turkey & Russia
##
## Purpose: Demonstrate that the sensitivity gradient methodology generalizes
## beyond the Hong Kong / ABS case. Using WVS Waves 6→7 for Turkey and Russia,
## both of which experienced clear autocratization between survey waves, we show:
##   (1) Democracy-related items exhibit higher non-response than trust items
##   (2) Trust "recovers" or increases despite democratic backsliding
##   (3) Item non-response is systematically higher for politically sensitive items
##
## Data: Harmonized WVS parquet from survey-data-prep pipeline.
##   Trust items already reversed (higher = more trust, 1-4).
##   Missing values coded as NA (not negative codes).
##
## Output: results/wvs_sensitivity_gradient.RData
##         figures/wvs_sensitivity_gradient.pdf
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(arrow)
library(kableExtra)

# ─── 1. Load data ────────────────────────────────────────────────────────────

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))
wvs_path <- wvs_harmonized_path

stopifnot("Harmonized WVS parquet not found" = file.exists(wvs_path))

wvs <- read_parquet(wvs_path)
cat("Harmonized WVS:", nrow(wvs), "obs,", n_distinct(wvs$country), "countries\n")

# ─── 2. Variable mapping ────────────────────────────────────────────────────

# Map harmonized column names to item labels and sensitivity categories.
# Trust items: 1-4, higher = more trust (already reversed in harmonization).
# Democracy items: native scales (1-4 or 1-10).
# Control items: native scales.

item_map <- tribble(
  ~label,               ~var,                              ~sensitivity,  ~scale,
  "Democratic system",  "dem_democratic_system",            "Democracy",   "1-4",
  "Strong leader",      "dem_strong_leader",               "Democracy",   "1-4",
  "Experts decide",     "dem_experts_rule",                 "Democracy",   "1-4",
  "Army rule",          "dem_army_rule",                    "Democracy",   "1-4",
  "Dem. importance",    "dem_importance_democracy",         "Democracy",   "1-10",
  "Dem. evaluation",    "dem_how_democratic",               "Democracy",   "1-10",
  "Conf. government",   "trust_government",                "Trust",       "1-4",
  "Conf. parliament",   "trust_parliament",                "Trust",       "1-4",
  "Conf. pol. parties", "trust_political_parties",          "Trust",       "1-4",
  "Conf. courts",       "trust_courts",                    "Trust",       "1-4",
  "Conf. police",       "trust_police",                    "Trust",       "1-4",
  "Conf. armed forces", "trust_armed_forces",              "Trust",       "1-4",
  "Conf. press",        "trust_press",                     "Trust",       "1-4",
  "Happiness",          "happiness",                       "Control",     "1-4",
  "Life satisfaction",  "life_satisfaction",               "Control",     "1-10"
)

# ─── 3. Extract Turkey and Russia ───────────────────────────────────────────

countries <- c(Turkey = "TUR", Russia = "RUS")

# ─── 4. Compute non-response rates and means ────────────────────────────────

compute_stats <- function(w6_data, w7_data, country_name) {
  results <- item_map |>
    rowwise() |>
    mutate(
      country = country_name,
      # W6 stats (missing coded as NA in harmonized data)
      w6_n = sum(!is.na(w6_data[[var]]), na.rm = TRUE),
      w6_nr = (sum(is.na(w6_data[[var]])) / nrow(w6_data)) * 100,
      w6_mean = mean(w6_data[[var]], na.rm = TRUE),
      # W7 stats
      w7_n = sum(!is.na(w7_data[[var]]), na.rm = TRUE),
      w7_nr = (sum(is.na(w7_data[[var]])) / nrow(w7_data)) * 100,
      w7_mean = mean(w7_data[[var]], na.rm = TRUE),
      # Changes
      nr_change = w7_nr - w6_nr,
      mean_change = w7_mean - w6_mean
    ) |>
    ungroup()

  results
}

all_results <- pmap_dfr(
  list(
    w6_data = map(countries, ~ wvs |> filter(country == .x, wave == 6)),
    w7_data = map(countries, ~ wvs |> filter(country == .x, wave == 7)),
    country_name = names(countries)
  ),
  compute_stats
)

# Add fieldwork years
all_results <- all_results |>
  mutate(
    w6_year = case_when(country == "Turkey" ~ 2011L, country == "Russia" ~ 2011L),
    w7_year = case_when(country == "Turkey" ~ 2018L, country == "Russia" ~ 2017L)
  )

# ─── 5. Summary table ───────────────────────────────────────────────────────

gradient_summary <- all_results |>
  group_by(country, sensitivity) |>
  summarise(
    avg_w6_nr = mean(w6_nr, na.rm = TRUE),
    avg_w7_nr = mean(w7_nr, na.rm = TRUE),
    avg_nr_change = mean(nr_change, na.rm = TRUE),
    avg_mean_change = mean(mean_change, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    sensitivity = factor(sensitivity, levels = c("Democracy", "Trust", "Control"))
  ) |>
  arrange(country, sensitivity)

cat("\n=== Sensitivity Gradient Summary ===\n")
print(gradient_summary)

# ─── 6. Figures ──────────────────────────────────────────────────────────────

# Figure: Item-level non-response rates in Wave 7, grouped by sensitivity
fig_data <- all_results |>
  filter(label != "Army rule") |>  # Exclude: Turkey didn't field this item

  mutate(
    sensitivity = factor(sensitivity, levels = c("Democracy", "Trust", "Control")),
    # Sort: by category (Democracy top, Control bottom), then by NR within category
    label = fct_reorder2(label, as.numeric(sensitivity), -w7_nr,
                         .fun = function(s, nr) s[1] * 1000 - mean(nr))
  )

p_nr <- ggplot(fig_data, aes(x = w7_nr, y = label, fill = sensitivity)) +
  geom_col(width = 0.7, alpha = 0.85) +
  facet_wrap(~country, scales = "free_x") +
  scale_fill_manual(
    values = c(Democracy = "#CC3311", Trust = "#0077BB", Control = "#009988"),
    name = "Item category"
  ) +
  labs(
    x = "Item non-response rate (%, Wave 7)",
    y = NULL,
    title = "Sensitivity gradient in autocratizing contexts",
    subtitle = "WVS Wave 7: Democracy items show systematically higher non-response"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/wvs_sensitivity_gradient_nr.pdf", p_nr, width = 10, height = 7)
ggsave("figures/wvs_sensitivity_gradient_nr.png", p_nr, width = 10, height = 7, dpi = 300)

# Figure: Mean shifts (W6→W7) for trust items
# In harmonized data, higher = more trust, so positive mean_change = trust INCREASED
trust_data <- all_results |>
  filter(sensitivity == "Trust") |>
  mutate(
    trust_increased = mean_change > 0,
    label = fct_reorder(label, mean_change)
  )

p_trust <- ggplot(trust_data, aes(x = mean_change, y = label, fill = trust_increased)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~country) +
  scale_fill_manual(
    values = c(`TRUE` = "#228833", `FALSE` = "#EE6677"),
    labels = c(`TRUE` = "Trust increased", `FALSE` = "Trust decreased"),
    name = NULL
  ) +
  labs(
    x = "Mean change (W6 \u2192 W7)\n\u2190 Trust decreased | Trust increased \u2192",
    y = NULL,
    title = "Trust paradox: Institutional confidence during autocratization",
    subtitle = "WVS Wave 6 \u2192 Wave 7 (1-4, higher = more trust)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/wvs_trust_paradox.pdf", p_trust, width = 10, height = 6)
ggsave("figures/wvs_trust_paradox.png", p_trust, width = 10, height = 6, dpi = 300)

# ─── 7. Combined gradient figure (for appendix) ─────────────────────────────

# Dual-panel: NR gradient + mean shift, side by side
combined_data <- all_results |>
  filter(label != "Army rule") |>
  mutate(
    sensitivity = factor(sensitivity, levels = c("Democracy", "Trust", "Control")),
    label_ordered = fct_reorder2(label, as.numeric(sensitivity), -w7_nr,
                                 .fun = function(s, nr) s[1] * 1000 - mean(nr))
  )

p_combined <- combined_data |>
  select(country, label, label_ordered, sensitivity, w7_nr) |>
  ggplot(aes(x = w7_nr, y = label_ordered, fill = sensitivity)) +
  geom_col(width = 0.7, alpha = 0.85) +
  facet_wrap(~country, scales = "free_x") +
  scale_fill_manual(
    values = c(Democracy = "#CC3311", Trust = "#0077BB", Control = "#009988"),
    name = "Item category"
  ) +
  labs(
    x = "Item non-response rate (%, Wave 7)",
    y = NULL,
    caption = paste0(
      "Note: WVS Wave 6 \u2192 Wave 7. Turkey: 2011\u21922018 (post-2016 coup attempt); ",
      "Russia: 2011\u21922017 (post-Crimea annexation).\n",
      "'Army rule' item excluded (Turkey: not fielded in Wave 7). ",
      "Items ordered by mean non-response rate across both countries."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, hjust = 0)
  )

ggsave("figures/wvs_sensitivity_gradient_combined.pdf", p_combined, width = 10, height = 7)
ggsave("figures/wvs_sensitivity_gradient_combined.png", p_combined, width = 10, height = 7, dpi = 300)

# ─── 8. Statistical tests ────────────────────────────────────────────────────

# For each country, test whether NR rates differ across sensitivity categories.
# Two approaches:
#   (a) Item-level: Kruskal-Wallis on W7 NR rates across 3 categories
#       (unit of analysis = item, N is small so this is illustrative)
#   (b) Respondent-level: For each respondent, compute proportion of items
#       answered within each category; test whether category predicts response.
#       More powerful because N = respondents.

library(broom)

# --- (a) Item-level Kruskal-Wallis ---
item_level_tests <- all_results |>
  filter(label != "Army rule") |>
  group_by(country) |>
  summarise(
    kw_stat = kruskal.test(w7_nr ~ factor(sensitivity))$statistic,
    kw_df = kruskal.test(w7_nr ~ factor(sensitivity))$parameter,
    kw_p = kruskal.test(w7_nr ~ factor(sensitivity))$p.value,
    .groups = "drop"
  )

cat("\n=== Item-level Kruskal-Wallis tests (W7 NR ~ sensitivity category) ===\n")
print(item_level_tests)

# --- (b) Respondent-level chi-squared tests ---
# For each item, construct a 2x3 contingency: (responded / didn't) x (Dem / Trust / Control)
# Pool across items within category to get aggregate response counts.

respondent_level_tests <- list()

for (cty_iso in c("TUR", "RUS")) {
  cty_name <- if (cty_iso == "TUR") "Turkey" else "Russia"

  w7c <- wvs |> filter(country == cty_iso, wave == 7)
  n_resp <- nrow(w7c)

  item_subset <- item_map |> filter(label != "Army rule")

  # Count total responses and non-responses per category
  cat_counts <- item_subset |>
    rowwise() |>
    mutate(
      n_responded = sum(!is.na(w7c[[var]])),
      n_missing = n_resp - n_responded
    ) |>
    ungroup() |>
    group_by(sensitivity) |>
    summarise(
      total_responded = sum(n_responded),
      total_missing = sum(n_missing),
      n_items = n(),
      .groups = "drop"
    )

  # Chi-squared test on the 3x2 contingency table
  cont_table <- matrix(
    c(cat_counts$total_responded, cat_counts$total_missing),
    nrow = 3, ncol = 2,
    dimnames = list(cat_counts$sensitivity, c("Responded", "Missing"))
  )

  chi_test <- chisq.test(cont_table)

  # Pairwise: Democracy vs Trust, Democracy vs Control, Trust vs Control
  pairs <- list(
    c("Democracy", "Trust"),
    c("Democracy", "Control"),
    c("Trust", "Control")
  )

  pairwise_results <- map_dfr(pairs, function(pair) {
    idx <- cat_counts$sensitivity %in% pair
    sub_table <- cont_table[idx, ]
    test <- chisq.test(sub_table)
    tibble(
      comparison = paste(pair, collapse = " vs. "),
      chi_sq = test$statistic,
      df = test$parameter,
      p_value = test$p.value
    )
  })

  respondent_level_tests[[cty_name]] <- list(
    omnibus = tibble(
      country = cty_name,
      chi_sq = chi_test$statistic,
      df = chi_test$parameter,
      p_value = chi_test$p.value
    ),
    pairwise = pairwise_results |> mutate(country = cty_name, .before = 1),
    contingency = cont_table
  )

  cat(sprintf("\n=== %s: Respondent-level chi-squared (pooled item-responses) ===\n", cty_name))
  cat("Contingency table (items pooled within category):\n")
  print(cont_table)
  cat(sprintf("Omnibus chi-squared: %.1f, df=%d, p < %.1e\n",
              chi_test$statistic, chi_test$parameter, chi_test$p.value))
  cat("Pairwise comparisons:\n")
  print(pairwise_results)
}

# --- (c) Effect sizes: odds ratios for Democracy vs Trust NR ---
or_results <- map_dfr(c("Turkey", "Russia"), function(cty_name) {
  ct <- respondent_level_tests[[cty_name]]$contingency
  # Democracy vs Trust: 2x2 sub-table
  dem_row <- which(rownames(ct) == "Democracy")
  trust_row <- which(rownames(ct) == "Trust")
  sub <- ct[c(dem_row, trust_row), ]
  # OR = (Dem_missing / Dem_responded) / (Trust_missing / Trust_responded)
  or_val <- (sub[1, 2] / sub[1, 1]) / (sub[2, 2] / sub[2, 1])
  log_or_se <- sqrt(1/sub[1,1] + 1/sub[1,2] + 1/sub[2,1] + 1/sub[2,2])
  or_ci_lo <- exp(log(or_val) - 1.96 * log_or_se)
  or_ci_hi <- exp(log(or_val) + 1.96 * log_or_se)
  tibble(
    country = cty_name,
    odds_ratio = or_val,
    or_ci_lo = or_ci_lo,
    or_ci_hi = or_ci_hi
  )
})

cat("\n=== Odds ratios: Democracy vs Trust non-response ===\n")
print(or_results)

# ─── 9. Save results ────────────────────────────────────────────────────────

wvs_gradient_results <- all_results
wvs_gradient_summary <- gradient_summary
wvs_gradient_item_tests <- item_level_tests
wvs_gradient_respondent_tests <- respondent_level_tests
wvs_gradient_odds_ratios <- or_results

save(wvs_gradient_results, wvs_gradient_summary,
     wvs_gradient_item_tests, wvs_gradient_respondent_tests,
     wvs_gradient_odds_ratios,
     file = "results/wvs_sensitivity_gradient.RData")

cat("\n\u2713 Results saved to results/wvs_sensitivity_gradient.RData\n")
cat("\u2713 Figures saved to figures/\n")

# ─── 10. Print key findings for manuscript ───────────────────────────────────

cat("\n=== KEY FINDINGS FOR MANUSCRIPT ===\n")

for (cty in c("Turkey", "Russia")) {
  cat(sprintf("\n--- %s ---\n", cty))
  cty_data <- all_results |> filter(country == cty)

  dem_nr <- cty_data |> filter(sensitivity == "Democracy", label != "Army rule") |>
    pull(w7_nr) |> mean()
  trust_nr <- cty_data |> filter(sensitivity == "Trust") |> pull(w7_nr) |> mean()
  ctrl_nr <- cty_data |> filter(sensitivity == "Control") |> pull(w7_nr) |> mean()

  cat(sprintf("  W7 avg NR: Democracy=%.1f%%, Trust=%.1f%%, Control=%.1f%%\n",
              dem_nr, trust_nr, ctrl_nr))
  cat(sprintf("  Gradient (Dem \u2212 Trust): %+.1fpp\n", dem_nr - trust_nr))

  # Trust changes (in harmonized data, positive = trust increased)
  trust_change <- cty_data |>
    filter(sensitivity == "Trust") |>
    summarise(avg = mean(mean_change, na.rm = TRUE)) |>
    pull(avg)
  cat(sprintf("  Avg trust change: %+.2f (positive = trust increased)\n", trust_change))

  # Dem eval change
  dem_eval <- cty_data |> filter(label == "Dem. evaluation")
  cat(sprintf("  Dem. evaluation change: %+.2f\n", dem_eval$mean_change))
}
