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
## Data: WVS Wave 6 and Wave 7 (parquet format)
##   Place files at: data/raw/wvs_wave6.parquet, data/raw/wvs_wave7.parquet
##
## Output: results/wvs_sensitivity_gradient.RData
##         figures/wvs_sensitivity_gradient.pdf
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(arrow)
library(kableExtra)

# ─── 1. Load data ────────────────────────────────────────────────────────────

w6_path <- "../../../data/raw/wvs_wave6/wvs_wave6.parquet"
w7_path <- "../../../data/raw/wvs_wave7/wvs_wave7.parquet"

stopifnot("WVS W6 parquet not found" = file.exists(w6_path))
stopifnot("WVS W7 parquet not found" = file.exists(w7_path))

w6_raw <- read_parquet(w6_path) |> mutate(across(where(haven::is.labelled), haven::zap_labels))
w7_raw <- read_parquet(w7_path) |> mutate(across(where(haven::is.labelled), haven::zap_labels))

cat("WVS W6:", nrow(w6_raw), "obs;  WVS W7:", nrow(w7_raw), "obs\n")

# ─── 2. Variable mapping ────────────────────────────────────────────────────

# WVS variable crosswalk: Wave 6 (V-numbers) → Wave 7 (Q-numbers)
#
# DEMOCRACY / POLITICAL SYSTEM ITEMS (high sensitivity)
#   V130 → Q238 : Having a democratic political system (1=very good … 4=very bad)
#   V127 → Q235 : Having a strong leader (1-4)
#   V128 → Q236 : Having experts make decisions (1-4)
#   V129 → Q237 : Having the army rule (1-4)
#   V140 → Q250 : Importance of living in democracy (1-10)
#   V141 → Q251 : How democratically governed today (1-10)
#
# INSTITUTIONAL TRUST ITEMS (medium sensitivity)
#   V115 → Q71  : Confidence in government (1=great deal … 4=none at all)
#   V117 → Q64  : Confidence in parliament
#   V116 → Q72  : Confidence in political parties
#   V114 → Q70  : Confidence in courts
#   V113 → Q69  : Confidence in police
#   V109 → Q65  : Confidence in armed forces
#   V110 → Q66  : Confidence in press
#
# APOLITICAL CONTROL ITEMS (low sensitivity)
#   V10  → Q46  : Happiness (1-4)
#   V23  → Q49  : Life satisfaction (1-10)
#   V11  → Q47  : State of health (1-5)

item_map <- tribble(
  ~label,               ~w6_var, ~w7_var, ~sensitivity,  ~scale,
  "Democratic system",  "V130",  "Q238",  "Democracy",   "1-4",
  "Strong leader",      "V127",  "Q235",  "Democracy",   "1-4",
  "Experts decide",     "V128",  "Q236",  "Democracy",   "1-4",
  "Army rule",          "V129",  "Q237",  "Democracy",   "1-4",
  "Dem. importance",    "V140",  "Q250",  "Democracy",   "1-10",
  "Dem. evaluation",    "V141",  "Q251",  "Democracy",   "1-10",
  "Conf. government",   "V115",  "Q71",   "Trust",       "1-4",
  "Conf. parliament",   "V117",  "Q64",   "Trust",       "1-4",
  "Conf. pol. parties", "V116",  "Q72",   "Trust",       "1-4",
  "Conf. courts",       "V114",  "Q70",   "Trust",       "1-4",
  "Conf. police",       "V113",  "Q69",   "Trust",       "1-4",
  "Conf. armed forces", "V109",  "Q65",   "Trust",       "1-4",
  "Conf. press",        "V110",  "Q66",   "Trust",       "1-4",
  "Happiness",          "V10",   "Q46",   "Control",     "1-4",
  "Life satisfaction",  "V23",   "Q49",   "Control",     "1-10",
  "State of health",    "V11",   "Q47",   "Control",     "1-5"
)

# ─── 3. Extract Turkey and Russia ───────────────────────────────────────────

# ISO 3166-1 numeric: Turkey=792, Russia=643
countries <- c(Turkey = 792, Russia = 643)

# ─── 4. Compute non-response rates and means ────────────────────────────────

compute_stats <- function(w6_data, w7_data, country_name) {
  results <- item_map |>
    rowwise() |>
    mutate(
      country = country_name,
      # W6 stats (missing = NaN in parquet)
      w6_n = sum(!is.na(w6_data[[w6_var]]) & w6_data[[w6_var]] > 0, na.rm = TRUE),
      w6_nr = (sum(is.na(w6_data[[w6_var]]) | w6_data[[w6_var]] < 0, na.rm = TRUE) /
                 nrow(w6_data)) * 100,
      w6_mean = {
        x <- w6_data[[w6_var]]
        x[x < 0 | is.na(x)] <- NA
        mean(x, na.rm = TRUE)
      },
      # W7 stats (missing = negative codes)
      w7_n = sum(!is.na(w7_data[[w7_var]]) & w7_data[[w7_var]] > 0, na.rm = TRUE),
      w7_nr = (sum(is.na(w7_data[[w7_var]]) | w7_data[[w7_var]] < 0, na.rm = TRUE) /
                 nrow(w7_data)) * 100,
      w7_mean = {
        x <- w7_data[[w7_var]]
        x[x < 0 | is.na(x)] <- NA
        mean(x, na.rm = TRUE)
      },
      # Changes
      nr_change = w7_nr - w6_nr,
      mean_change = w7_mean - w6_mean
    ) |>
    ungroup()

  results
}

all_results <- pmap_dfr(
  list(
    w6_data = map(countries, ~ w6_raw |> filter(V2 == .x)),
    w7_data = map(countries, ~ w7_raw |> filter(B_COUNTRY == .x)),
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

# Figure: Mean shifts (W6→W7) for confidence items — trust paradox
trust_data <- all_results |>
  filter(sensitivity == "Trust") |>
  mutate(
    # For confidence items, lower = more trust (1=great deal, 4=none)
    # Negative mean_change = trust INCREASED
    trust_increased = mean_change < 0,
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
    x = "Mean change (W6 → W7)\n← Trust increased | Trust decreased →",
    y = NULL,
    title = "Trust paradox: Institutional confidence during autocratization",
    subtitle = "WVS Wave 6 → Wave 7 (1 = great deal of confidence … 4 = none at all)"
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
      "Note: WVS Wave 6 → Wave 7. Turkey: 2011→2018 (post-2016 coup attempt); ",
      "Russia: 2011→2017 (post-Crimea annexation).\n",
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

# ─── 8. Save results ────────────────────────────────────────────────────────

wvs_gradient_results <- all_results
wvs_gradient_summary <- gradient_summary

save(wvs_gradient_results, wvs_gradient_summary,
     file = "results/wvs_sensitivity_gradient.RData")

cat("\n✓ Results saved to results/wvs_sensitivity_gradient.RData\n")
cat("✓ Figures saved to figures/\n")

# ─── 9. Print key findings for manuscript ────────────────────────────────────

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
  cat(sprintf("  Gradient (Dem − Trust): %+.1fpp\n", dem_nr - trust_nr))

  # Trust changes (for confidence items, negative = trust increased)
  trust_change <- cty_data |>
    filter(sensitivity == "Trust") |>
    summarise(avg = mean(mean_change, na.rm = TRUE)) |>
    pull(avg)
  cat(sprintf("  Avg trust change: %+.2f (negative = trust increased)\n", trust_change))

  # Dem eval change
  dem_eval <- cty_data |> filter(label == "Dem. evaluation")
  cat(sprintf("  Dem. evaluation change: %+.2f\n", dem_eval$mean_change))
}
