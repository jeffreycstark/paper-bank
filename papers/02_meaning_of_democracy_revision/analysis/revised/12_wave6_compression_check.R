# =============================================================================
# Script 12: Wave 6 Compression Check (Revision Package Section 8)
# Goal: Did governance/state-capacity items rise across-the-board in Wave 6,
#       potentially explaining the cross-country compression of the gap?
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

here::i_am("papers/02_meaning_of_democracy_revision/manuscript/md-manuscript.qmd")
results_path <- here("papers", "02_meaning_of_democracy_revision", "analysis", "revised", "results")

# --- 1. Load data -------------------------------------------------------
d <- readRDS(here("papers", "02_meaning_of_democracy_revision", "analysis", "data", "w346_main.rds"))

# --- 2. Compute item proportions by wave --------------------------------
# Choices are factors with item-label levels; use as.character() to get label,
# then join by label string.

wave_props <- function(data, choice_col, valid_col, set_name) {
  data %>%
    filter(!!sym(valid_col) == TRUE) %>%
    mutate(item = as.character(!!sym(choice_col))) %>%
    filter(!is.na(item)) %>%
    count(wave, wave_label, item) %>%
    group_by(wave, wave_label) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    mutate(set = set_name)
}

props <- bind_rows(
  wave_props(d, "set1_choice", "set1_valid", "Set1"),
  wave_props(d, "set2_choice", "set2_valid", "Set2"),
  wave_props(d, "set3_choice", "set3_valid", "Set3"),
  wave_props(d, "set4_choice", "set4_valid", "Set4")
)

# Item type lookup by item label
item_labels <- tibble(
  item = c(
    "Free elections","No waste","Free expression","Reduce gap rich/poor",
    "Legislature oversight","Quality services","Organize groups","Basic necessities",
    "Media freedom","Law and order","Party competition","Jobs for all",
    "Court protection","Clean politics","Protest freedom","Unemployment aid"
  ),
  item_type = c(
    "procedural","governance","procedural","substantive",
    "procedural","governance","procedural","substantive",
    "procedural","governance","procedural","substantive",
    "procedural","governance","procedural","substantive"
  )
)

props_labeled <- props %>%
  left_join(item_labels, by = "item")

# --- 3. Focus on governance/capacity items: W4 vs W6 ------------------
gov_items <- props_labeled %>%
  filter(item_type == "governance") %>%
  filter(wave %in% c(4L, 6L)) %>%
  select(wave_label, item, prop) %>%
  pivot_wider(names_from = wave_label, values_from = prop) %>%
  mutate(
    diff_w4_to_w6 = W6 - W4,
    pct_w4 = round(W4 * 100, 1),
    pct_w6 = round(W6 * 100, 1),
    diff_pp = round(diff_w4_to_w6 * 100, 1)
  )

cat("=== Governance Item Proportions: Wave 4 vs Wave 6 (all respondents) ===\n")
print(gov_items %>% select(item, pct_w4, pct_w6, diff_pp))

# --- 4. Compare all item types W4 vs W6 --------------------------------
all_items_w46 <- props_labeled %>%
  filter(wave %in% c(4, 6)) %>%
  group_by(item_type, wave_label) %>%
  summarise(mean_prop = mean(prop, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = wave_label, values_from = mean_prop) %>%
  mutate(diff_pp = round((W6 - W4) * 100, 1))

cat("\n=== Mean Item Type Proportions: W4 vs W6 ===\n")
print(all_items_w46)

# --- 5. Decision ---
# If governance items rose meaningfully (>2pp), pandemic salience story supported
# If near zero, drop speculation
gov_mean_diff <- mean(gov_items$diff_pp, na.rm = TRUE)
cat(sprintf("\nMean governance item change (W4→W6): %.2f pp\n", gov_mean_diff))

if (gov_mean_diff > 2) {
  cat("Verdict: Governance items ROSE → pandemic salience story has some support.\n")
} else if (gov_mean_diff < -2) {
  cat("Verdict: Governance items FELL → pandemic salience story is NOT supported (opposite direction). Remove speculation.\n")
} else {
  cat("Verdict: Governance items largely stable → remove pandemic speculation, note compression as unexplained.\n")
}

# --- 6. Country-level governance proportions (to check if it's uniform) ------
gov_by_country <- props_labeled %>%
  filter(item_type == "governance", wave %in% c(4, 6)) %>%
  group_by(wave_label, item) %>%
  # These are already pooled proportions by wave; re-compute at country level
  summarise(mean_prop = mean(prop), .groups = "drop")

# Actually let's do it properly at country-wave level
country_gov <- d %>%
  mutate(
    gov_set1 = as.integer(as.character(set1_choice) == "No waste" & set1_valid == TRUE),
    gov_set2 = as.integer(as.character(set2_choice) == "Quality services" & set2_valid == TRUE),
    gov_set3 = as.integer(as.character(set3_choice) == "Law and order" & set3_valid == TRUE),
    gov_set4 = as.integer(as.character(set4_choice) == "Clean politics" & set4_valid == TRUE)
  ) %>%
  filter(wave %in% c(4, 6)) %>%
  group_by(wave, wave_label, country_name) %>%
  summarise(
    gov_prop = mean(c(gov_set1, gov_set2, gov_set3, gov_set4), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = wave_label, values_from = gov_prop) %>%
  mutate(diff_pp = round((W6 - W4) * 100, 1)) %>%
  arrange(diff_pp)

cat("\n=== Country-level governance preference change (W4→W6) ===\n")
print(country_gov)

# --- 7. Save -----------------------------------------------------------
wave6_check <- list(
  gov_items      = gov_items,
  all_items_w46  = all_items_w46,
  gov_mean_diff  = gov_mean_diff,
  country_gov    = country_gov,
  verdict        = if (abs(gov_mean_diff) > 2) "supported" else "not_supported"
)
saveRDS(wave6_check, file.path(results_path, "wave6_compression_check.rds"))
write_csv(gov_items, file.path(results_path, "wave6_gov_items.csv"))

cat("\nDone.\n")
