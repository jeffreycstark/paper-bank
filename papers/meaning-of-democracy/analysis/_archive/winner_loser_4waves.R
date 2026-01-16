# Winner/Loser × Procedural Orientation: W2, W3, W4, W6 (2005-2022)
# FOUR WAVES - 17 YEARS OF DATA!
library(haven)
library(tidyverse)
library(broom)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"

cat("=============================================================\n")
cat("WINNER/LOSER × PROCEDURAL ORIENTATION: 2005-2022 (4 WAVES)\n")
cat("=============================================================\n\n")

# Load data
w2 <- readRDS(file.path(data_dir, "w2.rds"))
w3 <- readRDS(file.path(data_dir, "w3.rds"))
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

# Helper functions
get_country_name <- function(x) {
  if (is.character(x)) {
    case_when(x == "Korea" ~ "South Korea", TRUE ~ x)
  } else {
    code <- as.integer(x)
    case_when(
      code == 1 ~ "Japan", code == 2 ~ "South Korea", code == 3 ~ "Mongolia",
      code == 4 ~ "Taiwan", code == 5 ~ "Hong Kong", code == 6 ~ "China",
      code == 7 ~ "Philippines", code == 8 ~ "Thailand", code == 9 ~ "Vietnam",
      code == 10 ~ "Cambodia", code == 11 ~ "Singapore", code == 12 ~ "Myanmar",
      code == 13 ~ "Malaysia", code == 14 ~ "Indonesia", TRUE ~ NA_character_
    )
  }
}

# For W3/W4/W6: 4-set battery recoding
recode_set1 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set2 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)
recode_set3 <- function(x) case_when(x %in% c(2, 4) ~ 1L, x %in% c(1, 3) ~ 0L, TRUE ~ NA_integer_)
recode_set4 <- function(x) case_when(x %in% c(1, 3) ~ 1L, x %in% c(2, 4) ~ 0L, TRUE ~ NA_integer_)

# ============================================================================
# Process W2 (2005-2008)
# Uses q39a for winner/loser, q92 for single-item procedural/substantive
# q92: 1=elections, 2=freedom to criticize (procedural)
#      3=income gap, 4=basic necessities (substantive)
# ============================================================================
cat("Processing W2 (2005-08)...\n")

w2_full <- w2 %>%
  mutate(
    wave = "W2",
    wave_year = 2006,
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q39a) == 1 ~ "Winner",
      as.integer(q39a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    ),
    # Single item: 1,2 = procedural; 3,4 = substantive
    procedural_single = case_when(
      as.integer(q92) %in% c(1, 2) ~ 1L,  # procedural
      as.integer(q92) %in% c(3, 4) ~ 0L,  # substantive
      TRUE ~ NA_integer_
    ),
    # For comparability, scale to 0-4 range (0 or 4)
    # Or we can use proportion: procedural_single already is 0/1
    procedural_index = procedural_single * 4,  # Scale: 0 = substantive, 4 = procedural
    measure_type = "single_item"
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_single)) %>%
  select(wave, wave_year, country_name, winner_loser, procedural_single, procedural_index, measure_type)

cat("  W2:", nrow(w2_full), "observations\n")
cat("  W2 countries:", paste(unique(w2_full$country_name), collapse = ", "), "\n")

# ============================================================================
# Process W3 (2010-2012)
# ============================================================================
cat("\nProcessing W3 (2010-12)...\n")

w3_full <- w3 %>%
  mutate(
    wave = "W3",
    wave_year = 2010,
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q33a) == 1 ~ "Winner",
      as.integer(q33a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    procedural_single = if_else(procedural_index >= 2, 1L, 0L),  # For binary comparison
    measure_type = "four_set"
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index)) %>%
  select(wave, wave_year, country_name, winner_loser, procedural_single, procedural_index, measure_type)

cat("  W3:", nrow(w3_full), "observations\n")

# ============================================================================
# Process W4 (2014-2016)
# ============================================================================
cat("\nProcessing W4 (2014-16)...\n")

w4_full <- w4 %>%
  mutate(
    wave = "W4",
    wave_year = 2014,
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q88)),
    set2_proc = recode_set2(as.integer(q89)),
    set3_proc = recode_set3(as.integer(q90)),
    set4_proc = recode_set4(as.integer(q91)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    procedural_single = if_else(procedural_index >= 2, 1L, 0L),
    measure_type = "four_set"
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index)) %>%
  select(wave, wave_year, country_name, winner_loser, procedural_single, procedural_index, measure_type)

cat("  W4:", nrow(w4_full), "observations\n")

# ============================================================================
# Process W6 (2019-2022)
# ============================================================================
cat("\nProcessing W6 (2019-22)...\n")

w6_full <- w6 %>%
  mutate(
    wave = "W6",
    wave_year = 2020,
    country_name = get_country_name(country),
    winner_loser = case_when(
      as.integer(q34a) == 1 ~ "Winner",
      as.integer(q34a) == 2 ~ "Loser",
      TRUE ~ NA_character_
    ),
    set1_proc = recode_set1(as.integer(q85)),
    set2_proc = recode_set2(as.integer(q86)),
    set3_proc = recode_set3(as.integer(q87)),
    set4_proc = recode_set4(as.integer(q88)),
    procedural_index = set1_proc + set2_proc + set3_proc + set4_proc,
    procedural_single = if_else(procedural_index >= 2, 1L, 0L),
    measure_type = "four_set"
  ) %>%
  filter(!is.na(winner_loser), !is.na(procedural_index)) %>%
  select(wave, wave_year, country_name, winner_loser, procedural_single, procedural_index, measure_type)

cat("  W6:", nrow(w6_full), "observations\n")

# ============================================================================
# Combine all four waves
# ============================================================================
combined <- bind_rows(w2_full, w3_full, w4_full, w6_full)
cat("\n*** COMBINED: ", nrow(combined), "observations across 4 waves (2005-2022) ***\n")

# ============================================================================
cat("\n\n=============================================================\n")
cat("OVERALL LOSER EFFECT BY WAVE\n")
cat("=============================================================\n\n")

# Using binary procedural measure for cross-wave comparability
by_wave_binary <- combined %>%
  group_by(wave, wave_year, winner_loser) %>%
  summarise(
    n = n(),
    pct_procedural = mean(procedural_single) * 100,
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = winner_loser, values_from = c(n, pct_procedural)) %>%
  mutate(
    loser_effect = round(pct_procedural_Loser - pct_procedural_Winner, 1),
    total_n = n_Winner + n_Loser
  ) %>%
  arrange(wave_year)

cat("Binary measure (% choosing procedural):\n")
print(by_wave_binary)

# Also show the 0-4 index for W3/W4/W6
cat("\n\nUsing 0-4 index (W3/W4/W6 only):\n")
by_wave_index <- combined %>%
  filter(measure_type == "four_set") %>%
  group_by(wave, wave_year, winner_loser) %>%
  summarise(
    n = n(),
    mean_proc = mean(procedural_index),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = winner_loser, values_from = c(n, mean_proc)) %>%
  mutate(loser_effect = round(mean_proc_Loser - mean_proc_Winner, 3))

print(by_wave_index)

# Statistical tests
cat("\n\nStatistical tests (binary measure):\n")
for (w in c("W2", "W3", "W4", "W6")) {
  wave_data <- combined %>% filter(wave == w)
  prop_test <- prop.test(
    x = c(sum(wave_data$procedural_single[wave_data$winner_loser == "Loser"]),
          sum(wave_data$procedural_single[wave_data$winner_loser == "Winner"])),
    n = c(sum(wave_data$winner_loser == "Loser"),
          sum(wave_data$winner_loser == "Winner"))
  )
  loser_pct <- mean(wave_data$procedural_single[wave_data$winner_loser == "Loser"]) * 100
  winner_pct <- mean(wave_data$procedural_single[wave_data$winner_loser == "Winner"]) * 100
  cat(w, ": Loser", round(loser_pct, 1), "% vs Winner", round(winner_pct, 1), 
      "%, diff =", round(loser_pct - winner_pct, 1), "pp, p =", 
      format(prop_test$p.value, digits = 3), "\n")
}

# ============================================================================
cat("\n\n=============================================================\n")
cat("LOSER EFFECT BY COUNTRY ACROSS ALL WAVES\n")
cat("=============================================================\n\n")

by_country_wave <- combined %>%
  group_by(country_name, wave, wave_year) %>%
  filter(n() >= 50) %>%
  summarise(
    n = n(),
    pct_winner = round(sum(winner_loser == "Winner") / n() * 100, 1),
    pct_proc_winner = mean(procedural_single[winner_loser == "Winner"]) * 100,
    pct_proc_loser = mean(procedural_single[winner_loser == "Loser"]) * 100,
    loser_effect = round(pct_proc_loser - pct_proc_winner, 1),
    .groups = "drop"
  )

# Countries with data in multiple waves
country_coverage <- by_country_wave %>%
  count(country_name, name = "n_waves") %>%
  arrange(desc(n_waves))

cat("Country coverage across waves:\n")
print(country_coverage)

# ============================================================================
cat("\n\n=============================================================\n")
cat("THAILAND: THE FULL 17-YEAR TRAJECTORY\n")
cat("=============================================================\n\n")

thailand <- by_country_wave %>% 
  filter(country_name == "Thailand") %>%
  arrange(wave_year)

cat("Thailand trajectory:\n")
print(thailand)

cat("\nContext:\n")
cat("  W2 (2006): Post-2006 coup, interim government\n")
cat("  W3 (2010): Democrat government (Abhisit)\n")
cat("  W4 (2014): COUP (May 2014), military government\n")
cat("  W6 (2020): Military-backed civilian government\n")

# ============================================================================
cat("\n\n=============================================================\n")
cat("COUNTRIES WITH 3+ WAVES: TRAJECTORY ANALYSIS\n")
cat("=============================================================\n\n")

countries_3plus <- country_coverage %>% 
  filter(n_waves >= 3) %>% 
  pull(country_name)

cat("Countries with 3+ waves:", paste(countries_3plus, collapse = ", "), "\n\n")

trajectory <- by_country_wave %>%
  filter(country_name %in% countries_3plus) %>%
  select(country_name, wave, loser_effect) %>%
  pivot_wider(names_from = wave, values_from = loser_effect)

# Calculate changes
trajectory <- trajectory %>%
  mutate(
    earliest = coalesce(W2, W3),
    latest = coalesce(W6, W4, W3),
    total_change = latest - earliest
  ) %>%
  arrange(desc(total_change))

cat("Trajectory of loser effect (percentage points):\n")
print(trajectory)

# ============================================================================
cat("\n\n=============================================================\n")
cat("FULL PANEL: ALL COUNTRY × WAVE COMBINATIONS\n")
cat("=============================================================\n\n")

by_country_wave %>%
  select(country_name, wave, n, pct_winner, loser_effect) %>%
  arrange(country_name, wave) %>%
  print(n = 60)

# ============================================================================
cat("\n\n=============================================================\n")
cat("POOLED REGRESSION: ALL 4 WAVES\n")
cat("=============================================================\n\n")

combined$loser <- if_else(combined$winner_loser == "Loser", 1L, 0L)

# Model with country and wave FE
cat("Logistic regression: procedural ~ loser + country + wave\n")
cat("(Using binary procedural measure for cross-wave comparability)\n\n")

m1 <- glm(procedural_single ~ loser + country_name + wave, 
          data = combined, family = binomial)

loser_coef <- tidy(m1) %>% filter(term == "loser")
cat("Loser coefficient (log-odds):", round(loser_coef$estimate, 3), "\n")
cat("Loser odds ratio:", round(exp(loser_coef$estimate), 3), "\n")
cat("SE:", round(loser_coef$std.error, 3), "\n")
cat("p-value:", format(loser_coef$p.value, digits = 3), "\n")

# ============================================================================
cat("\n\n=============================================================\n")
cat("SUMMARY STATISTICS\n")
cat("=============================================================\n\n")

cat("Sample sizes by wave:\n")
combined %>%
  count(wave, winner_loser) %>%
  pivot_wider(names_from = winner_loser, values_from = n) %>%
  mutate(total = Loser + Winner) %>%
  print()

cat("\nTotal observations:", nrow(combined), "\n")
cat("Total countries:", n_distinct(combined$country_name), "\n")
cat("Time span: 2005-2022 (17 years)\n")

cat("\nCountries by wave:\n")
combined %>%
  count(country_name, wave) %>%
  pivot_wider(names_from = wave, values_from = n, values_fill = 0) %>%
  mutate(total = W2 + W3 + W4 + W6) %>%
  arrange(desc(total)) %>%
  print(n = 20)

# ============================================================================
# Save results
# ============================================================================
cat("\n\n--- Saving results ---\n")
output_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/papers/meaning-of-democracy/analysis"

saveRDS(combined, file.path(output_dir, "winner_loser_4waves.rds"))
write_csv(by_country_wave, file.path(output_dir, "loser_effect_by_country_4waves.csv"))
write_csv(trajectory, file.path(output_dir, "loser_effect_trajectory_4waves.csv"))

cat("Saved to:", output_dir, "\n")
cat("\n=== DONE ===\n")
